open Format
open X86_64
open Ast

let debug = ref false

(* New label generation *)
let label_counter = ref 0

let new_label () =
  let label = Printf.sprintf "L%d" !label_counter in
  incr label_counter;
  label

let string_constants = ref []

(* Define a stack to keep track of symbol tables for each scope *)
let var_table_stack : (string, int) Hashtbl.t Stack.t  = Stack.create ()
let () = Stack.push (Hashtbl.create 100) var_table_stack

(* Function to record variable name, type, and stack offset *)
let record_var_name var_name var_ofs =
  let current_var_table = Stack.top var_table_stack in
  Hashtbl.replace current_var_table var_name var_ofs

let stack_offset_stack = Stack.create ()
let () = Stack.push 0 stack_offset_stack

(* Define a variable to keep track of the current stack offset *)
let current_stack_offset = ref 0

(* Function to enter a new scope *)
let enter_scope () =
  let new_var_table = Hashtbl.create 100 in
  Stack.push new_var_table var_table_stack;
  Stack.push !current_stack_offset stack_offset_stack;
  current_stack_offset := 0

(* Function to exit the current scope *)
let exit_scope () =
  ignore (Stack.pop var_table_stack);
  current_stack_offset := Stack.pop stack_offset_stack

(* Function to calculate the stack offset for a variable *)
let calculate_stack_offset var =
  let current_var_table = Stack.top var_table_stack in
  match Hashtbl.find_opt current_var_table var.v_name with
  | Some (existing_offset) -> (existing_offset, existing_offset - 8)
  | None ->
      (* Assuming each variable occupies 8 bytes on the stack for data and 8 bytes for type *)
      let var_size = 8 in
      let type_size = 8 in
      (* Update the current stack offset *)
      current_stack_offset := !current_stack_offset - (var_size + type_size);
      let stack_offset = !current_stack_offset + type_size in
      let type_offset = !current_stack_offset in
      (stack_offset, type_offset)

let current_fn_name = ref "main"

let rec compile_expr = function
  | TEcst c -> 
      (match c with
       | Cnone -> 
          movq (imm 0) !%rax++
          movq (imm 4) !%r10
       | Cbool true -> 
          movq (imm 1) !%rax++
          movq (imm 2) !%r10
       | Cbool false -> 
          movq (imm 0) !%rax++
          movq (imm 2) !%r10
       | Cint i -> 
          movq (imm64 i) !%rax ++
          movq (imm 1) !%r10
       | Cstring s ->
          (let lbl = new_label () in
          string_constants := (lbl, s) :: !string_constants;
          leaq (lab lbl) rax)++
          movq (imm 0) !%r10)
  | TEvar v ->
      let current_var_table = Stack.top var_table_stack in
      (match Hashtbl.find_opt current_var_table v.v_name with
       | Some (stack_offset) ->
           (* Generate code to load the variable's value from the stack *)
           movq (ind ~ofs:stack_offset rbp) !%rax ++
           movq (ind ~ofs:(stack_offset - 8) rbp) !%r10
       | None ->
           failwith ("Variable " ^ v.v_name ^ " not found in symbol table"))
  | TEbinop (op, lhs, rhs) ->
      (match lhs, rhs with
       | TEcst (Cstring s1), TEcst (Cstring s2) ->
          (match op with
            | Badd ->
                let lbl1 = new_label () in
                let lbl2 = new_label () in
                string_constants := (lbl1, s1) :: (lbl2, s2) :: !string_constants;
                leaq (lab lbl2) rsi ++
                leaq (lab lbl1) rdi ++
                movq (imm 0) !%rax ++
                call "strcat" ++
                movq (imm 0) !%r10
            | Beq -> movq (imm64 (if s1 = s2 then Int64.of_int 1 else Int64.of_int 0)) !%rax ++
                      movq (imm 2) !%r10
            | Bneq -> movq (imm64 (if s1 <> s2 then Int64.of_int 1 else Int64.of_int 0)) !%rax ++ 
                       movq (imm 2) !%r10
            | Blt -> movq (imm64 (if s1 < s2 then Int64.of_int 1 else Int64.of_int 0)) !%rax ++
                      movq (imm 2) !%r10
            | Ble -> movq (imm64 (if s1 <= s2 then Int64.of_int 1 else Int64.of_int 0)) !%rax ++
                      movq (imm 2) !%r10
            | Bgt -> movq (imm64 (if s1 > s2 then Int64.of_int 1 else Int64.of_int 0)) !%rax ++
                      movq (imm 2) !%r10
            | Bge -> movq (imm64 (if s1 >= s2 then Int64.of_int 1 else Int64.of_int 0)) !%rax ++
                      movq (imm 2) !%r10
            | _ -> failwith "Type error: cannot perform operation on strings")
       | TEcst (Cint _), TEcst (Cstring _)
       | TEcst (Cstring _), TEcst (Cint _) ->
           failwith "Type error: cannot add integer and string"
       | _ ->
           (match op with
            | Band -> 
              let false_label = new_label () in
              let end_label = new_label () in
              compile_expr lhs ++
              testq !%rax !%rax ++
              jz false_label ++
              compile_expr rhs ++
              testq !%rax !%rax ++
              jz false_label ++
              movq (imm 1) !%rax ++
              jmp end_label ++
              label false_label ++
              movq (imm 0) !%rax ++
              label end_label ++
              movq (imm 2) !%r10
            | Bor -> 
              let true_label = new_label () in
              let end_label = new_label () in
              compile_expr lhs ++
              testq !%rax !%rax ++
              jnz true_label ++
              compile_expr rhs ++
              testq !%rax !%rax ++
              jnz true_label ++
              movq (imm 0) !%rax ++
              jmp end_label ++
              label true_label ++
              movq (imm 1) !%rax ++
              label end_label ++
              movq (imm 2) !%r10
            | _ -> (
              compile_expr rhs ++
              pushq !%rax ++
              compile_expr lhs ++
              popq rbx ++ 
              match op with
                | Badd -> addq !%rbx !%rax
                | Bsub -> subq !%rbx !%rax
                | Bmul -> imulq !%rbx !%rax
                | Bdiv -> cqto ++ idivq !%rbx
                | Bmod -> cqto ++ idivq !%rbx ++ movq !%rdx !%rax
                | Beq -> cmpq !%rbx !%rax ++ sete !%al ++ movzbq !%al rax ++ movq (imm 2) !%r10
                | Bneq -> cmpq !%rbx !%rax ++ setne !%al ++ movzbq !%al rax ++ movq (imm 2) !%r10
                | Blt -> cmpq !%rbx !%rax ++ setl !%al ++ movzbq !%al rax ++ movq (imm 2) !%r10
                | Ble -> cmpq !%rbx !%rax ++ setle !%al ++ movzbq !%al rax ++ movq (imm 2) !%r10
                | Bgt -> cmpq !%rbx !%rax ++ setg !%al ++ movzbq !%al rax ++ movq (imm 2) !%r10
                | Bge -> cmpq !%rbx !%rax ++ setge !%al ++ movzbq !%al rax ++ movq (imm 2) !%r10
                | _ -> failwith "Type error: cannot perform operation on strings"
              )))
  | TEunop (Uneg, e) -> compile_expr e ++ negq !%rax
  | TEunop (Unot, e) -> compile_expr e ++ notq !%rax
  | TEcall (fn, args) ->
      (match fn.fn_name with
      | "len" ->
          compile_expr (List.hd args) ++
          movq (ind ~ofs:0 rax) !%rax
      | _ ->
        (* Generate code to push the arguments onto the stack *)
        let code = List.fold_right (fun arg code -> compile_expr arg ++ pushq !%rax ++ code) args nop in
        (* Call the function *)
        let code = code ++ call fn.fn_name in
        code
      )
  | TElist elements -> 
      let num_elements = List.length elements in
      let allocate_space = subq (imm (8 * (num_elements + 1))) !%rsp in
      let compile_element (code, offset) elem =
        code ++ compile_expr elem ++ movq !%rax (ind ~ofs:offset rbp), offset - 8
      in
      let ptr = !current_stack_offset in
      current_stack_offset := ptr - 8 * (num_elements + 1);
      let code, _ = List.fold_left compile_element (nop, ptr - 16) elements in
      allocate_space ++ 
      movq (imm (num_elements)) (ind ~ofs:(ptr-8) rbp) ++
      code ++
      movq !%rbp !%rax ++ 
      addq (imm (ptr - 8)) !%rax ++
      movq (imm 3) !%r10
  | TErange _ -> failwith "Range is not supported in code generation"
  | TEget (list_expr, index_expr) ->
      compile_expr index_expr ++
      addq (imm 1) !%rax ++  (* Increment index by 1 to account for the length field *)
      pushq !%rax ++  (* Save index on stack *)
      compile_expr list_expr ++
      popq rbx ++  (* Restore index from stack *)
      movq !%rax !%rcx ++  (* Save base address of list in rcx *)
      imulq (imm 8) !%rbx ++  (* Multiply index by 8 (size of each element) *)
      subq !%rbx !%rcx ++  (* Add offset to base address *)
      movq (ind ~ofs:0 rcx) !%rax  (* Load the element into rax *)

(* Example of updating stack offset during variable assignment *)
let rec compile_stmt = function
  | TSif (cond, then_branch, else_branch) ->
      let else_label = new_label () in
      let end_label = new_label () in
      compile_expr cond ++
      testq !%rax !%rax ++
      jz else_label ++
      compile_stmt then_branch ++
      jmp end_label ++
      X86_64.label else_label ++
      compile_stmt else_branch ++
      X86_64.label end_label
  | TSreturn e ->
        (match e with
          | TEcst (Cstring s) -> 
              compile_expr e ++ 
              movq (imm 0) !%r10
          | TEcst (Cint _) ->
              compile_expr e ++
              movq (imm 1) !%r10
          | TEcst (Cbool _) ->
              compile_expr e ++
              movq (imm 2) !%r10
          | TEcst (Cnone) ->
              movq (imm 0) !%rax ++
              movq (imm 4) !%r10
          | TEvar v -> (
              let current_var_table = Stack.top var_table_stack in
              (match Hashtbl.find_opt current_var_table v.v_name with
              | Some (stack_offset) ->
                  movq (ind ~ofs:stack_offset rbp) !%rax
              | None ->
                  failwith ("Variable " ^ v.v_name ^ " not found in symbol table"))
             )
          | _ -> compile_expr e
        )
  | TSassign (v, e) -> 
      (* Calculate the stack offset for the variable *)
      let (stack_offset, type_offset) = calculate_stack_offset v in
      record_var_name v.v_name stack_offset;
      v.v_ofs <- stack_offset;
      (* Record the variable name, type, and stack offset *)
      subq (imm 8) !%rsp ++
      compile_expr e ++
      movq !%rax (ind ~ofs:stack_offset rbp) ++
      (match e with
       | TEcst (Cstring s) ->
            movq (imm 0) (ind ~ofs:type_offset rbp)
       | TEcst (Cint _) ->
            movq (imm 1) (ind ~ofs:type_offset rbp)
       | TEcst (Cbool _) ->
            movq (imm 2) (ind ~ofs:type_offset rbp)
       | TElist _ ->
            movq (imm 3) (ind ~ofs:type_offset rbp)
       | _ -> failwith "Type error: cannot assign variable of type None") ++ movq (imm 4) !%r10
  | TSprint e -> 
    let print_string = new_label () in
    let print_int = new_label () in
    let print_bool = new_label () in
    let print_none = new_label () in
    let print_end = new_label () in
    let print_true = new_label () in
    compile_expr e ++
    movq !%rax !%rsi ++
    movq (imm 0) !%rax ++
    cmpq (imm 0) !%r10 ++
    je print_string ++
    cmpq (imm 1) !%r10 ++
    je print_int ++
    cmpq (imm 2) !%r10 ++
    je print_bool ++
    jmp print_none ++

    label print_string ++
    movq (ilab "fmt_str") !%rdi ++
    jmp print_end ++

    label print_int ++
    movq (ilab "fmt_int") !%rdi ++
    jmp print_end ++

    label print_bool ++
    cmpq (imm 1) !%rsi ++
    je print_true ++
    movq (ilab "false_str") !%rdi ++
    jmp print_end ++

    label print_true ++
    movq (ilab "true_str") !%rdi ++
    jmp print_end ++

    label print_none ++
    movq (ilab "none_str") !%rdi ++

    label print_end ++
    call "printf"
  | TSblock stmts -> List.fold_left (++) nop (List.map compile_stmt stmts)
  | TSfor (v, collection, body) ->
      (* Record the loop variable name, type, and stack offset *)
      record_var_name v.v_name v.v_ofs;
      movq (imm 1) !%rax ++
      compile_expr collection ++
      compile_stmt body
  | TSeval e -> compile_expr e
  | TSset (collection, index, value) ->
      compile_expr collection ++
      compile_expr index ++
      compile_expr value

let compile_def (fn, body) =
  (* Calculate stack offsets for the function params *)
  let params_offsets = List.mapi (fun i var -> (var, -8 * (i + 1))) fn.fn_params in
  let prologue = 
    pushq !%rbp ++ 
    movq !%rsp !%rbp ++
    List.fold_left (++) nop (List.map (fun (v, offset) -> 
      let (stack_offset, type_offset) = calculate_stack_offset v in
      movq (ind ~ofs:(8-stack_offset) rbp) !%rax ++
      movq !%rax (ind ~ofs:offset rbp)
      ) params_offsets)
  in
  enter_scope ();
  List.iter (fun (v, offset) -> record_var_name v.v_name offset) params_offsets;
  current_fn_name := fn.fn_name;
  let code = 
    compile_stmt body ++
    (if fn.fn_name = "main" then
      movq (imm 0) !%rax
    else
      nop) ++
    leave ++
    ret
  in
  exit_scope ();
  label fn.fn_name ++ 
  prologue ++
  code

let file ?debug:(b=false) (p: Ast.tfile) : X86_64.program =
  debug := b;
  let text_section = 
    globl "main" ++ 
    List.fold_left (++) nop (List.map compile_def p) in
  let data_section = 
    List.fold_left (fun acc (lbl, str) -> acc ++ X86_64.label lbl ++ string str) nop !string_constants ++
    X86_64.label "fmt_int" ++
    string "%d\n" ++
    X86_64.label "fmt_str" ++
    string "%s\n" ++
    X86_64.label "true_str" ++
    string "True\n" ++
    X86_64.label "false_str" ++
    string "False\n" ++
    X86_64.label "none_str" ++
    string "None\n"
    in
  { text = text_section; data = data_section }