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

let compile_constant = function
  | Cnone -> 0L
  | Cbool true -> 1L
  | Cbool false -> 0L
  | Cstring s -> 1L
  | Cint i -> i

(* Define a stack to keep track of symbol tables for each scope *)
let var_table_stack = Stack.create ()

(* Function to record variable name, type, and stack offset *)
let record_var_name var_name var_type var_ofs =
  let current_var_table = Stack.top var_table_stack in
  Hashtbl.replace current_var_table var_name (var_type, var_ofs)

(* Define a stack to keep track of stack offsets for each scope *)
let stack_offset_stack = Stack.create ()

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
  | Some (_, existing_offset) -> existing_offset
  | None ->
      (* Assuming each variable occupies 8 bytes on the stack *)
      let var_size = 8 in
      (* Update the current stack offset *)
      current_stack_offset := !current_stack_offset - var_size;
      !current_stack_offset

(* Define a hash table to store function names and their return types *)
let function_return_types = Hashtbl.create 100

(* Initialize the hash table with known functions and their return types *)
let initialize_function_return_types () =
  Hashtbl.add function_return_types "len" "int";
  (* Add other functions and their return types here *)
  ()

let current_fn_name = ref "main"

let rec compile_expr = function
  | TEcst c -> movq (imm64 (compile_constant c)) !%rax
  | TEvar v ->
      let current_var_table = Stack.top var_table_stack in
      (match Hashtbl.find_opt current_var_table v.v_name with
       | Some (_, stack_offset) ->
           (* Generate code to load the variable's value from the stack *)
           movq (ind ~ofs:stack_offset rbp) !%rax
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
                call "strcat"
            | Beq -> movq (imm64 (if s1 = s2 then Int64.of_int 1 else Int64.of_int 0)) !%rax
            | Bneq -> movq (imm64 (if s1 <> s2 then Int64.of_int 1 else Int64.of_int 0)) !%rax
            | Blt -> movq (imm64 (if s1 < s2 then Int64.of_int 1 else Int64.of_int 0)) !%rax
            | Ble -> movq (imm64 (if s1 <= s2 then Int64.of_int 1 else Int64.of_int 0)) !%rax
            | Bgt -> movq (imm64 (if s1 > s2 then Int64.of_int 1 else Int64.of_int 0)) !%rax
            | Bge -> movq (imm64 (if s1 >= s2 then Int64.of_int 1 else Int64.of_int 0)) !%rax
            | _ -> failwith "Type error: cannot perform operation on strings")
       | TEcst (Cint _), TEcst (Cstring _)
       | TEcst (Cstring _), TEcst (Cint _) ->
           jmp "error"  (* Exit the program if there is a type error *)
       | TElist _, TElist _ ->
            (match op with
              | Beq -> 
                  let false_label = new_label () in
                  let end_label = new_label () in
                  let allocate_space = subq (imm 16) !%rsp in
                  let ptr = !current_stack_offset in
                  current_stack_offset := ptr - 16;
                  let lhs_code = compile_expr lhs in
                  let rhs_code = compile_expr rhs in  
                  allocate_space ++
                  lhs_code ++
                  movq !%rax (ind ~ofs:(ptr-8) rbp) ++
                  rhs_code ++
                  movq !%rax (ind ~ofs:(ptr-16) rbp) ++
                  movq (ind ~ofs:(ptr-8) rbp) !%rsi ++
                  movq (ind ~ofs:(ptr-16) rbp) !%rdi ++
                  movq (imm 0) !%rax ++
                  call "compare_lists" ++
                  testq !%rax !%rax ++
                  jz false_label ++
                  movq (imm 1) !%rax ++
                  jmp end_label ++
                  label false_label ++
                  movq (imm 0) !%rax ++
                  label end_label
              | Bneq -> 
                  let true_label = new_label () in
                  let end_label = new_label () in
                  let allocate_space = subq (imm 16) !%rsp in
                  let ptr = !current_stack_offset in
                  current_stack_offset := ptr - 16;
                  let lhs_code = compile_expr lhs in
                  let rhs_code = compile_expr rhs in  
                  allocate_space ++
                  lhs_code ++
                  movq !%rax (ind ~ofs:(ptr-8) rbp) ++
                  rhs_code ++
                  movq !%rax (ind ~ofs:(ptr-16) rbp) ++
                  movq (ind ~ofs:(ptr-8) rbp) !%rsi ++
                  movq (ind ~ofs:(ptr-16) rbp) !%rdi ++
                  movq (imm 0) !%rax ++
                  call "compare_lists" ++
                  testq !%rax !%rax ++
                  jnz true_label ++
                  movq (imm 1) !%rax ++
                  jmp end_label ++
                  label true_label ++
                  movq (imm 0) !%rax ++
                  label end_label
              | _ -> failwith "Type error: cannot perform operation on lists")
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
              label end_label
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
              label end_label
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
                | Beq -> cmpq !%rbx !%rax ++ sete !%al ++ movzbq !%al rax
                | Bneq -> cmpq !%rbx !%rax ++ setne !%al ++ movzbq !%al rax
                | Blt -> cmpq !%rbx !%rax ++ setl !%al ++ movzbq !%al rax
                | Ble -> cmpq !%rbx !%rax ++ setle !%al ++ movzbq !%al rax
                | Bgt -> cmpq !%rbx !%rax ++ setg !%al ++ movzbq !%al rax
                | Bge -> cmpq !%rbx !%rax ++ setge !%al ++ movzbq !%al rax
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
        (* Calculate stack offsets for the function arguments *)
        let arg_offsets = List.mapi (fun i arg -> (arg, -8 * (i + 1))) args in
        (* Record the function arguments in the symbol table *)
        List.iter (fun (arg, offset) ->
          match arg with
          | TEvar v -> record_var_name v.v_name "int" offset  (* Assuming all arguments are integers *)
          | _ -> ()) arg_offsets;
        (* Generate code to push the arguments onto the stack *)
        let code = List.fold_right (fun (arg, _) code -> compile_expr arg ++ pushq !%rax ++ code) arg_offsets nop in
        (* Call the function *)
        let code = code ++ call fn.fn_name in
        (* Restore the stack pointer *)
        let code = code ++ addq (imm (8 * List.length args)) !%rsp in
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
      addq (imm (ptr - 8)) !%rax
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
              Hashtbl.add function_return_types !current_fn_name "string";
              let lbl = new_label () in
              string_constants := (lbl, s) :: !string_constants;
              leaq (lab lbl) rax 
          | TEcst (Cint _) ->
              Hashtbl.add function_return_types !current_fn_name "int";
              compile_expr e
          | TEcst (Cbool _) ->
              Hashtbl.add function_return_types !current_fn_name "bool";
              compile_expr e
          | TEcst (Cnone) ->
              Hashtbl.add function_return_types !current_fn_name "none";
              movq (imm 0) !%rax
          | _ -> compile_expr e
        )
  | TSassign (v, e) -> 
      (* Calculate the stack offset for the variable *)
      let stack_offset = calculate_stack_offset v in
      (* Record the variable name, type, and stack offset *)
      subq (imm 8) !%rsp ++
      (match e with
       | TEcst (Cstring s) ->
            record_var_name v.v_name "string" stack_offset;
            let lbl = new_label () in
            string_constants := (lbl, s) :: !string_constants;
            leaq (lab lbl) rax ++
            movq !%rax (ind ~ofs:stack_offset rbp)
       | TEcst (Cint _) ->
            record_var_name v.v_name "int" stack_offset;
            compile_expr e ++ movq !%rax (ind ~ofs:stack_offset rbp)
       | TEcst (Cbool _) ->
            record_var_name v.v_name "bool" stack_offset;
            compile_expr e ++ movq !%rax (ind ~ofs:stack_offset rbp)
       | TEbinop (_, TEvar (v1), TEvar (v2)) ->
            let current_var_table = Stack.top var_table_stack in
            (match Hashtbl.find_opt current_var_table v1.v_name, Hashtbl.find_opt current_var_table v2.v_name with
              | Some ("int", _), Some ("int", _) ->
                  record_var_name v.v_name "int" stack_offset;
                  compile_expr e ++ movq !%rax (ind ~ofs:stack_offset rbp)
              | Some ("bool", _), Some ("bool", _) ->
                record_var_name v.v_name "bool" stack_offset;
                compile_expr e ++ movq !%rax (ind ~ofs:stack_offset rbp)
              | Some ("string", _), Some ("string", _) ->
                record_var_name v.v_name "string" stack_offset;
                compile_expr e ++ movq !%rax (ind ~ofs:stack_offset rbp)
              | _ -> failwith "Type error: cannot assign variables of different types")
       | TElist _ ->
            record_var_name v.v_name "list" stack_offset;
            compile_expr e ++ movq !%rax (ind ~ofs:stack_offset rbp)
       | _ -> failwith "Type error: cannot assign variable of type None")
  | TSprint e -> 
      (match e with
       | TEvar v ->
            let current_var_table = Stack.top var_table_stack in
           (match Hashtbl.find_opt current_var_table v.v_name with
            | Some ("string", _) ->
                compile_expr e ++
                movq !%rax !%rsi ++
                leaq (lab "fmt_str") rdi ++
                movq (imm 0) !%rax ++
                call "printf"
            | Some ("int", _) | Some ("bool", _) ->
                compile_expr e ++
                movq !%rax !%rsi ++
                leaq (lab "fmt_int") rdi ++
                movq (imm 0) !%rax ++
                call "printf"
            | _ -> failwith "Type error: cannot print variable of type None")
       | TEbinop (Badd, TEcst (Cstring _), TEcst (Cstring _)) ->
           compile_expr e ++
           movq !%rax !%rsi ++
           leaq (lab "fmt_str") rdi ++
           movq (imm 0) !%rax ++
           call "printf"
       | TEcst (Cstring s) ->
           let lbl = new_label () in
           string_constants := (lbl, s) :: !string_constants;
           leaq (lab lbl) rax ++
           movq !%rax !%rsi ++
           leaq (lab "fmt_str") rdi ++
           movq (imm 0) !%rax ++
           call "printf"
       | TEcst (Cbool true) ->
           leaq (lab "true_str") rdi ++
           movq (imm 0) !%rax ++
           call "printf"          
       | TEbinop (Band, _, _) | TEbinop (Bor, _, _)
       | TEbinop (Beq, _, _) | TEbinop (Bneq, _, _) | TEbinop (Blt, _, _) 
       | TEbinop (Ble, _, _) | TEbinop (Bgt, _, _) | TEbinop (Bge, _, _) ->
           let false_label = new_label () in
           let end_label = new_label () in
           compile_expr e ++
           testq !%rax !%rax ++
           jz false_label ++
           leaq (lab "true_str") rdi ++
           movq (imm 0) !%rax ++
           call "printf" ++
           jmp end_label ++
           X86_64.label false_label ++
           leaq (lab "false_str") rdi ++
           movq (imm 0) !%rax ++
           call "printf" ++
           X86_64.label end_label
       | TEcst (Cbool false) ->
           leaq (lab "false_str") rdi ++
           movq (imm 0) !%rax ++
           call "printf"
       | TEcst (Cnone) ->
          leaq (lab "none_str") rdi ++
          movq (imm 0) !%rax ++
          call "printf"
       | TEcall (fn, _) ->
           (match Hashtbl.find_opt function_return_types fn.fn_name with
            | Some "string" ->
                compile_expr e ++
                movq !%rax !%rsi ++
                leaq (lab "fmt_str") rdi ++
                movq (imm 0) !%rax ++
                call "printf"
            | Some "int" ->
                compile_expr e ++
                movq !%rax !%rsi ++
                leaq (lab "fmt_int") rdi ++
                movq (imm 0) !%rax ++
                call "printf"
            | Some "bool" ->
                let false_label = new_label () in
                let end_label = new_label () in
                compile_expr e ++
                testq !%rax !%rax ++
                jz false_label ++
                leaq (lab "true_str") rdi ++
                movq (imm 0) !%rax ++
                call "printf" ++
                jmp end_label ++
                X86_64.label false_label ++
                leaq (lab "false_str") rdi ++
                movq (imm 0) !%rax ++
                call "printf" ++
                X86_64.label end_label
            | _ ->
                leaq (lab "none_str") rdi ++
                movq (imm 0) !%rax ++
                call "printf")
       | _ ->
           compile_expr e ++
           movq !%rax !%rsi ++
           leaq (lab "fmt_int") rdi ++
           movq (imm 0) !%rax ++
           call "printf")
  | TSblock stmts -> List.fold_left (++) nop (List.map compile_stmt stmts)
  | TSfor (v, collection, body) ->
      (* Record the loop variable name, type, and stack offset *)
      record_var_name v.v_name "int" v.v_ofs; (* Assuming loop variable is an integer *)
      compile_expr collection ++
      compile_stmt body
  | TSeval e -> compile_expr e
  | TSset (collection, index, value) ->
      compile_expr collection ++
      compile_expr index ++
      compile_expr value

let compile_def (fn, body) =
  enter_scope ();
  current_fn_name := fn.fn_name;
  let code = 
    pushq !%rbp ++
    movq !%rsp !%rbp ++
    compile_stmt body ++
    (if fn.fn_name = "main" then
      movq (imm 0) !%rax
    else
      nop) ++
    leave ++
    ret
  in
  exit_scope ();
  label fn.fn_name ++ code

let file ?debug:(b=false) (p: Ast.tfile) : X86_64.program =
  debug := b;
  let text_section = 
    globl "main" ++ 
    List.fold_left (++) nop (List.map compile_def p) ++
    label "error" ++
    leave ++
    label "compare_lists" ++
    pushq !%rbp ++
    movq !%rsp !%rbp ++
    movq (ind ~ofs:0 rsi) !%rcx ++  (* Load length of first list into rcx *)
    movq (ind ~ofs:0 rdi) !%rdx ++  (* Load length of second list into rdx *)
    let not_equal = new_label () in
    cmpq !%rcx !%rdx ++  (* Compare lengths *)
    jne not_equal ++ (* Jump if lengths are not equal *)

    movq (imm 1) !%rax ++  (* Initialize index *)
    incq !%rcx ++

    (* Compare elements of the lists *)
    label "compare_elements" ++
    cmpq !%rax !%rcx ++  (* Compare index with length *)
    je "end_compare" ++  (* Jump if all elements are compared *)
    

    (* Load elements from both lists and compare *)
    movq !%rax !%rbx ++  (* Load index into rbx *)
    imulq (imm 8) !%rbx ++  (* Multiply index by 8 (size of each element) *)
    movq !%rsi !%rdx ++  (* Load base address of first list into rdx *)
    subq !%rbx !%rdx ++  (* sub offset to base address *)
    movq (ind ~ofs:0 rdx) !%r8 ++
    movq !%rdi !%rdx ++  (* Load base address of second list into rdx *)
    subq !%rbx !%rdx ++  (* sub offset to base address *)
    movq (ind ~ofs:0 rdx) !%r9 ++
    cmpq !%r8 !%r9 ++
    jne not_equal ++  (* Jump if elements are not equal *)

    incq !%rax ++  (* Increment index *)
    jmp "compare_elements" ++

    label "end_compare" ++
    movq (imm 1) !%rax ++  (* Lists are equal *)
    leave ++
    ret ++
    
    label not_equal ++
    movq (imm 0) !%rax ++  (* Lists are not equal *)
    leave ++
    ret 
    in
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