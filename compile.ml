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

let current_fn_name = ref "main"

(* Define a variable to keep track of the current stack offset *)
let current_stack_offset = ref 0

let rec compile_expr = function
  | TEcst c ->
    (match c with
      | Cnone -> 
        movq (imm 2) !%rdi ++
        call "my_malloc" ++
        movq (imm 0) (ind ~ofs:(-8) rax) ++
        movq (imm 0) (ind ~ofs:(-16) rax) 
      | Cbool b ->
        movq (imm 2) !%rdi ++
        call "my_malloc" ++
        movq (imm 1) (ind ~ofs:(-8) rax) ++
        movq (imm (if b then 1 else 0)) (ind ~ofs:(-16) rax) 
      | Cint i -> 
        movq (imm 2) !%rdi ++
        call "my_malloc" ++
        movq (imm 2) (ind ~ofs:(-8) rax) ++
        movq (imm64 i) (ind ~ofs:(-16) rax) 
      | Cstring s ->
        let lbl = new_label () in
        string_constants := (lbl, s) :: !string_constants;
        movq (imm 3) !%rdi ++
        call "my_malloc" ++
        movq (imm 3) (ind ~ofs:(-8) rax) ++
        movq (imm (String.length s)) (ind ~ofs:(-16) rax) ++
        leaq (lab lbl) rbx ++
        movq !%rbx (ind ~ofs:(-24) rax)
      )
  | TEvar v -> movq (ind ~ofs:v.v_ofs rbp) !%rax
  | TEbinop (Badd, TEcst (Cstring s1), TEcst (Cstring s2)) ->
      let lbl1 = new_label () in
      let lbl2 = new_label () in
      string_constants := (lbl1, s1) :: (lbl2, s2) :: !string_constants;
      leaq (lab lbl2) rsi ++
      leaq (lab lbl1) rdi ++
      movq (imm 0) !%rax ++
      call "strcat" ++
      movq (imm 3) !%rdi ++
      pushq !%rax ++
      call "my_malloc" ++
      movq (imm 3) (ind ~ofs:(-8) rax) ++
      movq (imm ((String.length s1) + (String.length s2))) (ind ~ofs:(-16) rax) ++
      popq rbx ++
      movq !%rbx (ind ~ofs:(-24) rax)
  | TEbinop (op, lhs, rhs) -> 
    (match op with
      | Badd ->
          compile_expr lhs ++
          pushq !%rax ++
          compile_expr rhs ++
          popq r10 ++
          movq (ind ~ofs:(-8) rax) !%r11 ++
          movq (ind ~ofs:(-8) r10) !%r12 ++
          cmpq !%r11 !%r12 ++
          jne "error" ++
          movq (ind ~ofs:(-16) rax) !%r13 ++
          movq (ind ~ofs:(-16) r10) !%r14 ++
          addq !%r13 !%r14 ++
          cmpq (imm 3) !%r11 ++
          cmpq (imm 2) !%r11 ++
          jne "error" ++
          movq (imm 2) !%rdi ++
          call "my_malloc" ++
          movq (imm 2) (ind ~ofs:(-8) rax) ++
          movq !%r14 (ind ~ofs:(-16) rax)
      | Bsub ->
          compile_expr lhs ++
          pushq !%rax ++
          compile_expr rhs ++
          popq r10 ++
          movq (ind ~ofs:(-8) rax) !%r11 ++
          movq (ind ~ofs:(-8) r10) !%r12 ++
          cmpq !%r11 !%r12 ++
          jne "error" ++
          movq (ind ~ofs:(-16) rax) !%r13 ++
          movq (ind ~ofs:(-16) r10) !%r14 ++
          subq !%r13 !%r14 ++
          cmpq (imm 2) !%r11 ++
          jne "error" ++
          movq (imm 2) !%rdi ++
          call "my_malloc" ++
          movq (imm 2) (ind ~ofs:(-8) rax) ++
          movq !%r14 (ind ~ofs:(-16) rax)
      | Bmul ->
          compile_expr lhs ++
          pushq !%rax ++
          compile_expr rhs ++
          popq r10 ++
          movq (ind ~ofs:(-8) rax) !%r11 ++
          movq (ind ~ofs:(-8) r10) !%r12 ++
          cmpq !%r11 !%r12 ++
          jne "error" ++
          movq (ind ~ofs:(-16) rax) !%r13 ++
          movq (ind ~ofs:(-16) r10) !%r14 ++
          imulq !%r13 !%r14 ++
          cmpq (imm 2) !%r11 ++
          jne "error" ++
          movq (imm 2) !%rdi ++
          call "my_malloc" ++
          movq (imm 2) (ind ~ofs:(-8) rax) ++
          movq !%r14 (ind ~ofs:(-16) rax)
      | Bdiv ->
          compile_expr lhs ++
          pushq !%rax ++
          compile_expr rhs ++
          popq r10 ++
          movq (ind ~ofs:(-8) rax) !%r11 ++
          movq (ind ~ofs:(-8) r10) !%r12 ++
          cmpq !%r11 !%r12 ++
          jne "error" ++
          movq (ind ~ofs:(-16) rax) !%r13 ++
          movq (ind ~ofs:(-16) r10) !%r14 ++
          movq !%r14 !%rax ++
          cqto ++
          idivq !%r13 ++
          movq !%rax !%r14 ++
          cmpq (imm 2) !%r11 ++
          jne "error" ++
          movq (imm 2) !%rdi ++
          call "my_malloc" ++
          movq (imm 2) (ind ~ofs:(-8) rax) ++
          movq !%r14 (ind ~ofs:(-16) rax)
      | Bmod ->
          compile_expr lhs ++
          pushq !%rax ++
          compile_expr rhs ++
          popq r10 ++
          movq (ind ~ofs:(-8) rax) !%r11 ++
          movq (ind ~ofs:(-8) r10) !%r12 ++
          cmpq !%r11 !%r12 ++
          jne "error" ++
          movq (ind ~ofs:(-16) rax) !%r13 ++
          movq (ind ~ofs:(-16) r10) !%r14 ++
          movq !%r14 !%rax ++
          cqto ++
          idivq !%r13 ++
          movq !%rdx !%r14 ++
          cmpq (imm 2) !%r11 ++
          jne "error" ++
          movq (imm 2) !%rdi ++
          call "my_malloc" ++
          movq (imm 2) (ind ~ofs:(-8) rax) ++
          movq !%r14 (ind ~ofs:(-16) rax)
      | Beq ->
          let string_label = new_label () in
          let end_label = new_label () in
          compile_expr lhs ++
          pushq !%rax ++
          compile_expr rhs ++
          popq r10 ++
          movq (ind ~ofs:(-8) rax) !%r11 ++
          movq (ind ~ofs:(-8) r10) !%r12 ++
          cmpq !%r11 !%r12 ++
          jne "error" ++
          cmpq (imm 3) !%r11 ++
          je string_label ++
          movq (ind ~ofs:(-16) rax) !%r13 ++
          movq (ind ~ofs:(-16) r10) !%r14 ++
          cmpq !%r13 !%r14 ++
          sete !%al ++
          movzbq !%al r14 ++
          movq (imm 2) !%rdi ++
          call "my_malloc" ++
          movq (imm 1) (ind ~ofs:(-8) rax) ++
          movq !%r14 (ind ~ofs:(-16) rax) ++
          jmp end_label ++
          label string_label ++
          movq (ind ~ofs:(-24) rax) !%rdi ++
          movq (ind ~ofs:(-24) r10) !%rsi ++
          movq (imm 0) !%rax ++
          call "strcmp" ++
          cmpq (imm 0) !%rax ++
          sete !%al ++
          movzbq !%al r14 ++
          movq (imm 2) !%rdi ++
          call "my_malloc" ++
          movq (imm 1) (ind ~ofs:(-8) rax) ++
          movq !%r14 (ind ~ofs:(-16) rax) ++
          label end_label
      | Bneq ->
        let string_label = new_label () in
        let end_label = new_label () in
        compile_expr lhs ++
        pushq !%rax ++
        compile_expr rhs ++
        popq r10 ++
        movq (ind ~ofs:(-8) rax) !%r11 ++
        movq (ind ~ofs:(-8) r10) !%r12 ++
        cmpq !%r11 !%r12 ++
        jne "error" ++
        cmpq (imm 3) !%r11 ++
        je string_label ++
        movq (ind ~ofs:(-16) rax) !%r13 ++
        movq (ind ~ofs:(-16) r10) !%r14 ++
        cmpq !%r13 !%r14 ++
        setne !%al ++
        movzbq !%al r14 ++
        movq (imm 2) !%rdi ++
        call "my_malloc" ++
        movq (imm 1) (ind ~ofs:(-8) rax) ++
        movq !%r14 (ind ~ofs:(-16) rax) ++
        jmp end_label ++
        label string_label ++
        movq (ind ~ofs:(-24) rax) !%rdi ++
        movq (ind ~ofs:(-24) r10) !%rsi ++
        movq (imm 0) !%rax ++
        call "strcmp" ++
        cmpq (imm 0) !%rax ++
        setne !%al ++
        movzbq !%al r14 ++
        movq (imm 2) !%rdi ++
        call "my_malloc" ++
        movq (imm 1) (ind ~ofs:(-8) rax) ++
        movq !%r14 (ind ~ofs:(-16) rax) ++
        label end_label
      | Blt ->
        let string_label = new_label () in
        let end_label = new_label () in
        compile_expr lhs ++
        pushq !%rax ++
        compile_expr rhs ++
        popq r10 ++
        movq (ind ~ofs:(-8) rax) !%r11 ++
        movq (ind ~ofs:(-8) r10) !%r12 ++
        cmpq !%r11 !%r12 ++
        jne "error" ++
        cmpq (imm 3) !%r11 ++
        je string_label ++
        movq (ind ~ofs:(-16) rax) !%r13 ++
        movq (ind ~ofs:(-16) r10) !%r14 ++
        cmpq !%r13 !%r14 ++
        setl !%al ++
        movzbq !%al r14 ++
        movq (imm 2) !%rdi ++
        call "my_malloc" ++
        movq (imm 1) (ind ~ofs:(-8) rax) ++
        movq !%r14 (ind ~ofs:(-16) rax) ++
        jmp end_label ++
        label string_label ++
        movq (ind ~ofs:(-24) rax) !%rdi ++
        movq (ind ~ofs:(-24) r10) !%rsi ++
        movq (imm 0) !%rax ++
        call "strcmp" ++
        cmpq (imm 0) !%rax ++
        setg !%al ++
        movzbq !%al r14 ++
        movq (imm 2) !%rdi ++
        call "my_malloc" ++
        movq (imm 1) (ind ~ofs:(-8) rax) ++
        movq !%r14 (ind ~ofs:(-16) rax) ++
        label end_label
      | Ble ->
        let string_label = new_label () in
        let end_label = new_label () in
        compile_expr lhs ++
        pushq !%rax ++
        compile_expr rhs ++
        popq r10 ++
        movq (ind ~ofs:(-8) rax) !%r11 ++
        movq (ind ~ofs:(-8) r10) !%r12 ++
        cmpq !%r11 !%r12 ++
        jne "error" ++
        cmpq (imm 3) !%r11 ++
        je string_label ++
        movq (ind ~ofs:(-16) rax) !%r13 ++
        movq (ind ~ofs:(-16) r10) !%r14 ++
        cmpq !%r13 !%r14 ++
        setle !%al ++
        movzbq !%al r14 ++
        movq (imm 2) !%rdi ++
        call "my_malloc" ++
        movq (imm 1) (ind ~ofs:(-8) rax) ++
        movq !%r14 (ind ~ofs:(-16) rax) ++
        jmp end_label ++
        label string_label ++
        movq (ind ~ofs:(-24) rax) !%rdi ++
        movq (ind ~ofs:(-24) r10) !%rsi ++
        movq (imm 0) !%rax ++
        call "strcmp" ++
        cmpq (imm 0) !%rax ++
        setle !%al ++
        movzbq !%al r13 ++
        movq (imm 2) !%rdi ++
        call "my_malloc" ++
        movq (imm 1) (ind ~ofs:(-8) rax) ++
        orq !%r13 !%r14 ++
        movq !%r14 (ind ~ofs:(-16) rax) ++
        label end_label
      | Bgt ->
        let string_label = new_label () in
        let end_label = new_label () in
        compile_expr lhs ++
        pushq !%rax ++
        compile_expr rhs ++
        popq r10 ++
        movq (ind ~ofs:(-8) rax) !%r11 ++
        movq (ind ~ofs:(-8) r10) !%r12 ++
        cmpq !%r11 !%r12 ++
        jne "error" ++
        cmpq (imm 3) !%r11 ++
        je string_label ++
        movq (ind ~ofs:(-16) rax) !%r13 ++
        movq (ind ~ofs:(-16) r10) !%r14 ++
        cmpq !%r13 !%r14 ++
        setg !%al ++
        movzbq !%al r14 ++
        movq (imm 2) !%rdi ++
        call "my_malloc" ++
        movq (imm 1) (ind ~ofs:(-8) rax) ++
        movq !%r14 (ind ~ofs:(-16) rax) ++
        jmp end_label ++
        label string_label ++
        movq (ind ~ofs:(-24) rax) !%rdi ++
        movq (ind ~ofs:(-24) r10) !%rsi ++
        movq (imm 0) !%rax ++
        call "strcmp" ++
        cmpq (imm 0) !%rax ++
        setl !%al ++
        movzbq !%al r14 ++
        movq (imm 2) !%rdi ++
        call "my_malloc" ++
        movq (imm 1) (ind ~ofs:(-8) rax) ++
        movq !%r14 (ind ~ofs:(-16) rax) ++
        label end_label
      | Bge ->
        let string_label = new_label () in
        let end_label = new_label () in
        compile_expr lhs ++
        pushq !%rax ++
        compile_expr rhs ++
        popq r10 ++
        movq (ind ~ofs:(-8) rax) !%r11 ++
        movq (ind ~ofs:(-8) r10) !%r12 ++
        cmpq !%r11 !%r12 ++
        jne "error" ++
        cmpq (imm 3) !%r11 ++
        je string_label ++
        movq (ind ~ofs:(-16) rax) !%r13 ++
        movq (ind ~ofs:(-16) r10) !%r14 ++
        cmpq !%r13 !%r14 ++
        setge !%al ++
        movzbq !%al r14 ++
        movq (imm 2) !%rdi ++
        call "my_malloc" ++
        movq (imm 1) (ind ~ofs:(-8) rax) ++
        movq !%r14 (ind ~ofs:(-16) rax) ++
        jmp end_label ++
        label string_label ++
        movq (ind ~ofs:(-24) rax) !%rdi ++
        movq (ind ~ofs:(-24) r10) !%rsi ++
        movq (imm 0) !%rax ++
        call "strcmp" ++
        cmpq (imm 0) !%rax ++
        setle !%al ++
        movzbq !%al r14 ++
        movq (imm 2) !%rdi ++
        call "my_malloc" ++
        movq (imm 1) (ind ~ofs:(-8) rax) ++
        movq !%r14 (ind ~ofs:(-16) rax) ++
        label end_label
      | Band ->
          let false_label = new_label () in
          let end_label = new_label () in
          compile_expr lhs ++
          movq (ind ~ofs:(-16) rax) !%rax ++
          testq !%rax !%rax ++
          jz false_label ++
          compile_expr rhs ++
          movq (ind ~ofs:(-16) rax) !%rax ++
          testq !%rax !%rax ++
          jz false_label ++
          movq (imm 2) !%rdi ++
          call "my_malloc" ++
          movq (imm 1) (ind ~ofs:(-8) rax) ++
          movq (imm 1) (ind ~ofs:(-16) rax) ++
          jmp end_label ++
          label false_label ++
          movq (imm 2) !%rdi ++
          call "my_malloc" ++
          movq (imm 1) (ind ~ofs:(-8) rax) ++
          movq (imm 0) (ind ~ofs:(-16) rax) ++
          label end_label
      | Bor ->
          let true_label = new_label () in
          let end_label = new_label () in
          compile_expr lhs ++
          movq (ind ~ofs:(-16) rax) !%rax ++
          testq !%rax !%rax ++
          jnz true_label ++
          compile_expr rhs ++
          movq (ind ~ofs:(-16) rax) !%rax ++
          testq !%rax !%rax ++
          jnz true_label ++
          movq (imm 2) !%rdi ++
          call "my_malloc" ++
          movq (imm 1) (ind ~ofs:(-8) rax) ++
          movq (imm 0) (ind ~ofs:(-16) rax) ++
          jmp end_label ++
          label true_label ++
          movq (imm 2) !%rdi ++
          call "my_malloc" ++
          movq (imm 1) (ind ~ofs:(-8) rax) ++
          movq (imm 1) (ind ~ofs:(-16) rax) ++
          label end_label
    )
  | TEunop (Uneg, e) -> failwith "Unary operations are not supported in code generation"
  | TEunop (Unot, e) -> failwith "Unary operations are not supported in code generation"
  | TEcall (fn, args) ->
      (match fn.fn_name with
      | "len" ->
          compile_expr (List.hd args) ++
          movq (ind ~ofs:(-16) rax) !%rax
      | _ ->
        failwith "Function calls are not supported in code generation")
        | TElist elements -> 
        let num_elements = List.length elements in
        movq (imm (num_elements + 2)) !%rdi ++
        call "my_malloc" ++
        movq (imm 4) (ind ~ofs:(-8) rax) ++
        movq (imm num_elements) (ind ~ofs:(-16) rax) ++
        movq !%rax !%rbx ++
        let rec compile_elements i = function
          | [] -> nop
          | e :: es -> 
              pushq !%rbx ++
              compile_expr e ++
              popq rbx ++
              movq !%rax (ind ~ofs:(-8 * (i + 3)) rbx) ++
              compile_elements (i + 1) es
        in
        compile_elements 0 elements ++
        movq !%rbx !%rax
    | TErange _ -> failwith "Range is not supported in code generation"
    | TEget (list_expr, index_expr) -> 
        compile_expr index_expr ++
        movq (ind ~ofs:(-16) rax) !%rax ++
        addq (imm 3) !%rax ++ 
        pushq !%rax ++
        compile_expr list_expr ++
        popq rbx ++
        movq !%rax !%rcx ++
        imulq (imm 8) !%rbx ++ 
        subq !%rbx !%rcx ++
        movq (ind ~ofs:0 rcx) !%rax 
  
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
  | TSreturn e -> failwith "Return statements are not supported in code generation"
  | TSassign (v, e) -> 
    let r = compile_expr e in
    current_stack_offset := !current_stack_offset - 8;
    v.v_ofs <- !current_stack_offset;
    subq (imm 8) !%rsp ++
    r ++ 
    movq !%rax (ind ~ofs:v.v_ofs rbp)
  | TSprint e -> 
    let print_none = new_label () in
    let print_bool = new_label () in
    let print_int = new_label () in
    let print_str = new_label () in
    let print_end = new_label () in
    let false_label = new_label () in
    compile_expr e ++
    movq (ind ~ofs:(-8) rax) !%r10 ++
    cmpq (imm 0) !%r10 ++
    je print_none ++
    cmpq (imm 1) !%r10 ++
    je print_bool ++
    cmpq (imm 2) !%r10 ++
    je print_int ++
    cmpq (imm 3) !%r10 ++
    je print_str ++
    cmpq (imm 4) !%r10 ++
    jne "error" ++

    label print_none ++
    leaq (lab "none_str") rdi ++
    movq (imm 0) !%rax ++
    call "printf" ++
    jmp print_end ++

    label print_bool ++
    movq (ind ~ofs:(-16) rax) !%rax ++
    testq !%rax !%rax ++
    jz false_label ++
    leaq (lab "true_str") rdi ++
    movq (imm 0) !%rax ++
    call "printf" ++
    jmp print_end ++
    X86_64.label false_label ++
    leaq (lab "false_str") rdi ++
    movq (imm 0) !%rax ++
    call "printf" ++
    jmp print_end ++

    label print_int ++
    leaq (lab "fmt_int") rdi ++
    movq (ind ~ofs:(-16) rax) !%rsi ++
    movq (imm 0) !%rax ++
    call "printf" ++
    jmp print_end ++

    label print_str ++
    leaq (lab "fmt_str") rdi ++
    movq (ind ~ofs:(-24) rax) !%rsi ++
    movq (imm 0) !%rax ++
    call "printf" ++

    label print_end 
  | TSblock stmts -> List.fold_left (++) nop (List.map compile_stmt stmts)
  | TSfor (v, collection, body) -> failwith "For loops are not supported in code generation"
  | TSeval e -> compile_expr e
  | TSset (collection, index, value) -> failwith "List assignment is not supported in code generation"

let compile_def (fn, body) =
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
  label fn.fn_name ++ code

let file ?debug:(b=false) (p: Ast.tfile) : X86_64.program =
  debug := b;
  let text_section = 
    globl "main" ++ 
    List.fold_left (++) nop (List.map compile_def p) ++
    label "error" ++
    movq (imm 1) !%rax ++
    leave ++
    ret ++
    label "my_malloc" ++
    pushq !%rbp ++
    movq !%rsp !%rbp ++
    andq (imm (-16)) !%rsp ++ 
    call "malloc" ++
    movq !%rbp !%rsp ++
    popq rbp ++
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
  { text = text_section; data = data_section ++ inline ".section .note.GNU-stack,\"\",@progbits\n" }