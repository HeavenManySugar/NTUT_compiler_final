	.text
	.globl	main
main:
	pushq %rbp
	movq %rsp, %rbp
	pushq %rdi
	movq $2, %rdi
	call my_malloc
	movq $2, -8(%rax)
	movq $10, -16(%rax)
	popq %rdi
	movq -8(%rax), %r10
	cmpq $2, %r10
	jne error
	movq -16(%rax), %r10
	movq $2, %rdi
	call my_malloc
	movq $5, -8(%rax)
	movq %r10, -16(%rax)
	movq -16(%rax), %r10
	movq $2, %r11
	movq %r11, %rdi
	call my_malloc
	movq $4, -8(%rax)
	movq %r10, -16(%rax)
	movq $0, %r11
L15:
	cmpq %r11, %r10
	jle L14
	movq %rax, %r12
	movq %r11, %r13
	addq $3, %r13
	imulq $8, %r13
	subq %r13, %r12
	pushq %r11
	pushq %r12
	pushq %r13
	pushq %rax
	movq $2, %rdi
	call my_malloc
	movq %rax, %rbx
	popq %rax
	popq %r13
	popq %r12
	popq %r11
	movq $2, -8(%rbx)
	movq %r11, -16(%rbx)
	movq %rbx, 0(%r12)
	incq %r11
	jmp L15
L14:
	movq -8(%rax), %r10
	cmpq $0, %r10
	je L8
	cmpq $1, %r10
	je L9
	cmpq $2, %r10
	je L10
	cmpq $3, %r10
	je L11
	cmpq $4, %r10
	jne L10
	call print_list
	jmp L12
L8:
	leaq none_str, %rdi
	movq $0, %rax
	call printf
	jmp L12
L9:
	movq -16(%rax), %rax
	testq %rax, %rax
	jz L13
	leaq true_str, %rdi
	movq $0, %rax
	call printf
	jmp L12
L13:
	leaq false_str, %rdi
	movq $0, %rax
	call printf
	jmp L12
L10:
	leaq fmt_int, %rdi
	movq -16(%rax), %rsi
	movq $0, %rax
	call printf
	jmp L12
L11:
	leaq fmt_str, %rdi
	movq -24(%rax), %rsi
	movq $0, %rax
	call printf
L12:
	leaq endl_str, %rdi
	movq $0, %rax
	call printf
	movq $0, %rax
	leave
	ret
error:
	movq $1, %rax
	leave
	ret
my_malloc:
	pushq %rbp
	movq %rsp, %rbp
	andq $-16, %rsp
	call malloc
	movq %rbp, %rsp
	popq %rbp
	ret
print_list:
	pushq %rbp
	movq %rsp, %rbp
	pushq %rax
	leaq open_bracket, %rdi
	movq $0, %rax
	call printf
	popq %rax
	movq -16(%rax), %r10
	movq $0, %r11
L0:
	cmpq %r10, %r11
	jge L1
	movq %r11, %r12
	addq $3, %r12
	imulq $8, %r12
	movq %rax, %r13
	subq %r12, %r13
	movq 0(%r13), %r12
	movq -8(%r12), %rsi
	cmpq $0, %rsi
	je L2
	cmpq $1, %rsi
	je L3
	cmpq $2, %rsi
	je L5
	cmpq $3, %rsi
	je L6
	cmpq $4, %rsi
	jne L5
	pushq %rax
	pushq %r10
	pushq %r11
	movq %r12, %rax
	call print_list
	popq %r11
	popq %r10
	popq %rax
	jmp L7
L2:
	leaq none_str, %rdi
	pushq %rax
	pushq %r10
	pushq %r11
	movq $0, %rax
	call printf
	popq %r11
	popq %r10
	popq %rax
	jmp L7
L3:
	pushq %rax
	pushq %r10
	pushq %r11
	movq -16(%r12), %rax
	testq %rax, %rax
	jz L4
	leaq true_str, %rdi
	movq $0, %rax
	call printf
	popq %r11
	popq %r10
	popq %rax
	jmp L7
L4:
	leaq false_str, %rdi
	movq $0, %rax
	call printf
	popq %r11
	popq %r10
	popq %rax
	jmp L7
L5:
	movq -16(%r12), %rsi
	leaq fmt_int, %rdi
	pushq %rax
	pushq %r10
	pushq %r11
	movq $0, %rax
	call printf
	popq %r11
	popq %r10
	popq %rax
	jmp L7
L6:
	movq -24(%r12), %rsi
	leaq fmt_str, %rdi
	pushq %rax
	pushq %r10
	pushq %r11
	movq $0, %rax
	call printf
	popq %r11
	popq %r10
	popq %rax
	jmp L7
L7:
	movq %r11, %r12
	addq $1, %r12
	cmpq %r12, %r10
	je L1
	leaq comma, %rdi
	pushq %rax
	pushq %r10
	pushq %r11
	movq $0, %rax
	call printf
	popq %r11
	popq %r10
	popq %rax
	addq $1, %r11
	jmp L0
L1:
	leaq close_bracket, %rdi
	movq $0, %rax
	pushq %rax
	call printf
	popq %rax
	leave
	ret
	.data
fmt_int:
	.string "%d"
fmt_str:
	.string "%s"
true_str:
	.string "True"
false_str:
	.string "False"
none_str:
	.string "None"
open_bracket:
	.string "["
close_bracket:
	.string "]"
endl_str:
	.string "\n"
comma:
	.string ", "
.section .note.GNU-stack,"",@progbits
