	.text
	.globl	main
main:
	pushq %rbp
	movq %rsp, %rbp
	movq $5, %rdi
	call my_malloc
	movq $4, -8(%rax)
	movq $3, -16(%rax)
	movq %rax, %rbx
	pushq %rbx
	movq $2, %rdi
	call my_malloc
	movq $2, -8(%rax)
	movq $1, -16(%rax)
	popq %rbx
	movq %rax, -24(%rbx)
	pushq %rbx
	movq $2, %rdi
	call my_malloc
	movq $2, -8(%rax)
	movq $2, -16(%rax)
	popq %rbx
	movq %rax, -32(%rbx)
	pushq %rbx
	movq $2, %rdi
	call my_malloc
	movq $2, -8(%rax)
	movq $3, -16(%rax)
	popq %rbx
	movq %rax, -40(%rbx)
	movq %rbx, %rax
	movq -8(%rax), %r10
	cmpq $0, %r10
	je L0
	cmpq $1, %r10
	je L1
	cmpq $2, %r10
	je L2
	cmpq $3, %r10
	je L3
	cmpq $4, %r10
	jne error
	pushq %rax
	leaq open_bracket, %rdi
	movq $0, %rax
	call printf
	popq %rax
	movq -16(%rax), %r10
	movq $0, %r11
L6:
	cmpq %r10, %r11
	jge L7
	movq %r11, %r12
	addq $3, %r12
	imulq $8, %r12
	movq %rax, %r13
	subq %r12, %r13
	movq 0(%r13), %r12
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
	movq %r11, %r12
	addq $1, %r12
	cmpq %r12, %r10
	je L7
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
	jmp L6
L7:
	leaq close_bracket, %rdi
	movq $0, %rax
	call printf
	jmp L4
L0:
	leaq none_str, %rdi
	movq $0, %rax
	call printf
	jmp L4
L1:
	movq -16(%rax), %rax
	testq %rax, %rax
	jz L5
	leaq true_str, %rdi
	movq $0, %rax
	call printf
	jmp L4
L5:
	leaq false_str, %rdi
	movq $0, %rax
	call printf
	jmp L4
L2:
	leaq fmt_int, %rdi
	movq -16(%rax), %rsi
	movq $0, %rax
	call printf
	jmp L4
L3:
	leaq fmt_str, %rdi
	movq -24(%rax), %rsi
	movq $0, %rax
	call printf
L4:
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
