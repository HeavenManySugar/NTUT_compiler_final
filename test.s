	.text
	.globl	main
main:
	pushq %rbp
	movq %rsp, %rbp
	subq $8, %rsp
	movq $5, %rdi
	call my_malloc
	movq $4, -8(%rax)
	movq $3, -16(%rax)
	movq %rax, %rbx
	movq $2, %rdi
	call my_malloc
	movq $2, -8(%rax)
	movq $111, -16(%rax)
	movq %rax, -24(%rbx)
	movq $2, %rdi
	call my_malloc
	movq $2, -8(%rax)
	movq $222, -16(%rax)
	movq %rax, -32(%rbx)
	movq $2, %rdi
	call my_malloc
	movq $2, -8(%rax)
	movq $333, -16(%rax)
	movq %rax, -40(%rbx)
	movq %rbx, %rax
	movq %rax, -8(%rbp)
	movq $2, %rdi
	call my_malloc
	movq $2, -8(%rax)
	movq $2, -16(%rax)
	movq -16(%rax), %rax
	addq $3, %rax
	pushq %rax
	movq -8(%rbp), %rax
	popq %rbx
	movq %rax, %rcx
	imulq $8, %rbx
	subq %rbx, %rcx
	movq 0(%rcx), %rax
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
	.string "%d\n"
fmt_str:
	.string "%s\n"
true_str:
	.string "True\n"
false_str:
	.string "False\n"
none_str:
	.string "None\n"
.section .note.GNU-stack,"",@progbits
