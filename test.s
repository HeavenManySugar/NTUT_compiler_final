	.text
	.globl	main
main:
	pushq %rbp
	movq %rsp, %rbp
	subq $8, %rsp
	subq $32, %rsp
	movq $3, -16(%rbp)
	movq $1, %rax
	movq %rax, -24(%rbp)
	subq $24, %rsp
	movq $2, -48(%rbp)
	movq $1, %rax
	movq %rax, -56(%rbp)
	movq $2, %rax
	movq %rax, -64(%rbp)
	movq %rbp, %rax
	addq $-48, %rax
	movq %rax, -32(%rbp)
	movq $3, %rax
	movq %rax, -40(%rbp)
	movq %rbp, %rax
	addq $-16, %rax
	movq %rax, -8(%rbp)
	movq $1, %rax
	addq $1, %rax
	pushq %rax
	movq $1, %rax
	addq $1, %rax
	pushq %rax
	movq -8(%rbp), %rax
	popq %rbx
	movq %rax, %rcx
	imulq $8, %rbx
	subq %rbx, %rcx
	movq 0(%rcx), %rax
	popq %rbx
	movq %rax, %rcx
	imulq $8, %rbx
	subq %rbx, %rcx
	movq 0(%rcx), %rax
	movq %rax, %rsi
	leaq fmt_int, %rdi
	movq $0, %rax
	call printf
	movl $0, %eax
	leave
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
