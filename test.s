	.text
	.globl	main
str:
	pushq %rbp
	movq %rsp, %rbp
	movq 16(%rbp), %rax
	movq %rax, -8(%rbp)
	subq $8, %rsp
	subq $88, %rsp
	movq $10, -16(%rbp)
	movq $1, %rax
	movq %rax, -24(%rbp)
	movq $1, %rax
	movq %rax, -32(%rbp)
	movq $1, %rax
	movq %rax, -40(%rbp)
	movq $1, %rax
	movq %rax, -48(%rbp)
	movq $1, %rax
	movq %rax, -56(%rbp)
	movq $1, %rax
	movq %rax, -64(%rbp)
	movq $1, %rax
	movq %rax, -72(%rbp)
	movq $1, %rax
	movq %rax, -80(%rbp)
	movq $1, %rax
	movq %rax, -88(%rbp)
	movq $1, %rax
	movq %rax, -96(%rbp)
	movq %rbp, %rax
	addq $-16, %rax
	movq %rax, -8(%rbp)
	movq $10, %rax
	pushq %rax
	movq -8(%rbp), %rax
	popq %rbx
	cmpq %rbx, %rax
	setl %al
	movzbq %al, %rax
	testq %rax, %rax
	jz L0
	movq -8(%rbp), %rax
	addq $1, %rax
	pushq %rax
	movq -8(%rbp), %rax
	popq %rbx
	movq %rax, %rcx
	imulq $8, %rbx
	subq %rbx, %rcx
	movq 0(%rcx), %rax
	jmp L1
L0:
L1:
	leave
	ret
main:
	pushq %rbp
	movq %rsp, %rbp
	leaq none_str, %rdi
	movq $0, %rax
	call printf
	movq $0, %rax
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
none_str:
	.string "None\n"
