	.text
	.globl	main
main:
	pushq %rbp
	movq %rsp, %rbp
	subq $8, %rsp
	subq $32, %rsp
	movq $3, -24(%rbp)
	movq $1, %rax
	movq $1, %r10
	movq %rax, -32(%rbp)
	movq $2, %rax
	movq $1, %r10
	movq %rax, -40(%rbp)
	movq $3, %rax
	movq $1, %r10
	movq %rax, -48(%rbp)
	movq %rbp, %rax
	addq $-24, %rax
	movq $3, %r10
	movq %rax, -8(%rbp)
	movq $3, -16(%rbp)
	movq $4, %r10
	subq $8, %rsp
	subq $32, %rsp
	movq $3, -72(%rbp)
	movq -8(%rbp), %rax
	movq -16(%rbp), %r10
	movq %rax, -80(%rbp)
	movq -8(%rbp), %rax
	movq -16(%rbp), %r10
	movq %rax, -88(%rbp)
	movq -8(%rbp), %rax
	movq -16(%rbp), %r10
	movq %rax, -96(%rbp)
	movq %rbp, %rax
	addq $-72, %rax
	movq $3, %r10
	movq %rax, -56(%rbp)
	movq $3, -64(%rbp)
	movq $4, %r10
	movq -56(%rbp), %rax
	movq -64(%rbp), %r10
	movq %rax, %rsi
	movq $0, %rax
	cmpq $0, %r10
	je L0
	cmpq $1, %r10
	je L1
	cmpq $2, %r10
	je L2
	jmp L3
L0:
	movq $fmt_str, %rdi
	jmp L4
L1:
	movq $fmt_int, %rdi
	jmp L4
L2:
	cmpq $1, %rsi
	je L5
	movq $false_str, %rdi
	jmp L4
L5:
	movq $true_str, %rdi
	jmp L4
L3:
	movq $none_str, %rdi
L4:
	call printf
	movq $1, %rax
	movq $1, %r10
	addq $1, %rax
	pushq %rax
	movq -56(%rbp), %rax
	movq -64(%rbp), %r10
	popq %rbx
	movq %rax, %rcx
	imulq $8, %rbx
	subq %rbx, %rcx
	movq 0(%rcx), %rax
	movq $1, %rax
	movq $1, %r10
	movq $42, %rax
	movq $1, %r10
	movq -56(%rbp), %rax
	movq -64(%rbp), %r10
	movq %rax, %rsi
	movq $0, %rax
	cmpq $0, %r10
	je L6
	cmpq $1, %r10
	je L7
	cmpq $2, %r10
	je L8
	jmp L9
L6:
	movq $fmt_str, %rdi
	jmp L10
L7:
	movq $fmt_int, %rdi
	jmp L10
L8:
	cmpq $1, %rsi
	je L11
	movq $false_str, %rdi
	jmp L10
L11:
	movq $true_str, %rdi
	jmp L10
L9:
	movq $none_str, %rdi
L10:
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
