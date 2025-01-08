	.text
	.globl	main
main:
	pushq %rbp
	movq %rsp, %rbp
	subq $16, %rsp
	subq $16, %rsp
	movq $1, -24(%rbp)
	movq $1, %rax
	movq %rax, -32(%rbp)
	movq %rbp, %rax
	addq $-24, %rax
	movq %rax, -8(%rbp)
	subq $16, %rsp
	movq $1, -40(%rbp)
	movq $1, %rax
	movq %rax, -48(%rbp)
	movq %rbp, %rax
	addq $-40, %rax
	movq %rax, -16(%rbp)
	movq -8(%rbp), %rsi
	movq -16(%rbp), %rdi
	movq $0, %rax
	call compare_lists
	testq %rax, %rax
	jz L3
	movq $1, %rax
	jmp L4
L3:
	movq $0, %rax
L4:
	testq %rax, %rax
	jz L1
	leaq true_str, %rdi
	movq $0, %rax
	call printf
	jmp L2
L1:
	leaq false_str, %rdi
	movq $0, %rax
	call printf
L2:
	subq $16, %rsp
	subq $16, %rsp
	movq $1, -72(%rbp)
	movq $1, %rax
	movq %rax, -80(%rbp)
	movq %rbp, %rax
	addq $-72, %rax
	movq %rax, -56(%rbp)
	subq $16, %rsp
	movq $1, -88(%rbp)
	movq $2, %rax
	movq %rax, -96(%rbp)
	movq %rbp, %rax
	addq $-88, %rax
	movq %rax, -64(%rbp)
	movq -56(%rbp), %rsi
	movq -64(%rbp), %rdi
	movq $0, %rax
	call compare_lists
	testq %rax, %rax
	jz L7
	movq $1, %rax
	jmp L8
L7:
	movq $0, %rax
L8:
	testq %rax, %rax
	jz L5
	leaq true_str, %rdi
	movq $0, %rax
	call printf
	jmp L6
L5:
	leaq false_str, %rdi
	movq $0, %rax
	call printf
L6:
	subq $16, %rsp
	subq $24, %rsp
	movq $2, -120(%rbp)
	movq $1, %rax
	movq %rax, -128(%rbp)
	movq $2, %rax
	movq %rax, -136(%rbp)
	movq %rbp, %rax
	addq $-120, %rax
	movq %rax, -104(%rbp)
	subq $16, %rsp
	movq $1, -144(%rbp)
	movq $1, %rax
	movq %rax, -152(%rbp)
	movq %rbp, %rax
	addq $-144, %rax
	movq %rax, -112(%rbp)
	movq -104(%rbp), %rsi
	movq -112(%rbp), %rdi
	movq $0, %rax
	call compare_lists
	testq %rax, %rax
	jz L11
	movq $1, %rax
	jmp L12
L11:
	movq $0, %rax
L12:
	testq %rax, %rax
	jz L9
	leaq true_str, %rdi
	movq $0, %rax
	call printf
	jmp L10
L9:
	leaq false_str, %rdi
	movq $0, %rax
	call printf
L10:
	subq $16, %rsp
	subq $24, %rsp
	movq $2, -176(%rbp)
	movq $1, %rax
	movq %rax, -184(%rbp)
	movq $2, %rax
	movq %rax, -192(%rbp)
	movq %rbp, %rax
	addq $-176, %rax
	movq %rax, -160(%rbp)
	subq $24, %rsp
	movq $2, -200(%rbp)
	movq $1, %rax
	movq %rax, -208(%rbp)
	movq $2, %rax
	movq %rax, -216(%rbp)
	movq %rbp, %rax
	addq $-200, %rax
	movq %rax, -168(%rbp)
	movq -160(%rbp), %rsi
	movq -168(%rbp), %rdi
	movq $0, %rax
	call compare_lists
	testq %rax, %rax
	jz L15
	movq $1, %rax
	jmp L16
L15:
	movq $0, %rax
L16:
	testq %rax, %rax
	jz L13
	leaq true_str, %rdi
	movq $0, %rax
	call printf
	jmp L14
L13:
	leaq false_str, %rdi
	movq $0, %rax
	call printf
L14:
	subq $16, %rsp
	subq $16, %rsp
	movq $1, -240(%rbp)
	movq $1, %rax
	movq %rax, -248(%rbp)
	movq %rbp, %rax
	addq $-240, %rax
	movq %rax, -224(%rbp)
	subq $16, %rsp
	movq $1, -256(%rbp)
	movq $1, %rax
	movq %rax, -264(%rbp)
	movq %rbp, %rax
	addq $-256, %rax
	movq %rax, -232(%rbp)
	movq -224(%rbp), %rsi
	movq -232(%rbp), %rdi
	movq $0, %rax
	call compare_lists
	testq %rax, %rax
	jnz L19
	movq $1, %rax
	jmp L20
L19:
	movq $0, %rax
L20:
	testq %rax, %rax
	jz L17
	leaq true_str, %rdi
	movq $0, %rax
	call printf
	jmp L18
L17:
	leaq false_str, %rdi
	movq $0, %rax
	call printf
L18:
	subq $16, %rsp
	subq $16, %rsp
	movq $1, -288(%rbp)
	movq $1, %rax
	movq %rax, -296(%rbp)
	movq %rbp, %rax
	addq $-288, %rax
	movq %rax, -272(%rbp)
	subq $16, %rsp
	movq $1, -304(%rbp)
	movq $2, %rax
	movq %rax, -312(%rbp)
	movq %rbp, %rax
	addq $-304, %rax
	movq %rax, -280(%rbp)
	movq -272(%rbp), %rsi
	movq -280(%rbp), %rdi
	movq $0, %rax
	call compare_lists
	testq %rax, %rax
	jnz L23
	movq $1, %rax
	jmp L24
L23:
	movq $0, %rax
L24:
	testq %rax, %rax
	jz L21
	leaq true_str, %rdi
	movq $0, %rax
	call printf
	jmp L22
L21:
	leaq false_str, %rdi
	movq $0, %rax
	call printf
L22:
	subq $16, %rsp
	subq $24, %rsp
	movq $2, -336(%rbp)
	movq $1, %rax
	movq %rax, -344(%rbp)
	movq $2, %rax
	movq %rax, -352(%rbp)
	movq %rbp, %rax
	addq $-336, %rax
	movq %rax, -320(%rbp)
	subq $16, %rsp
	movq $1, -360(%rbp)
	movq $1, %rax
	movq %rax, -368(%rbp)
	movq %rbp, %rax
	addq $-360, %rax
	movq %rax, -328(%rbp)
	movq -320(%rbp), %rsi
	movq -328(%rbp), %rdi
	movq $0, %rax
	call compare_lists
	testq %rax, %rax
	jnz L27
	movq $1, %rax
	jmp L28
L27:
	movq $0, %rax
L28:
	testq %rax, %rax
	jz L25
	leaq true_str, %rdi
	movq $0, %rax
	call printf
	jmp L26
L25:
	leaq false_str, %rdi
	movq $0, %rax
	call printf
L26:
	subq $16, %rsp
	subq $24, %rsp
	movq $2, -392(%rbp)
	movq $1, %rax
	movq %rax, -400(%rbp)
	movq $2, %rax
	movq %rax, -408(%rbp)
	movq %rbp, %rax
	addq $-392, %rax
	movq %rax, -376(%rbp)
	subq $24, %rsp
	movq $2, -416(%rbp)
	movq $1, %rax
	movq %rax, -424(%rbp)
	movq $2, %rax
	movq %rax, -432(%rbp)
	movq %rbp, %rax
	addq $-416, %rax
	movq %rax, -384(%rbp)
	movq -376(%rbp), %rsi
	movq -384(%rbp), %rdi
	movq $0, %rax
	call compare_lists
	testq %rax, %rax
	jnz L31
	movq $1, %rax
	jmp L32
L31:
	movq $0, %rax
L32:
	testq %rax, %rax
	jz L29
	leaq true_str, %rdi
	movq $0, %rax
	call printf
	jmp L30
L29:
	leaq false_str, %rdi
	movq $0, %rax
	call printf
L30:
	movq $0, %rax
	leave
	ret
error:
	leave
compare_lists:
	pushq %rbp
	movq %rsp, %rbp
	movq 0(%rsi), %rcx
	movq 0(%rdi), %rdx
	cmpq %rcx, %rdx
	jne L0
	movq $1, %rax
	incq %rcx
compare_elements:
	cmpq %rax, %rcx
	je end_compare
	movq %rax, %rbx
	imulq $8, %rbx
	movq %rsi, %rdx
	subq %rbx, %rdx
	movq 0(%rdx), %r8
	movq %rdi, %rdx
	subq %rbx, %rdx
	movq 0(%rdx), %r9
	cmpq %r8, %r9
	jne L0
	incq %rax
	jmp compare_elements
end_compare:
	movq $1, %rax
	leave
	ret
L0:
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
