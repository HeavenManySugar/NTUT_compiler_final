	.text
	.globl	main
main:
	pushq %rbp
	movq %rsp, %rbp
	movq $3, %rdi
	call my_malloc
	movq $3, 0(%rax)
	movq $13, -8(%rax)
	leaq L5, %rbx
	movq %rbx, -16(%rax)
	movq 0(%rax), %r10
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
	leaq true_str, %rdi
	movq $0, %rax
	call printf
	jmp L4
L2:
	leaq fmt_int, %rdi
	movq -8(%rax), %rsi
	movq $0, %rax
	call printf
	jmp L4
L3:
	leaq fmt_str, %rdi
	movq -16(%rax), %rsi
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
L5:
	.string "Hello, World!"
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
