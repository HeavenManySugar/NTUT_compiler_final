	.text
	.globl	main
main:
	pushq %rbp
	movq %rsp, %rbp
	subq $8, %rsp
	movq $2, %rdi
	call my_malloc
	movq $2, -8(%rax)
	movq $1, -16(%rax)
	movq %rax, -8(%rbp)
	subq $8, %rsp
	movq $2, %rdi
	call my_malloc
	movq $2, -8(%rax)
	movq $5, -16(%rax)
	movq %rax, -16(%rbp)
	subq $8, %rsp
	movq $2, %rdi
	call my_malloc
	movq $2, -8(%rax)
	movq $10, -16(%rax)
	movq %rax, -24(%rbp)
	movq -16(%rbp), %rax
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
	movq -8(%rbp), %rax
	movq -8(%rax), %r10
	cmpq $0, %r10
	je L6
	cmpq $1, %r10
	je L7
	cmpq $2, %r10
	je L8
	cmpq $3, %r10
	je L9
	cmpq $4, %r10
	jne error
L6:
	leaq none_str, %rdi
	movq $0, %rax
	call printf
	jmp L10
L7:
	movq -16(%rax), %rax
	testq %rax, %rax
	jz L11
	leaq true_str, %rdi
	movq $0, %rax
	call printf
	jmp L10
L11:
	leaq false_str, %rdi
	movq $0, %rax
	call printf
	jmp L10
L8:
	leaq fmt_int, %rdi
	movq -16(%rax), %rsi
	movq $0, %rax
	call printf
	jmp L10
L9:
	leaq fmt_str, %rdi
	movq -24(%rax), %rsi
	movq $0, %rax
	call printf
L10:
	movq -24(%rbp), %rax
	movq -8(%rax), %r10
	cmpq $0, %r10
	je L12
	cmpq $1, %r10
	je L13
	cmpq $2, %r10
	je L14
	cmpq $3, %r10
	je L15
	cmpq $4, %r10
	jne error
L12:
	leaq none_str, %rdi
	movq $0, %rax
	call printf
	jmp L16
L13:
	movq -16(%rax), %rax
	testq %rax, %rax
	jz L17
	leaq true_str, %rdi
	movq $0, %rax
	call printf
	jmp L16
L17:
	leaq false_str, %rdi
	movq $0, %rax
	call printf
	jmp L16
L14:
	leaq fmt_int, %rdi
	movq -16(%rax), %rsi
	movq $0, %rax
	call printf
	jmp L16
L15:
	leaq fmt_str, %rdi
	movq -24(%rax), %rsi
	movq $0, %rax
	call printf
L16:
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
