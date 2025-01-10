	.text
	.globl	main
main:
	pushq %rbp
	movq %rsp, %rbp
	movq $2, %rdi
	call my_malloc
	movq $1, -8(%rax)
	movq $1, -16(%rax)
	pushq %rax
	movq $2, %rdi
	call my_malloc
	movq $1, -8(%rax)
	movq $0, -16(%rax)
	popq %r10
	movq -16(%rax), %r11
	movq -16(%r10), %r12
	cmpq %r11, %r12
	sete %al
	movzbq %al, %r14
	movq $2, %rdi
	call my_malloc
	movq $1, -8(%rax)
	movq %r14, -16(%rax)
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
	movq $2, %rdi
	call my_malloc
	movq $1, -8(%rax)
	movq $1, -16(%rax)
	pushq %rax
	movq $2, %rdi
	call my_malloc
	movq $1, -8(%rax)
	movq $0, -16(%rax)
	popq %r10
	movq -16(%rax), %r11
	movq -16(%r10), %r12
	cmpq %r11, %r12
	setne %al
	movzbq %al, %r14
	movq $2, %rdi
	call my_malloc
	movq $1, -8(%rax)
	movq %r14, -16(%rax)
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
	movq $2, %rdi
	call my_malloc
	movq $1, -8(%rax)
	movq $1, -16(%rax)
	pushq %rax
	movq $2, %rdi
	call my_malloc
	movq $1, -8(%rax)
	movq $0, -16(%rax)
	popq %r10
	movq -16(%rax), %r11
	movq -16(%r10), %r12
	cmpq %r11, %r12
	setl %al
	movzbq %al, %r14
	movq $2, %rdi
	call my_malloc
	movq $1, -8(%rax)
	movq %r14, -16(%rax)
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
	movq $2, %rdi
	call my_malloc
	movq $1, -8(%rax)
	movq $1, -16(%rax)
	pushq %rax
	movq $2, %rdi
	call my_malloc
	movq $1, -8(%rax)
	movq $0, -16(%rax)
	popq %r10
	movq -16(%rax), %r11
	movq -16(%r10), %r12
	cmpq %r11, %r12
	setle %al
	movzbq %al, %r14
	movq $2, %rdi
	call my_malloc
	movq $1, -8(%rax)
	movq %r14, -16(%rax)
	movq -8(%rax), %r10
	cmpq $0, %r10
	je L18
	cmpq $1, %r10
	je L19
	cmpq $2, %r10
	je L20
	cmpq $3, %r10
	je L21
	cmpq $4, %r10
	jne error
L18:
	leaq none_str, %rdi
	movq $0, %rax
	call printf
	jmp L22
L19:
	movq -16(%rax), %rax
	testq %rax, %rax
	jz L23
	leaq true_str, %rdi
	movq $0, %rax
	call printf
	jmp L22
L23:
	leaq false_str, %rdi
	movq $0, %rax
	call printf
	jmp L22
L20:
	leaq fmt_int, %rdi
	movq -16(%rax), %rsi
	movq $0, %rax
	call printf
	jmp L22
L21:
	leaq fmt_str, %rdi
	movq -24(%rax), %rsi
	movq $0, %rax
	call printf
L22:
	movq $2, %rdi
	call my_malloc
	movq $1, -8(%rax)
	movq $1, -16(%rax)
	pushq %rax
	movq $2, %rdi
	call my_malloc
	movq $1, -8(%rax)
	movq $0, -16(%rax)
	popq %r10
	movq -16(%rax), %r11
	movq -16(%r10), %r12
	cmpq %r11, %r12
	setg %al
	movzbq %al, %r14
	movq $2, %rdi
	call my_malloc
	movq $1, -8(%rax)
	movq %r14, -16(%rax)
	movq -8(%rax), %r10
	cmpq $0, %r10
	je L24
	cmpq $1, %r10
	je L25
	cmpq $2, %r10
	je L26
	cmpq $3, %r10
	je L27
	cmpq $4, %r10
	jne error
L24:
	leaq none_str, %rdi
	movq $0, %rax
	call printf
	jmp L28
L25:
	movq -16(%rax), %rax
	testq %rax, %rax
	jz L29
	leaq true_str, %rdi
	movq $0, %rax
	call printf
	jmp L28
L29:
	leaq false_str, %rdi
	movq $0, %rax
	call printf
	jmp L28
L26:
	leaq fmt_int, %rdi
	movq -16(%rax), %rsi
	movq $0, %rax
	call printf
	jmp L28
L27:
	leaq fmt_str, %rdi
	movq -24(%rax), %rsi
	movq $0, %rax
	call printf
L28:
	movq $2, %rdi
	call my_malloc
	movq $1, -8(%rax)
	movq $1, -16(%rax)
	pushq %rax
	movq $2, %rdi
	call my_malloc
	movq $1, -8(%rax)
	movq $0, -16(%rax)
	popq %r10
	movq -16(%rax), %r11
	movq -16(%r10), %r12
	cmpq %r11, %r12
	setge %al
	movzbq %al, %r14
	movq $2, %rdi
	call my_malloc
	movq $1, -8(%rax)
	movq %r14, -16(%rax)
	movq -8(%rax), %r10
	cmpq $0, %r10
	je L30
	cmpq $1, %r10
	je L31
	cmpq $2, %r10
	je L32
	cmpq $3, %r10
	je L33
	cmpq $4, %r10
	jne error
L30:
	leaq none_str, %rdi
	movq $0, %rax
	call printf
	jmp L34
L31:
	movq -16(%rax), %rax
	testq %rax, %rax
	jz L35
	leaq true_str, %rdi
	movq $0, %rax
	call printf
	jmp L34
L35:
	leaq false_str, %rdi
	movq $0, %rax
	call printf
	jmp L34
L32:
	leaq fmt_int, %rdi
	movq -16(%rax), %rsi
	movq $0, %rax
	call printf
	jmp L34
L33:
	leaq fmt_str, %rdi
	movq -24(%rax), %rsi
	movq $0, %rax
	call printf
L34:
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
