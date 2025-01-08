	.text
	.globl	main
main:
	pushq %rbp
	movq %rsp, %rbp
	jmp error
	movq %rax, %rsi
	leaq fmt_int, %rdi
	movq $0, %rax
	call printf
	movq $0, %rax
	leave
	ret
error:
	leave
	movq $1, %rax
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
