	.text
	.globl	main
main:
	pushq %rbp
	movq %rsp, %rbp
	movq $3, %rdi
	call my_malloc
	movq $3, -8(%rax)
	movq $0, -16(%rax)
	leaq L9, %rbx
	movq %rbx, -24(%rax)
	pushq %rax
	movq $3, %rdi
	call my_malloc
	movq $3, -8(%rax)
	movq $0, -16(%rax)
	leaq L8, %rbx
	movq %rbx, -24(%rax)
	popq %r10
	movq -8(%rax), %r11
	movq -8(%r10), %r12
	cmpq %r11, %r12
	jne error
	cmpq $3, %r11
	je L6
	movq -16(%rax), %r13
	movq -16(%r10), %r14
	cmpq %r13, %r14
	sete %al
	movzbq %al, %r14
	movq $2, %rdi
	call my_malloc
	movq $1, -8(%rax)
	movq %r14, -16(%rax)
	jmp L7
L6:
	movq -24(%rax), %rdi
	movq -24(%r10), %rsi
	movq $0, %rax
	call strcmp
	cmpq $0, %rax
	sete %al
	movzbq %al, %r14
	movq $2, %rdi
	call my_malloc
	movq $1, -8(%rax)
	movq %r14, -16(%rax)
L7:
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
	movq $3, %rdi
	call my_malloc
	movq $3, -8(%rax)
	movq $0, -16(%rax)
	leaq L19, %rbx
	movq %rbx, -24(%rax)
	pushq %rax
	movq $3, %rdi
	call my_malloc
	movq $3, -8(%rax)
	movq $1, -16(%rax)
	leaq L18, %rbx
	movq %rbx, -24(%rax)
	popq %r10
	movq -8(%rax), %r11
	movq -8(%r10), %r12
	cmpq %r11, %r12
	jne error
	cmpq $3, %r11
	je L16
	movq -16(%rax), %r13
	movq -16(%r10), %r14
	cmpq %r13, %r14
	setl %al
	movzbq %al, %r14
	movq $2, %rdi
	call my_malloc
	movq $1, -8(%rax)
	movq %r14, -16(%rax)
	jmp L17
L16:
	movq -24(%rax), %rdi
	movq -24(%r10), %rsi
	movq $0, %rax
	call strcmp
	cmpq $0, %rax
	setg %al
	movzbq %al, %r14
	movq $2, %rdi
	call my_malloc
	movq $1, -8(%rax)
	movq %r14, -16(%rax)
L17:
	movq -8(%rax), %r10
	cmpq $0, %r10
	je L10
	cmpq $1, %r10
	je L11
	cmpq $2, %r10
	je L12
	cmpq $3, %r10
	je L13
	cmpq $4, %r10
	jne error
L10:
	leaq none_str, %rdi
	movq $0, %rax
	call printf
	jmp L14
L11:
	movq -16(%rax), %rax
	testq %rax, %rax
	jz L15
	leaq true_str, %rdi
	movq $0, %rax
	call printf
	jmp L14
L15:
	leaq false_str, %rdi
	movq $0, %rax
	call printf
	jmp L14
L12:
	leaq fmt_int, %rdi
	movq -16(%rax), %rsi
	movq $0, %rax
	call printf
	jmp L14
L13:
	leaq fmt_str, %rdi
	movq -24(%rax), %rsi
	movq $0, %rax
	call printf
L14:
	movq $3, %rdi
	call my_malloc
	movq $3, -8(%rax)
	movq $1, -16(%rax)
	leaq L29, %rbx
	movq %rbx, -24(%rax)
	pushq %rax
	movq $3, %rdi
	call my_malloc
	movq $3, -8(%rax)
	movq $1, -16(%rax)
	leaq L28, %rbx
	movq %rbx, -24(%rax)
	popq %r10
	movq -8(%rax), %r11
	movq -8(%r10), %r12
	cmpq %r11, %r12
	jne error
	cmpq $3, %r11
	je L26
	movq -16(%rax), %r13
	movq -16(%r10), %r14
	cmpq %r13, %r14
	setl %al
	movzbq %al, %r14
	movq $2, %rdi
	call my_malloc
	movq $1, -8(%rax)
	movq %r14, -16(%rax)
	jmp L27
L26:
	movq -24(%rax), %rdi
	movq -24(%r10), %rsi
	movq $0, %rax
	call strcmp
	cmpq $0, %rax
	setg %al
	movzbq %al, %r14
	movq $2, %rdi
	call my_malloc
	movq $1, -8(%rax)
	movq %r14, -16(%rax)
L27:
	movq -8(%rax), %r10
	cmpq $0, %r10
	je L20
	cmpq $1, %r10
	je L21
	cmpq $2, %r10
	je L22
	cmpq $3, %r10
	je L23
	cmpq $4, %r10
	jne error
L20:
	leaq none_str, %rdi
	movq $0, %rax
	call printf
	jmp L24
L21:
	movq -16(%rax), %rax
	testq %rax, %rax
	jz L25
	leaq true_str, %rdi
	movq $0, %rax
	call printf
	jmp L24
L25:
	leaq false_str, %rdi
	movq $0, %rax
	call printf
	jmp L24
L22:
	leaq fmt_int, %rdi
	movq -16(%rax), %rsi
	movq $0, %rax
	call printf
	jmp L24
L23:
	leaq fmt_str, %rdi
	movq -24(%rax), %rsi
	movq $0, %rax
	call printf
L24:
	movq $3, %rdi
	call my_malloc
	movq $3, -8(%rax)
	movq $2, -16(%rax)
	leaq L39, %rbx
	movq %rbx, -24(%rax)
	pushq %rax
	movq $3, %rdi
	call my_malloc
	movq $3, -8(%rax)
	movq $2, -16(%rax)
	leaq L38, %rbx
	movq %rbx, -24(%rax)
	popq %r10
	movq -8(%rax), %r11
	movq -8(%r10), %r12
	cmpq %r11, %r12
	jne error
	cmpq $3, %r11
	je L36
	movq -16(%rax), %r13
	movq -16(%r10), %r14
	cmpq %r13, %r14
	setge %al
	movzbq %al, %r14
	movq $2, %rdi
	call my_malloc
	movq $1, -8(%rax)
	movq %r14, -16(%rax)
	jmp L37
L36:
	movq -24(%rax), %rdi
	movq -24(%r10), %rsi
	movq $0, %rax
	call strcmp
	cmpq $0, %rax
	setle %al
	movzbq %al, %r14
	movq $2, %rdi
	call my_malloc
	movq $1, -8(%rax)
	movq %r14, -16(%rax)
L37:
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
	movq $3, %rdi
	call my_malloc
	movq $3, -8(%rax)
	movq $0, -16(%rax)
	leaq L49, %rbx
	movq %rbx, -24(%rax)
	pushq %rax
	movq $3, %rdi
	call my_malloc
	movq $3, -8(%rax)
	movq $1, -16(%rax)
	leaq L48, %rbx
	movq %rbx, -24(%rax)
	popq %r10
	movq -8(%rax), %r11
	movq -8(%r10), %r12
	cmpq %r11, %r12
	jne error
	cmpq $3, %r11
	je L46
	movq -16(%rax), %r13
	movq -16(%r10), %r14
	cmpq %r13, %r14
	setne %al
	movzbq %al, %r14
	movq $2, %rdi
	call my_malloc
	movq $1, -8(%rax)
	movq %r14, -16(%rax)
	jmp L47
L46:
	movq -24(%rax), %rdi
	movq -24(%r10), %rsi
	movq $0, %rax
	call strcmp
	cmpq $0, %rax
	setne %al
	movzbq %al, %r14
	movq $2, %rdi
	call my_malloc
	movq $1, -8(%rax)
	movq %r14, -16(%rax)
L47:
	movq -8(%rax), %r10
	cmpq $0, %r10
	je L40
	cmpq $1, %r10
	je L41
	cmpq $2, %r10
	je L42
	cmpq $3, %r10
	je L43
	cmpq $4, %r10
	jne error
L40:
	leaq none_str, %rdi
	movq $0, %rax
	call printf
	jmp L44
L41:
	movq -16(%rax), %rax
	testq %rax, %rax
	jz L45
	leaq true_str, %rdi
	movq $0, %rax
	call printf
	jmp L44
L45:
	leaq false_str, %rdi
	movq $0, %rax
	call printf
	jmp L44
L42:
	leaq fmt_int, %rdi
	movq -16(%rax), %rsi
	movq $0, %rax
	call printf
	jmp L44
L43:
	leaq fmt_str, %rdi
	movq -24(%rax), %rsi
	movq $0, %rax
	call printf
L44:
	movq $3, %rdi
	call my_malloc
	movq $3, -8(%rax)
	movq $3, -16(%rax)
	leaq L59, %rbx
	movq %rbx, -24(%rax)
	pushq %rax
	movq $3, %rdi
	call my_malloc
	movq $3, -8(%rax)
	movq $3, -16(%rax)
	leaq L58, %rbx
	movq %rbx, -24(%rax)
	popq %r10
	movq -8(%rax), %r11
	movq -8(%r10), %r12
	cmpq %r11, %r12
	jne error
	cmpq $3, %r11
	je L56
	movq -16(%rax), %r13
	movq -16(%r10), %r14
	cmpq %r13, %r14
	sete %al
	movzbq %al, %r14
	movq $2, %rdi
	call my_malloc
	movq $1, -8(%rax)
	movq %r14, -16(%rax)
	jmp L57
L56:
	movq -24(%rax), %rdi
	movq -24(%r10), %rsi
	movq $0, %rax
	call strcmp
	cmpq $0, %rax
	sete %al
	movzbq %al, %r14
	movq $2, %rdi
	call my_malloc
	movq $1, -8(%rax)
	movq %r14, -16(%rax)
L57:
	movq -8(%rax), %r10
	cmpq $0, %r10
	je L50
	cmpq $1, %r10
	je L51
	cmpq $2, %r10
	je L52
	cmpq $3, %r10
	je L53
	cmpq $4, %r10
	jne error
L50:
	leaq none_str, %rdi
	movq $0, %rax
	call printf
	jmp L54
L51:
	movq -16(%rax), %rax
	testq %rax, %rax
	jz L55
	leaq true_str, %rdi
	movq $0, %rax
	call printf
	jmp L54
L55:
	leaq false_str, %rdi
	movq $0, %rax
	call printf
	jmp L54
L52:
	leaq fmt_int, %rdi
	movq -16(%rax), %rsi
	movq $0, %rax
	call printf
	jmp L54
L53:
	leaq fmt_str, %rdi
	movq -24(%rax), %rsi
	movq $0, %rax
	call printf
L54:
	movq $3, %rdi
	call my_malloc
	movq $3, -8(%rax)
	movq $2, -16(%rax)
	leaq L69, %rbx
	movq %rbx, -24(%rax)
	pushq %rax
	movq $3, %rdi
	call my_malloc
	movq $3, -8(%rax)
	movq $2, -16(%rax)
	leaq L68, %rbx
	movq %rbx, -24(%rax)
	popq %r10
	movq -8(%rax), %r11
	movq -8(%r10), %r12
	cmpq %r11, %r12
	jne error
	cmpq $3, %r11
	je L66
	movq -16(%rax), %r13
	movq -16(%r10), %r14
	cmpq %r13, %r14
	setle %al
	movzbq %al, %r14
	movq $2, %rdi
	call my_malloc
	movq $1, -8(%rax)
	movq %r14, -16(%rax)
	jmp L67
L66:
	movq -24(%rax), %rdi
	movq -24(%r10), %rsi
	movq $0, %rax
	call strcmp
	cmpq $0, %rax
	setle %al
	movzbq %al, %r13
	movq $2, %rdi
	call my_malloc
	movq $1, -8(%rax)
	orq %r13, %r14
	movq %r14, -16(%rax)
L67:
	movq -8(%rax), %r10
	cmpq $0, %r10
	je L60
	cmpq $1, %r10
	je L61
	cmpq $2, %r10
	je L62
	cmpq $3, %r10
	je L63
	cmpq $4, %r10
	jne error
L60:
	leaq none_str, %rdi
	movq $0, %rax
	call printf
	jmp L64
L61:
	movq -16(%rax), %rax
	testq %rax, %rax
	jz L65
	leaq true_str, %rdi
	movq $0, %rax
	call printf
	jmp L64
L65:
	leaq false_str, %rdi
	movq $0, %rax
	call printf
	jmp L64
L62:
	leaq fmt_int, %rdi
	movq -16(%rax), %rsi
	movq $0, %rax
	call printf
	jmp L64
L63:
	leaq fmt_str, %rdi
	movq -24(%rax), %rsi
	movq $0, %rax
	call printf
L64:
	movq $3, %rdi
	call my_malloc
	movq $3, -8(%rax)
	movq $1, -16(%rax)
	leaq L79, %rbx
	movq %rbx, -24(%rax)
	pushq %rax
	movq $3, %rdi
	call my_malloc
	movq $3, -8(%rax)
	movq $2, -16(%rax)
	leaq L78, %rbx
	movq %rbx, -24(%rax)
	popq %r10
	movq -8(%rax), %r11
	movq -8(%r10), %r12
	cmpq %r11, %r12
	jne error
	cmpq $3, %r11
	je L76
	movq -16(%rax), %r13
	movq -16(%r10), %r14
	cmpq %r13, %r14
	setle %al
	movzbq %al, %r14
	movq $2, %rdi
	call my_malloc
	movq $1, -8(%rax)
	movq %r14, -16(%rax)
	jmp L77
L76:
	movq -24(%rax), %rdi
	movq -24(%r10), %rsi
	movq $0, %rax
	call strcmp
	cmpq $0, %rax
	setle %al
	movzbq %al, %r13
	movq $2, %rdi
	call my_malloc
	movq $1, -8(%rax)
	orq %r13, %r14
	movq %r14, -16(%rax)
L77:
	movq -8(%rax), %r10
	cmpq $0, %r10
	je L70
	cmpq $1, %r10
	je L71
	cmpq $2, %r10
	je L72
	cmpq $3, %r10
	je L73
	cmpq $4, %r10
	jne error
L70:
	leaq none_str, %rdi
	movq $0, %rax
	call printf
	jmp L74
L71:
	movq -16(%rax), %rax
	testq %rax, %rax
	jz L75
	leaq true_str, %rdi
	movq $0, %rax
	call printf
	jmp L74
L75:
	leaq false_str, %rdi
	movq $0, %rax
	call printf
	jmp L74
L72:
	leaq fmt_int, %rdi
	movq -16(%rax), %rsi
	movq $0, %rax
	call printf
	jmp L74
L73:
	leaq fmt_str, %rdi
	movq -24(%rax), %rsi
	movq $0, %rax
	call printf
L74:
	movq $3, %rdi
	call my_malloc
	movq $3, -8(%rax)
	movq $1, -16(%rax)
	leaq L89, %rbx
	movq %rbx, -24(%rax)
	pushq %rax
	movq $3, %rdi
	call my_malloc
	movq $3, -8(%rax)
	movq $1, -16(%rax)
	leaq L88, %rbx
	movq %rbx, -24(%rax)
	popq %r10
	movq -8(%rax), %r11
	movq -8(%r10), %r12
	cmpq %r11, %r12
	jne error
	cmpq $3, %r11
	je L86
	movq -16(%rax), %r13
	movq -16(%r10), %r14
	cmpq %r13, %r14
	setg %al
	movzbq %al, %r14
	movq $2, %rdi
	call my_malloc
	movq $1, -8(%rax)
	movq %r14, -16(%rax)
	jmp L87
L86:
	movq -24(%rax), %rdi
	movq -24(%r10), %rsi
	movq $0, %rax
	call strcmp
	cmpq $0, %rax
	setl %al
	movzbq %al, %r14
	movq $2, %rdi
	call my_malloc
	movq $1, -8(%rax)
	movq %r14, -16(%rax)
L87:
	movq -8(%rax), %r10
	cmpq $0, %r10
	je L80
	cmpq $1, %r10
	je L81
	cmpq $2, %r10
	je L82
	cmpq $3, %r10
	je L83
	cmpq $4, %r10
	jne error
L80:
	leaq none_str, %rdi
	movq $0, %rax
	call printf
	jmp L84
L81:
	movq -16(%rax), %rax
	testq %rax, %rax
	jz L85
	leaq true_str, %rdi
	movq $0, %rax
	call printf
	jmp L84
L85:
	leaq false_str, %rdi
	movq $0, %rax
	call printf
	jmp L84
L82:
	leaq fmt_int, %rdi
	movq -16(%rax), %rsi
	movq $0, %rax
	call printf
	jmp L84
L83:
	leaq fmt_str, %rdi
	movq -24(%rax), %rsi
	movq $0, %rax
	call printf
L84:
	movq $3, %rdi
	call my_malloc
	movq $3, -8(%rax)
	movq $0, -16(%rax)
	leaq L99, %rbx
	movq %rbx, -24(%rax)
	pushq %rax
	movq $3, %rdi
	call my_malloc
	movq $3, -8(%rax)
	movq $1, -16(%rax)
	leaq L98, %rbx
	movq %rbx, -24(%rax)
	popq %r10
	movq -8(%rax), %r11
	movq -8(%r10), %r12
	cmpq %r11, %r12
	jne error
	cmpq $3, %r11
	je L96
	movq -16(%rax), %r13
	movq -16(%r10), %r14
	cmpq %r13, %r14
	setg %al
	movzbq %al, %r14
	movq $2, %rdi
	call my_malloc
	movq $1, -8(%rax)
	movq %r14, -16(%rax)
	jmp L97
L96:
	movq -24(%rax), %rdi
	movq -24(%r10), %rsi
	movq $0, %rax
	call strcmp
	cmpq $0, %rax
	setl %al
	movzbq %al, %r14
	movq $2, %rdi
	call my_malloc
	movq $1, -8(%rax)
	movq %r14, -16(%rax)
L97:
	movq -8(%rax), %r10
	cmpq $0, %r10
	je L90
	cmpq $1, %r10
	je L91
	cmpq $2, %r10
	je L92
	cmpq $3, %r10
	je L93
	cmpq $4, %r10
	jne error
L90:
	leaq none_str, %rdi
	movq $0, %rax
	call printf
	jmp L94
L91:
	movq -16(%rax), %rax
	testq %rax, %rax
	jz L95
	leaq true_str, %rdi
	movq $0, %rax
	call printf
	jmp L94
L95:
	leaq false_str, %rdi
	movq $0, %rax
	call printf
	jmp L94
L92:
	leaq fmt_int, %rdi
	movq -16(%rax), %rsi
	movq $0, %rax
	call printf
	jmp L94
L93:
	leaq fmt_str, %rdi
	movq -24(%rax), %rsi
	movq $0, %rax
	call printf
L94:
	movq $3, %rdi
	call my_malloc
	movq $3, -8(%rax)
	movq $3, -16(%rax)
	leaq L109, %rbx
	movq %rbx, -24(%rax)
	pushq %rax
	movq $3, %rdi
	call my_malloc
	movq $3, -8(%rax)
	movq $2, -16(%rax)
	leaq L108, %rbx
	movq %rbx, -24(%rax)
	popq %r10
	movq -8(%rax), %r11
	movq -8(%r10), %r12
	cmpq %r11, %r12
	jne error
	cmpq $3, %r11
	je L106
	movq -16(%rax), %r13
	movq -16(%r10), %r14
	cmpq %r13, %r14
	setle %al
	movzbq %al, %r14
	movq $2, %rdi
	call my_malloc
	movq $1, -8(%rax)
	movq %r14, -16(%rax)
	jmp L107
L106:
	movq -24(%rax), %rdi
	movq -24(%r10), %rsi
	movq $0, %rax
	call strcmp
	cmpq $0, %rax
	setle %al
	movzbq %al, %r13
	movq $2, %rdi
	call my_malloc
	movq $1, -8(%rax)
	orq %r13, %r14
	movq %r14, -16(%rax)
L107:
	movq -8(%rax), %r10
	cmpq $0, %r10
	je L100
	cmpq $1, %r10
	je L101
	cmpq $2, %r10
	je L102
	cmpq $3, %r10
	je L103
	cmpq $4, %r10
	jne error
L100:
	leaq none_str, %rdi
	movq $0, %rax
	call printf
	jmp L104
L101:
	movq -16(%rax), %rax
	testq %rax, %rax
	jz L105
	leaq true_str, %rdi
	movq $0, %rax
	call printf
	jmp L104
L105:
	leaq false_str, %rdi
	movq $0, %rax
	call printf
	jmp L104
L102:
	leaq fmt_int, %rdi
	movq -16(%rax), %rsi
	movq $0, %rax
	call printf
	jmp L104
L103:
	leaq fmt_str, %rdi
	movq -24(%rax), %rsi
	movq $0, %rax
	call printf
L104:
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
L109:
	.string "abc"
L108:
	.string "ab"
L99:
	.string ""
L98:
	.string "b"
L89:
	.string "a"
L88:
	.string "b"
L79:
	.string "a"
L78:
	.string "ab"
L69:
	.string "ab"
L68:
	.string "ab"
L59:
	.string "abc"
L58:
	.string "abc"
L49:
	.string ""
L48:
	.string "a"
L39:
	.string "ab"
L38:
	.string "ab"
L29:
	.string "b"
L28:
	.string "c"
L19:
	.string ""
L18:
	.string "a"
L9:
	.string ""
L8:
	.string ""
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
