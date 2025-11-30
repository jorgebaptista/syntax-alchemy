	.text
	.globl	main
main:
	pushq %rbp
	movq %rsp, %rbp
	subq $0, %rsp
	pushq $42
	popq %rdi
	call print_int
	pushq $10
	popq %rdi
	call print_int
	movq %rbp, %rsp
	popq %rbp
	movq $0, %rax
	ret
print_int:
	pushq %rbp
	movq %rdi, %rsi
	leaq .Sprint_int, %rdi
	movq $0, %rax
	call printf
	popq %rbp
	ret
	.data
.Sprint_int:
	.string "%d\n"
