	.text
	.globl	main
main:
	pushq %rbp
	movq %rsp, %rbp
	subq $16, %rsp
	pushq $3
	popq %rax
	movq %rax, -8(%rbp)
	pushq -8(%rbp)
	pushq -8(%rbp)
	popq %rdi
	popq %rax
	imulq %rdi, %rax
	pushq %rax
	popq %rdi
	call print_int
	pushq $3
	popq %rax
	movq %rax, -8(%rbp)
	pushq -8(%rbp)
	pushq -8(%rbp)
	popq %rdi
	popq %rax
	addq %rdi, %rax
	pushq %rax
	popq %rax
	movq %rax, -16(%rbp)
	pushq -8(%rbp)
	pushq -16(%rbp)
	popq %rdi
	popq %rax
	imulq %rdi, %rax
	pushq %rax
	pushq -8(%rbp)
	pushq $3
	popq %rdi
	popq %rax
	addq %rdi, %rax
	pushq %rax
	popq %rax
	movq %rax, -16(%rbp)
	pushq -16(%rbp)
	pushq -16(%rbp)
	popq %rdi
	popq %rax
	cqto
	idivq %rdi
	pushq %rax
	popq %rdi
	popq %rax
	addq %rdi, %rax
	pushq %rax
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
