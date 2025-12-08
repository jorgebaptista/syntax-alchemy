	.text
	.globl	main
main:
	pushq %rbp
	movq %rsp, %rbp
	subq $0, %rsp
	pushq $10
	popq %rax
	movq %rax, x
	pushq x
	popq %rdi
	call print_int
	pushq x
	pushq x
	pushq $1
	popq %rdi
	popq %rax
	addq %rdi, %rax
	pushq %rax
	popq %rdi
	popq %rax
	imulq %rdi, %rax
	pushq %rax
	pushq $2
	popq %rdi
	popq %rax
	cqto
	idivq %rdi
	pushq %rax
	popq %rax
	movq %rax, y
	pushq y
	popq %rdi
	call print_int
	pushq $100
	popq %rax
	movq %rax, z
	pushq x
	pushq y
	popq %rdi
	popq %rax
	addq %rdi, %rax
	pushq %rax
	pushq z
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
x:
	.quad 1
y:
	.quad 1
z:
	.quad 1
.Sprint_int:
	.string "%d\n"
