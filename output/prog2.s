	.file	"prog2.ll"
	.text
	.globl	main
	.align	16, 0x90
	.type	main,@function
main:                                   # @main
	.cfi_startproc
# BB#0:                                 # %entry
	subq	$24, %rsp
.Ltmp1:
	.cfi_def_cfa_offset 32
	movl	%esi, 20(%rsp)
	movl	%edi, 16(%rsp)
	movl	$65, %edi
	callq	putchar
	movl	$5, 20(%rsp)
	movl	12(%rsp), %eax
	addq	$24, %rsp
	ret
.Ltmp2:
	.size	main, .Ltmp2-main
	.cfi_endproc


	.section	".note.GNU-stack","",@progbits
