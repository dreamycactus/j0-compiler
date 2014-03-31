	.file	"prog2.ll"
	.text
	.globl	main
	.align	16, 0x90
	.type	main,@function
main:                                   # @main
	.cfi_startproc
# BB#0:                                 # %if.exit
	subq	$24, %rsp
.Ltmp1:
	.cfi_def_cfa_offset 32
	movl	%edi, 12(%rsp)
	movl	$1, %edi
	callq	putchar
	xorl	%eax, %eax
	addq	$24, %rsp
	ret
.Ltmp2:
	.size	main, .Ltmp2-main
	.cfi_endproc


	.section	".note.GNU-stack","",@progbits
