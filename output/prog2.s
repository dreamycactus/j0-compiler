	.file	"prog2.ll"
	.text
	.globl	main
	.align	16, 0x90
	.type	main,@function
main:                                   # @main
	.cfi_startproc
# BB#0:                                 # %entry
	movl	%edi, -8(%rsp)
	movl	$5, -8(%rsp)
	movl	$5, %eax
	ret
.Ltmp0:
	.size	main, .Ltmp0-main
	.cfi_endproc


	.section	".note.GNU-stack","",@progbits
