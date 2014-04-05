	.file	"test.j0.ll"
	.text
	.globl	baz
	.align	16, 0x90
	.type	baz,@function
baz:                                    # @baz
	.cfi_startproc
# BB#0:                                 # %entry
	movl	%esi, -4(%rsp)
	movl	%edi, -8(%rsp)
	movl	-4(%rsp), %eax
	incl	%eax
	ret
.Ltmp0:
	.size	baz, .Ltmp0-baz
	.cfi_endproc

	.globl	main
	.align	16, 0x90
	.type	main,@function
main:                                   # @main
	.cfi_startproc
# BB#0:                                 # %entry
	subq	$24, %rsp
.Ltmp2:
	.cfi_def_cfa_offset 32
	movl	%edi, 16(%rsp)
	movl	$0, 12(%rsp)
	movl	$5, (%rsp)
	movl	12(%rsp), %eax
	movl	%eax, 4(%rsp)
	movl	16(%rsp), %eax
	cmpl	(%rsp), %eax
	je	.LBB1_2
# BB#1:                                 # %if.then
	movl	$65, %edi
	callq	putchar
	movl	$66, %edi
	callq	putchar
	movl	$67, %edi
	jmp	.LBB1_3
.LBB1_2:                                # %if.else
	movl	$65, %edi
	callq	putchar
	movl	$65, %edi
	callq	putchar
	movl	$65, %edi
.LBB1_3:                                # %if.else
	callq	putchar
	movl	(%rsp), %edi
	movl	4(%rsp), %esi
	callq	baz
	testl	%eax, %eax
	je	.LBB1_5
# BB#4:                                 # %if.then1
	movl	$69, %edi
	jmp	.LBB1_6
.LBB1_5:                                # %if.else1
	movl	$70, %edi
.LBB1_6:                                # %if.else1
	callq	putchar
	movl	(%rsp), %eax
	addq	$24, %rsp
	ret
.Ltmp3:
	.size	main, .Ltmp3-main
	.cfi_endproc

	.globl	t
	.align	16, 0x90
	.type	t,@function
t:                                      # @t
	.cfi_startproc
# BB#0:                                 # %entry
	movl	%edi, -8(%rsp)
	movl	%edi, %eax
	ret
.Ltmp4:
	.size	t, .Ltmp4-t
	.cfi_endproc


	.section	".note.GNU-stack","",@progbits
