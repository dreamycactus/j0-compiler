j0-compiler
===========

A j0 compiler outputting LLVM IR for a school project. j0 is a toy language that resembles Java. The BNF for the language is found here https://gist.github.com/dreamycactus/20eac37eda6d954315db

I wrote the compiler in Haskell using llvm-general and used stephen diel's tutorial and the minijava compiler ( https://github.com/paulcc/minijava-compiler ) as reference. My experience with Haskell is pretty limited and llvm-general was completely new to me, so don't expect any elegant functional (or even correct) programming. Thanks to the nice people on the haskell-llvm irc were kind enough to help me getting everything set up.

The compiler itself probably has quite a few bugs. It isn't fully correct, and often doesn't catch compilation errors, resulting in problems during linking or execution... but most of the common features are working. That's good enough for now.

Instructions & Notes
============
I built this program using llvm-general v.3.3.11.1, I am not sure it works with any other version. To run,
simply run the compiler executable like so:
src myinputfile.j0.

This will generate a myinputfile.j0.ll.

Then you can use the llvm toolchain to create an executable:

llc myinputfile.j0.ll

gcc myinputfile.j0.s
