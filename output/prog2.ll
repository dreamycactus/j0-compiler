%Foo = type { i32, i32 }

@str1 = external global [15 x i8]
@Foo0 = external global %Foo

declare i32 @putchar(i32)

define i32 @main({ i32, i32 } %this) {
entry:
  %0 = alloca { i32, i32 }
  store { i32, i32 } %this, { i32, i32 }* %0
  %1 = alloca { i32, i32 }
  %2 = call i32 @putchar(i32 65)
  %3 = getelementptr inbounds { i32, i32 }* %0, i32 0, i32 1
  store i32 5, i32* %3
  %4 = getelementptr inbounds { i32, i32 }* %1, i32 0, i32 1
  %5 = load i32* %4
  ret i32 %5
}

