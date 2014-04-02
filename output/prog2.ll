%Foo = type { i32 }

@hello = external global %Foo

declare i32 @putchar(i32)

define i32 @main({ i32 } %this) {
entry:
  %0 = alloca { i32 }
  store { i32 } %this, { i32 }* %0
  %1 = alloca i32
  %2 = getelementptr inbounds { i32 }* %0, i32 0, i32 0
  store i32 5, i32* %2
  %3 = getelementptr inbounds { i32 }* %0, i32 0, i32 0
  %4 = load i32* %3
  ret i32 %4
}

