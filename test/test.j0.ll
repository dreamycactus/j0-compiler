; ModuleID = 'my cool compiler'

%Foo = type { i32, i32 }
%Bar = type { i32 }

@Foo0 = external global %Foo
@Bar0 = external global %Bar

declare i32 @putchar(i32)

define i32 @baz({ i32, i32 } %this) {
entry:
  %0 = alloca { i32, i32 }
  store { i32, i32 } %this, { i32, i32 }* %0
  %1 = getelementptr inbounds { i32, i32 }* %0, i32 0, i32 1
  %2 = load i32* %1
  %3 = add i32 %2, 1
  ret i32 %3
}

define i32 @main({ i32 } %this) {
entry:
  %0 = alloca { i32 }
  store { i32 } %this, { i32 }* %0
  %1 = alloca i32
  %2 = alloca { i32, i32 }
  store i32 0, i32* %1
  %3 = getelementptr inbounds { i32, i32 }* %2, i32 0, i32 0
  store i32 5, i32* %3
  %4 = load i32* %1
  %5 = getelementptr inbounds { i32, i32 }* %2, i32 0, i32 1
  store i32 %4, i32* %5
  %6 = getelementptr inbounds { i32 }* %0, i32 0, i32 0
  %7 = load i32* %6
  %8 = getelementptr inbounds { i32, i32 }* %2, i32 0, i32 0
  %9 = load i32* %8
  %10 = sub i32 %7, %9
  %11 = icmp ne i32 0, %10
  br i1 %11, label %if.then, label %if.else

if.then:                                          ; preds = %entry
  %12 = call i32 @putchar(i32 65)
  %13 = call i32 @putchar(i32 66)
  %14 = call i32 @putchar(i32 67)
  br label %if.exit

if.else:                                          ; preds = %entry
  %15 = call i32 @putchar(i32 65)
  %16 = call i32 @putchar(i32 65)
  %17 = call i32 @putchar(i32 65)
  br label %if.exit

if.exit:                                          ; preds = %if.else, %if.then
  %18 = phi i32 [ 67, %if.then ], [ 65, %if.else ]
  %19 = load { i32, i32 }* %2
  %20 = call i32 @baz({ i32, i32 } %19)
  %21 = icmp ne i32 0, %20
  br i1 %21, label %if.then1, label %if.else1

if.then1:                                         ; preds = %if.exit
  %22 = call i32 @putchar(i32 69)
  br label %if.exit1

if.else1:                                         ; preds = %if.exit
  %23 = call i32 @putchar(i32 70)
  br label %if.exit1

if.exit1:                                         ; preds = %if.else1, %if.then1
  %24 = phi i32 [ 69, %if.then1 ], [ 70, %if.else1 ]
  %25 = getelementptr inbounds { i32, i32 }* %2, i32 0, i32 0
  %26 = load i32* %25
  ret i32 %26
}

define i32 @t({ i32 } %this) {
entry:
  %0 = alloca { i32 }
  store { i32 } %this, { i32 }* %0
  %1 = getelementptr inbounds { i32 }* %0, i32 0, i32 0
  %2 = load i32* %1
  ret i32 %2
}
