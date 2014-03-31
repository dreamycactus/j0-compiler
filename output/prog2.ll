; ModuleID = 'my cool compiler'

declare i32 @putchar(i32)

define i32 @main({} %this, i32 %i) {
entry:
  %0 = alloca {}
  store {} %this, {}* %0
  %1 = alloca i32
  store i32 %i, i32* %1
  br i1 true, label %if.then, label %if.else

if.then:                                          ; preds = %entry
  %2 = call i32 @putchar(i32 1)
  br label %if.exit

if.else:                                          ; preds = %entry
  %3 = call i32 @putchar(i32 0)
  br label %if.exit

if.exit:                                          ; preds = %if.else, %if.then
  %4 = phi i32 [ 1, %if.then ], [ 0, %if.else ]
  ret i32 0
}
