; ModuleID = 'javapm'

@printf.1 = private unnamed_addr constant [4 x i8] c"%d\0A\00"

declare i32 @printf(i8*, ...)

declare noalias i8* @malloc(i32)

define i64* @lookup(i32 %c_index, i32 %f_index) {
entry:
  %tmp = alloca i64**
  %tmp1 = alloca i64*
  %tmp2 = getelementptr i64*, i64** %tmp1, i32 0
  store i64* bitcast (i32 ()* @main to i64*), i64** %tmp2
  %tmp3 = getelementptr i64**, i64*** %tmp, i32 0
  store i64** %tmp1, i64*** %tmp3
  %tmp4 = getelementptr i64**, i64*** %tmp, i32 %c_index
  %tmp5 = load i64**, i64*** %tmp4
  %tmp6 = getelementptr i64*, i64** %tmp5, i32 %f_index
  %tmp7 = load i64*, i64** %tmp6
  ret i64* %tmp7
}

define i32 @main() {
entry:
  %list = alloca i32*
  %malloccall = tail call i8* @malloc(i32 mul (i32 add (i32 mul (i32 ptrtoint (i32* getelementptr (i32, i32* null, i32 1) to i32), i32 3), i32 1), i32 ptrtoint (i32* getelementptr (i32, i32* null, i32 1) to i32)))
  %"6tmp" = bitcast i8* %malloccall to i32*
  store i32 add (i32 mul (i32 ptrtoint (i32* getelementptr (i32, i32* null, i32 1) to i32), i32 3), i32 1), i32* %"6tmp"
  br label %array.cond

array.cond:                                       ; preds = %array.init, %entry
  %counter = phi i32 [ 0, %entry ], [ %tmp, %array.init ]
  %tmp = add i32 %counter, 1
  %tmp1 = icmp slt i32 %counter, add (i32 mul (i32 ptrtoint (i32* getelementptr (i32, i32* null, i32 1) to i32), i32 3), i32 1)
  br i1 %tmp1, label %array.init, label %array.done

array.init:                                       ; preds = %array.cond
  %tmp2 = getelementptr i32, i32* %"6tmp", i32 %counter
  store i32 0, i32* %tmp2
  br label %array.cond

array.done:                                       ; preds = %array.cond
  store i32* %"6tmp", i32** %list
  %list3 = load i32*, i32** %list
  %"2tmp" = getelementptr i32, i32* %list3, i32 1
  store i32 9, i32* %"2tmp"
  %list4 = load i32*, i32** %list
  %"2tmp5" = getelementptr i32, i32* %list4, i32 2
  store i32 5, i32* %"2tmp5"
  %list6 = load i32*, i32** %list
  %"2tmp7" = getelementptr i32, i32* %list6, i32 3
  store i32 8, i32* %"2tmp7"
  %print = alloca i1
  store i1 true, i1* %print
  br label %while

while:                                            ; preds = %merge, %array.done
  %print16 = load i1, i1* %print
  %binop_int17 = icmp eq i1 %print16, true
  br i1 %binop_int17, label %while_body, label %merge18

while_body:                                       ; preds = %while
  %i = alloca i32
  store i32 0, i32* %i
  br label %while8

while8:                                           ; preds = %while_body9, %while_body
  %i14 = load i32, i32* %i
  %binop_int15 = icmp slt i32 %i14, 3
  br i1 %binop_int15, label %while_body9, label %merge

while_body9:                                      ; preds = %while8
  %i10 = load i32, i32* %i
  %"1tmp" = add i32 %i10, 1
  %list11 = load i32*, i32** %list
  %"2tmp12" = getelementptr i32, i32* %list11, i32 %"1tmp"
  %"3tmp" = load i32, i32* %"2tmp12"
  %printf = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @printf.1, i32 0, i32 0), i32 %"3tmp")
  %i13 = load i32, i32* %i
  %binop_int = add i32 %i13, 1
  store i32 %binop_int, i32* %i
  br label %while8

merge:                                            ; preds = %while8
  store i1 false, i1* %print
  br label %while

merge18:                                          ; preds = %while
  ret i32 0
}
