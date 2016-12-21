; ModuleID = 'javapm'

@tmp = private unnamed_addr constant [24 x i8] c"Is it winter break yet?\00"
@printf.1 = private unnamed_addr constant [4 x i8] c"%s\0A\00"
@tmp.2 = private unnamed_addr constant [21 x i8] c"Finally, it's break!\00"
@printf.3 = private unnamed_addr constant [4 x i8] c"%s\0A\00"
@tmp.4 = private unnamed_addr constant [4 x i8] c"No.\00"
@printf.5 = private unnamed_addr constant [4 x i8] c"%s\0A\00"
@tmp.6 = private unnamed_addr constant [24 x i8] c"Is it winter break yet?\00"
@printf.7 = private unnamed_addr constant [4 x i8] c"%s\0A\00"
@tmp.8 = private unnamed_addr constant [21 x i8] c"Finally, it's break!\00"
@printf.9 = private unnamed_addr constant [4 x i8] c"%s\0A\00"
@tmp.10 = private unnamed_addr constant [4 x i8] c"No.\00"
@printf.11 = private unnamed_addr constant [4 x i8] c"%s\0A\00"
@tmp.12 = private unnamed_addr constant [25 x i8] c"Ist it winter break yet?\00"
@printf.13 = private unnamed_addr constant [4 x i8] c"%s\0A\00"
@tmp.14 = private unnamed_addr constant [25 x i8] c"Yes, it's finally break!\00"
@printf.15 = private unnamed_addr constant [4 x i8] c"%s\0A\00"
@tmp.16 = private unnamed_addr constant [4 x i8] c"No.\00"
@printf.17 = private unnamed_addr constant [4 x i8] c"%s\0A\00"

declare i32 @printf(i8*, ...)

declare i8* @malloc(i32)

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
  %tup = alloca <{ i32, i32, i32 }>*
  %dummy = alloca <{ i32, i32, i32 }>
  %temp = getelementptr inbounds <{ i32, i32, i32 }>, <{ i32, i32, i32 }>* %dummy, i32 0, i32 0
  store i32 2, i32* %temp
  %temp1 = getelementptr inbounds <{ i32, i32, i32 }>, <{ i32, i32, i32 }>* %dummy, i32 0, i32 1
  store i32 2, i32* %temp1
  %temp2 = getelementptr inbounds <{ i32, i32, i32 }>, <{ i32, i32, i32 }>* %dummy, i32 0, i32 2
  store i32 2, i32* %temp2
  store <{ i32, i32, i32 }>* %dummy, <{ i32, i32, i32 }>** %tup
  %tup3 = load <{ i32, i32, i32 }>*, <{ i32, i32, i32 }>** %tup
  %dummy4 = getelementptr inbounds <{ i32, i32, i32 }>, <{ i32, i32, i32 }>* %tup3, i32 0, i32 2
  store i32 1, i32* %dummy4
  %test = alloca i1
  store i1 true, i1* %test
  br label %while

while:                                            ; preds = %merge25, %entry
  %test30 = load i1, i1* %test
  %binop_int31 = icmp eq i1 %test30, true
  br i1 %binop_int31, label %while_body, label %merge32

while_body:                                       ; preds = %while
  %printf = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @printf.1, i32 0, i32 0), i8* getelementptr inbounds ([24 x i8], [24 x i8]* @tmp, i32 0, i32 0))
  %tup5 = load <{ i32, i32, i32 }>*, <{ i32, i32, i32 }>** %tup
  %dummy6 = getelementptr inbounds <{ i32, i32, i32 }>, <{ i32, i32, i32 }>* %tup5, i32 0, i32 0
  %dummy7 = load i32, i32* %dummy6
  %binop_int = icmp eq i32 %dummy7, 1
  br i1 %binop_int, label %then, label %else

merge:                                            ; preds = %else, %then
  %printf10 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @printf.7, i32 0, i32 0), i8* getelementptr inbounds ([24 x i8], [24 x i8]* @tmp.6, i32 0, i32 0))
  %tup11 = load <{ i32, i32, i32 }>*, <{ i32, i32, i32 }>** %tup
  %dummy12 = getelementptr inbounds <{ i32, i32, i32 }>, <{ i32, i32, i32 }>* %tup11, i32 0, i32 1
  %dummy13 = load i32, i32* %dummy12
  %binop_int14 = icmp eq i32 %dummy13, 1
  br i1 %binop_int14, label %then16, label %else18

then:                                             ; preds = %while_body
  store i1 false, i1* %test
  %printf8 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @printf.3, i32 0, i32 0), i8* getelementptr inbounds ([21 x i8], [21 x i8]* @tmp.2, i32 0, i32 0))
  br label %merge

else:                                             ; preds = %while_body
  %printf9 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @printf.5, i32 0, i32 0), i8* getelementptr inbounds ([4 x i8], [4 x i8]* @tmp.4, i32 0, i32 0))
  br label %merge

merge15:                                          ; preds = %else18, %then16
  %printf20 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @printf.13, i32 0, i32 0), i8* getelementptr inbounds ([25 x i8], [25 x i8]* @tmp.12, i32 0, i32 0))
  %tup21 = load <{ i32, i32, i32 }>*, <{ i32, i32, i32 }>** %tup
  %dummy22 = getelementptr inbounds <{ i32, i32, i32 }>, <{ i32, i32, i32 }>* %tup21, i32 0, i32 2
  %dummy23 = load i32, i32* %dummy22
  %binop_int24 = icmp eq i32 %dummy23, 1
  br i1 %binop_int24, label %then26, label %else28

then16:                                           ; preds = %merge
  store i1 false, i1* %test
  %printf17 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @printf.9, i32 0, i32 0), i8* getelementptr inbounds ([21 x i8], [21 x i8]* @tmp.8, i32 0, i32 0))
  br label %merge15

else18:                                           ; preds = %merge
  %printf19 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @printf.11, i32 0, i32 0), i8* getelementptr inbounds ([4 x i8], [4 x i8]* @tmp.10, i32 0, i32 0))
  br label %merge15

merge25:                                          ; preds = %else28, %then26
  br label %while

then26:                                           ; preds = %merge15
  store i1 false, i1* %test
  %printf27 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @printf.15, i32 0, i32 0), i8* getelementptr inbounds ([25 x i8], [25 x i8]* @tmp.14, i32 0, i32 0))
  br label %merge25

else28:                                           ; preds = %merge15
  %printf29 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @printf.17, i32 0, i32 0), i8* getelementptr inbounds ([4 x i8], [4 x i8]* @tmp.16, i32 0, i32 0))
  br label %merge25

merge32:                                          ; preds = %while
  ret i32 0
}
