; ModuleID = 'javapm'

%Animal = type <{ i32 }>

@printf.1 = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@tmp = private unnamed_addr constant [27 x i8] c"How many cats do you have?\00"
@printf.2 = private unnamed_addr constant [4 x i8] c"%s\0A\00"
@tmp.3 = private unnamed_addr constant [19 x i8] c"You're a cat lady.\00"
@printf.4 = private unnamed_addr constant [4 x i8] c"%s\0A\00"

declare i32 @printf(i8*, ...)

declare i8* @malloc(i32)

define i64* @lookup(i32 %c_index, i32 %f_index) {
entry:
  %tmp = alloca i64**, i32 2
  %tmp1 = alloca i64*, i32 0
  %tmp2 = getelementptr i64**, i64*** %tmp, i32 0
  store i64** %tmp1, i64*** %tmp2
  %tmp3 = alloca i64*
  %tmp4 = getelementptr i64*, i64** %tmp3, i32 0
  store i64* bitcast (i32 ()* @main to i64*), i64** %tmp4
  %tmp5 = getelementptr i64**, i64*** %tmp, i32 1
  store i64** %tmp3, i64*** %tmp5
  %tmp6 = getelementptr i64**, i64*** %tmp, i32 %c_index
  %tmp7 = load i64**, i64*** %tmp6
  %tmp8 = getelementptr i64*, i64** %tmp7, i32 %f_index
  %tmp9 = load i64*, i64** %tmp8
  ret i64* %tmp9
}

define %Animal* @Animal(i32 %x1) {
entry:
  %object = alloca %Animal
  %x = alloca i32
  store i32 %x1, i32* %x
  %x2 = load i32, i32* %x
  %printf = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @printf.1, i32 0, i32 0), i32 %x2)
  ret %Animal* %object
}

define i32 @main() {
entry:
  %printf = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @printf.2, i32 0, i32 0), i8* getelementptr inbounds ([27 x i8], [27 x i8]* @tmp, i32 0, i32 0))
  %cat = alloca %Animal*
  %tmp = call %Animal* @Animal(i32 6)
  store %Animal* %tmp, %Animal** %cat
  %printf1 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @printf.4, i32 0, i32 0), i8* getelementptr inbounds ([19 x i8], [19 x i8]* @tmp.3, i32 0, i32 0))
  ret i32 0
}
