library(rlang)

# データ型
# https://stats.biopapyrus.jp/r/basic/data-type.html
# https://bit.ly/3KdTW4B
## typeof() はデータを保存できる変数のオブジェクトに対する型であり、ベクトル型、リスト型などがある
## mode() はオブジェクトに格納されている要素に対する型であり、理論型、実数型、文字型、関数型などがある
# ちなみに basic typeであるvectorに含まれているtypeはlogical,numeric,integer,double,complex,character がある
# typeofではnumericは出てこなくて、integerとdoubleが区別される. mode()とtypeof()はnumeric以外ではほぼ同じ動作になる.
x <- (c(1:3) > 2)
x
typeof(x)
mode(x)
x <- c(1.5, 3.1)
typeof(x)
mode(x)

## Lをつけることで明示的にintegerとして扱う
typeof(5L)
typeof(5)
y <- c(1L, 3L, 5L)
typeof(y)
z <- 1:5
typeof(z)
search()
# 1はnumericを含むvector型だということがわかる
typeof(1)
mode(1)
# vectorであることを明示的に確認するにはis.vector() を使う
is.vector(1)
# 文字はcharacter
x <- c("a", "b", "c")
x
typeof(x)
mode(x)

## class() はオブジェクトの属性に対する型であり、行列型、factor型、dataframe型などがある (return the values of the class attribute of an R object)
## 属性とはオブジェクトに付随するラベルのようなもので, そのオブジェクトの特徴を表す文字列である. オブジェクトは複数の属性を持つことができる. 属性には, name, dim, dimname, class, tsp(時系列属性) などがある. このうちclass属性によって表される名前がclass()関数で返ってくる値である.
# dataframe
df <- iris
attributes(df)
class(df)
# factor
iris.species <- df$Species
attributes(iris.species)
class(df$Species)
# matrix
m <- matrix(1:9, 3)
## matrixはclass属性を持たないがmatrix型になる
attributes(m)
class(m)

## classがmatrix型、データはnumeric(double)
VADeaths
class(VADeaths)
mode(VADeaths)
typeof(VADeaths)

# matrix, factor, dataframeではない場合numericやcharacterなどを返す
12
class(12)
x <- c("I am learning R programming", "correct my sentence")
x
class(x)


# built-in constants

x <- c("I am learning R programming", "correct my sentence")
x
x[1]

# rnorm() generates multivariate normal random variates in the space X.
x <- rnorm(10)
x


# Tue-Jan-24

# source()
# getwd() returns the name of the working directory
getwd()
# dir() returns the names of files and subdirectories in the working directory
dir()
dir("/Users/satoshiido/Documents/statistical-analysis/Purdue_STAT527/economoydata.txt")
setwd("/Users/satoshiido/Documents/statistical-analysis/")

P <- 10
X <- "as"
ls()


# ============================================================================================ #

# Basic syntax of the R functions and their usage

# A R function is defined with the key word "function" followed 
# first by a pair of parentheses that contains a list of name-and-value pairs separated by commas, called the formal arguments, 
# and then by a statement, call the body. 
# The function body is typically a block statement that is a list of statements nested in a pair of curly braces "{" and "}. 

# Example 1.
   a <- function() 1+2
   a()

  a <- function(x=0) x+2
  a(10)
  a()

# Example 1. Create a function that takes a numeric vector and returns the average of the input vector
my.mean <- function(x) {
      y <-  sum(x)/length(x) 
      return(y)
   }

# The structure of an R function object:
# 1. the formal arguments, which can be obtained by the function formals.
# 2. the function body, which can be obtained by the function body.
# 3. the environment object, which can be accessed by the function environment.

############################################################
# The scripts used in class, Jan. 26, 2023
############################################################
function() 1+2

a <- function() 1+2
a
a()
a <- function(x) x+2
a <- function(x) x+2
a()
a(10)
a <- function(x=0) x+2
a(10)
a()
my.mean <- function(x) sum(x)/length(x)
my.mean
ls()
my.mean(1:3)
my.mean <- function(x) {
y <-  sum(x)/length(x)
y
}

# The simplest function that takes no arguments and returns NULL (a special object representing nothing).

# Access to the components of function objects: 
formals(a)
body(a)
body(ls)
formals(ls)
environment(a)

############################################################
# The end of the scripts used in class, Jan. 26, 2023
############################################################

##
## Jan. 31, 2023
##

## The function-call semantics of R
#  * A deep understanding of environments (or namespaces): ? environment, ? search, ? ls
#  * A good understanding of the read–eval–print loop (REPL)  interactive environment of R
#  * A good understanding of the evaluation of function calls

environment(my.mean)
my.mean <- function(x) { 
   y <- sum(x)/length(x)
   E <- environment()
   print(ls(E))
   cat("y:", y,"\n")
   cat("x:", x, "\n")
   print(E)
   cat("parent environment: ")
   print(parent.env(E))
   cat("return\n")
   return(y)
}

### Scripts in class (with comments added later):
# Take a look at the names of the objects in the working environment, .GlobalEnv
ls()

# Remove all the objects in GlobalEnv
rm(list=ls())
ls()

# Object the environment where the following expression is evaluated
environment()
.GlobalEnv
globalenv()
ls(envir=.GlobalEnv)
x <- 1:2
# y < 2:3
y <- 2:3

# Operations/functions to work with environments as database-type of objects
ls(envir=.GlobalEnv)
x
my.env <- environment()
my.env
# environments can contain themselves
ls(envir=my.env)

# take out the functions out from my.env
my.env$x
get("x", my.env)

# assign new values to the function in my.env
my.env$x <- 1:10
my.env$x
assign("x", 1:5, my.env)
my.env$x
# remove the function "x" from my.env
rm("x", envir=my.env)

# x <- 5

# Define a function
my.mean <- function(x) { y <- sum(x)/length(x); return(y)}
my.mean
my.mean(1:3)

# Let A be the current environment where the following expressions are evaluated
A <- environment()
A

# Where are the expressions of the actual arguments in a function call evaluated?
# They are evaluated in A, where the function call is made!
my.mean(X <- 1:3)


# Create a new environment as the environment of the function my.mean
my.mean
environment(my.mean)
environment(my.mean) <- new.env(parent = A)
environment(my.mean)
# gives us a little more information
env_print(my.mean)
A
identical(global_env(), environment(my.mean))
identical(global_env(), current_env())

environment


# Let B be the environment of the function my.mean
B <- environment(my.mean)
B

# Where is the function body evaluated?
# It is evaluated in a temporary environment!
# The parent environment of this temporary environment is the environment of the function.
# This is verified by the following expressions:
A$y
rm('y')
rm(y)
rm("y")
ls()
my.mean(X <- 1:3)
ls(A)
my.mean
ls(B)
# print(E) returns different environment every time
my.mean <- function(x) {
  y <- sum(x)/length(x)
  E <- environment()
  print(ls(E))
  print(E)
  return(y)
}
my.mean(x <- 1:3)
# returns the environment associated with a given function or formula
environment(my.mean)
env_print(my.mean)

my.meana2 <- function(x) {
  y <- sum(x) / length(x)
  E <- environment()
  print(ls(E))
  cat("y:", y, "\n")
  cat("x:", x, "\n")
  print(E)
  cat("return\n")
  return(y)
}

my.meana2(x <- 1:3)
# returns the environment associated with a given function or formula
environment(my.meana2)
# why is the parent env different from the parent env in environment?
env_print(my.meana2)
ls(environment(my.meana2))

my.meana3 <- function(x) {
  y <- sum(x)/length(x)
  E <- environment()
  print(ls(E))
  cat("y:", y, "\n")
  cat("x:", x, "\n")
  print(E)
  cat("parent environment: ")
  print(parent.env(E))
  cat("return\n")
  return(y)
}
my.meana3(X <- 1:3)
environment(my.meana3)
env_print(my.meana3)
identical(global_env(), environment(my.meana3))

# switch the environment to new.env and so as parent environment
environment(my.meana) <- new.env(parent = A)
B <- environment(my.meana3)

my.meana3(X <- 1:3)
B






# ========================================================================================================================== #

# Feb-02
## Chapter 4. Basic Graphics
## Functions: ls(as.environment("package:graphics"))
## Data: Table 4.2 (read.table(file="https://webpages.uidaho.edu/~brian/rsc/economy%20data.txt", header=TRUE))

# Useful functions:
# class, typeof, ...

# The scripts used in class:
search()
# return a vector of character strings containing all the variables and functions that are defined in the current working directory in R programming
ls()
search()
ls(2)
ls(3)
search()
sin(0)
ls(4)
# coercing an R object to an `environment.`
ls(as.environment("package:graphics"))
ls("package:graphics")
plot.xy
T4.1 <- read.table(file="https://webpages.uidaho.edu/~brian/rsc/economy%20data.txt", header=TRUE)
T4.1[1:3,]
## class() はオブジェクトの属性に対する型であり、行列型、factor型、dataframe型などがある (return the values of the class attribute of an R object)
## 属性とはオブジェクトに付随するラベルのようなもので, そのオブジェクトの特徴を表す文字列である. オブジェクトは複数の属性を持つことができる. 属性には, name, dim, dimname, class, tsp(時系列属性) などがある. このうちclass属性によって表される名前がclass()関数で返ってくる値である.
class(T4.1)
## typeof() はデータを保存できる変数のオブジェクトに対する型であり、ベクトル型、リスト型などがある（全データが同じ型にはなっていない場合、vectorやmatrixではなくlist）
## mode() はオブジェクトに格納されている要素に対する型であり、理論型、実数型、文字型、関数型などがある
typeof(T4.1)
# number of columns
length(T4.1)
# https://bit.ly/41KrzBk
## listやdata.frameに対し一重角括弧[]で任意の列を抽出できるという機能を持つためには、一重角括弧[]の戻り値はlistもしくはdata.frameでなければならない
## data.frameの各列個別の要素にアクセスしたい場合は別の方法で行わなければならず、それが二重角括弧[[]]
## 二重角括弧[[]]でデータを抽出すると、戻り値の型はvectorになる
T4.1[[1]]
T4.1[1]
class(T4.1[[1]])
typeof(T4.1[[1]])
class(T4.1[1])
names(T4.1)

# let us access variables of a DataFrame without invoking any function or method
attach(T4.1)
search()
ls(2)
# color = 2 is red
plot(YEAR, UNEMPLOYMENT, col = 2)
# connect the dot with a line
plot(YEAR, UNEMPLOYMENT, type = "l")


## Chapter 5. Data Input and Output

getwd()
setwd("/Users/satoshiido/Documents/statistical-analysis")
df <- read.delim("./Purdue_STAT527/economy%20data.txt", header = TRUE, sep = "", dec = ".")
df

## Loops
A <- matrix(1:12, ncol = 4)
B <- matrix(1:20, nrow = 4)
A
B
x <- 1:10
y <- rep(2, 10)

# 3 * 3 matrix
Z <- matrix(, nrow = nrow(A), ncol = ncol(B))
Z
# nrow = 3
for (i in 1 : nrow(A)) { # for each row vector of the first operand A
   # ncol = 3
   for (j in 1 : ncol(B)) { # ....
      s <- 0 # initialize ...
      # ncol = 4
      for (k in 1 : ncol(A)) {
         s <- s + A[i, k] * B[k, j]
        }
      Z[i, j] <- s 
     }
} 
Z

# There are other ways of using loop in R, including
# the while, repeat statements 

## Flow Control and Logic (If-statements)

# The scripts used in class, Tuesday, 02/07/2023:

x <- 1:10
y <- rep(2, 10)

x[1] + y[1]
x[2] + y[2]
x[i] + y[i] # for all i=1,...,10
i
i <- 1
x[i] + y[i] # for all i=1,...,10
for (i in 1:10) x[i] + y[i]

A <- {for(i in 1:10) x[i] + y[i]}
A
for(i in 1:10) print(x[i] + y[i])

for(i in 1:10) cat("[", i, "] ", x[i] + y[i], "\n")

A <- matrix(1:12, ncol=4)
A
B <- matrix(1:20, nrow=4)
B
# A B
# matrix multiplication 
A %*% B
A[1, ] * B[, 1]
sum(A[1,] * B[,1])
Z <- matrix(NA, nrow=nrow(A), ncol=ncol(B))
Z
for(i in 1:nrow(A)) {
   for(j in 1:ncol(B)){ 
      Z[i,j] <- sum(A[i,]*B[,j])
      }
   }
# two below are the same
Z
A %*% B

for (i in 1:nrow(A)) {
   for (j in 1:ncol(B)){
      s <- 0
      for (k in 1:ncol(A))
         s <- s + A[i, k] * B[k, j]
         Z[i, j] <- s
      }
   }
Z
A %*% B
i <- 0; while(i < 10) {i <- i + 1; print(i)}
i <- 0; while(TRUE) {i <- i + 1; print(i) ; if(i>10) break}
i <- 0; repeat {i <- i + 1; print(i); if(i>10) break}
i <- 0; repeat {i <- i + 1; print(i); Sys.sleep(5); if(i>10) break}
