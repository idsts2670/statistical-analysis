# Special Practice from the website ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(rlang)
parent.env()
new.env()
rlang::env()
e1 <- env(
    a = FALSE,
    b = "a",
    c = 2.3,
    d = 1:3,
    )

# One importatn implication is that environemnts can contain themselves
e1$d <- e1
# just displays its memory address
e1
# this gives a little more information
env_print(e1)
# get a character vector giving the current bindings
env_names(e1)


# The current environment, or current_env() is the environment in which code is currently executing. 
# When you’re experimenting interactively, that’s usually the global environment, or global_env(). 
# The global environment is sometimes called your “workspace”, as it’s where all interactive (i.e. outside of a function) computation takes place.

# compare environments
identical(global_env(), current_env())
# this returns the error since == is a vectorised operator, and environments are not vectors
# global_env() == current_env()

# Every environment has a parent, another environment
e2a <- env(d = 4, e = 5)
e2b <- env(e2a, a = 1, b = 2, c = 3)

# You can find the parent of an environment with env_parent():
# parenet environement of e2b is e2a
env_parent(e2b)
env_parent(e2a)

# Only one environment doesn’t have a parent: the empty environment.
e2c <- env(empty_env(), d = 4, e = 5)
e2d <- env(e2c, a = 1, b = 2, c = 3)

# The ancestors of every environment eventually terminate with the empty environment.
env_parents(e2b)
env_parents(e2d)

# By default, env_parents() stops when it gets to the global environment. 
# This is useful because the ancestors of the global environment include every attached package, which you can see if you override the default behaviour below
env_parents(e2b, last = empty_env())

#????? 
# the double arrow operator can modify variables in parent levels.
# https://bit.ly/3kmrK58
# The ancestors of an environment have an important relationship to <<-. Regular assignment, <-, always creates a variable in the current environment. 
# Super assignment, <<-, never creates a variable in the current environment, but instead modifies an existing variable found in a parent environment.
x <- 0
f <- function() {
  x <<- 1
}
f()
x

# get and set elements of an environment with $ and [[ in the same way as a list:
e3 <- env(x = 1, y = 2)
e3$x
e3$z <- 3
e3[['z']]

# But you can’t use [[ with numeric indices, and you can’t use [:
#e3[[1]]
#e3[c("x", "Y")]

# env_bind() allows you to bind multiple values:
env_bind(e3, a = 10, b = 20)
# returns object names from an enviroment env as a character vector. 
env_names(e3)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# important
assign()
set()
rm()
ls()
exists()

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ユーザーが定義した関数をデータとして見ると、3つの部位（スロット、メンバー）を持つ構造体と考えることができます-> 仮引数リスト, 本体, 環境
foo <- function(x){x}
# Get or set the formal arguments of a function
# do we have to be able to guess the output??
# returns the formal argument list of the function specified, as a pairlist
formals(foo)
class(formals(foo))
typeof(formals(foo))
mode(formals(foo))

foo <- function(x=1:4){x}
a <- formals(foo)
a
a[1]
# 
a[[1]]
length(a)
a[[2]]
class(a[[2]])
typeof(a[[2]])
mode(a[[2]])
foo




# for hw 6
# moose.density=c(.17,.23,.23,.26,.37,.42,.66,.80,1.11,1.30,1.37,1.41,1.73,2.49)
# kill.rate=c(.37,.47,1.90,2.04,1.12,1.74,2.78,1.85,1.88,1.96,1.80,2.44,2.81,3.75)
# plot(moose.density,kill.rate,type="p")
# m=2.5*(0:100)/100
# a=3.37
# b=0.47
# k=a*m/(b+m)
# points(m,k,type="l")