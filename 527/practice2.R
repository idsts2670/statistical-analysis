
foo <- function(){ my.fun <- function(x){x}; my.fun}
foo
body(foo)
environment(foo)
environment()
ls()
a <- foo()
a
environment(a)
parent.env(environment(a))

a <- 1:10
a
names(a)
names(a) <- letters[1:10]
a

names(a)
# to check how this function is run in environment
## create the internal expression
## evaluate the expression
quote(names(a) <- letters[1:10])
b <- quote(names(a) <- letters[1:10])
b
class(b)
as.list(b)
# "<-" is an assignement of operator
# "names(a)" is the first sign
`names<-`
`names<-`(a, letters[1:10])
`names<-`(a, letters[1+1:10])
`names<-`(a, letters[2+1:10])
`names<-`(a, letters[1+(1:10)])

# there are five systems to operate the environment

eval(call("mean", quote(1:10)))
eval(as.call(list(quote(mean), quote(1:10))))
eval(as.call(list(as.symbol("mean"), quote(1:10))))
str(as.list(ex[[1]])

# R does not calculate
quote(1 + 2)

# 
e <- quote(1 + 2)
e
class(e)
str(e)
# the operator comes first
as.list(e)

# calculate the object as the formula
eval(e)

# a
eval(e, envir = new.env())
# in emptyenv(), there is no function +
# eval(e, envir = new.env(parent = emptyenv()))

ee <- expression(1 + 4)
ee[[1]]
ee <- ee[[1]]
ee
as.list(ee)
ee[[1]] <- `-`
eval(ee)
ee
e <- as.list(ee)
e

# e[[1]] <- as.name(`-`)

a <- as.call(e)
as.list(a)


eval(a, envir = new.env(parent = emptyenv()))
a[[1]] <- `-`
a
eval(a, envir = new.env(parent = emptyenv()))
foo <- function(x, y = 1:10) {
    a <- match.call(); e
}
foo(x = 10, y = 20)
e
a
e <- quote(a + b)
e
class(e)
typeof(e)
mode(e)
attributes(a)
attributes(e)
attr(e, "class")
# used to convert an object of expression class to an object of character class
deparse(e)
foo <- function(x, y = 1:10){
    e <- match.call()
    plot(1:10, 1:10)
    text(5, 5, deparse(e))
    return(e)
}
foo(x =10, y = 20)

foo <- function(x, y = 1:10){
    e <- match.call()
    plot(1:10, 1:10, xlab = substitute(x), type = "n", bty = "n")
    text(5, 5, deparse(e))
    e
}
foo(x = 100 + 10, y = 20)

iterator <- fuction(x) {
    x = as.vector(x)
    has.next <- function() length(x) > 0
    get.next <- function() {
        # check if x is atomic vector or not
        value = if(is.atomic(x)) x[1] else x[[1]]
        x <- x[-1]
        value
    }
    structure(environment(), class = "iterator")
}

a <- iterator(5)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Feb 23th
# formal argument, body argument (actual code), environment of foo
foo <- function(i){
    i <- 0
    m <- 0
    function(){
        i <<- i + 2
        m <- m + 2
        print(i)
        print(m)
    }
}
first <- foo()
print(first())

foo <- function(){
    count <<- count + 1
    count
}
foo()
environment(foo) <- new.env()
assign("count", value = 0, env = environment(foo))
formals(foo)
body(foo)
eval(body(foo))
environment(foo)
parent.env(environment(foo))
ls(environment(foo))
str(environment(foo))
as.list(environment(foo))
count
foo()
count
as.list(environment(foo))
for(i in 1:100000) foo()
as.list(environment(foo))

# Lazy Evaluation
## Lazy evaluation means that expressions are only evaluated if they are actually used. Similarly, there is a lazy loading. 
## That is, objects are only loaded if they are actually used. 
## These operations are internally implemented with the promise data structure.
foo = function(x) 10
foo(stop("Whoops, this argument is evaluated"))

## If you want to ensure that a argument is evaluated you can use the force() function.
foo = function(x) {
    force(x) # or simply, x
    10
}
foo(stop("Whoops, this argument is evaluated"))
## You can also create promise objects with the delayedAssign() function.
delayedAssign("x", {cat("Evaluating\n"); 1+2})
x

foo <- function(x = {cat("Evaluating x")}) 10
foo
foo()
foo <- function(x = {cat("Evaluating x\n")}) 10 + x
# ??? why numeric? instead of 10?
foo()

# some digit is not meant to be printed
foo <- function(x = {cat("\033[31mEvaluating x\n")}) 10 + x
foo()
cat("\064")

# Package basics: name spaces
library(lattice)
# qq is the function in lattice package
qq
environment(qq)
parent.env(environment(qq))
search()
# funtion int the environment can have a different environment
parent.env()

# do we have .GlobalEnv in package:stats


# Feb 28
search()
var(1:2)
# inh requests only looking at the specific environment
exists("var", envir = as.environment(3), inh = FALSE)
call1 <- quote(sqrt(1 + x^2))
call1
eval(quote(quote(1:10)))
