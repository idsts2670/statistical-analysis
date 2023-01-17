---
title: "STAT 527 Assignment 1"
author: "Satoshi Ido"
output: pdf_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

* 1
    + (a) Evaluate the following expressions.
```{r message = FALSE}
a <- ((93)^2 - 164)/ (46^3 + 189)
b <- 376 - (23^2) / 4
c <- (59 + 48^2) / ((-9) + 22^2)
d <- (-16 + 55^2) / 13 + 29^2
e <- 18^4 - 16^3 + 14^2 - 12
a
b

```
* 1
    + (b) Evaluate 3x
for x = 1, 2, . . . , 20 and store the values in a vector. Print the vector with the function print(). Report the length of the vector with the function length().
```{r message = FALSE}
x <- 1:20
print(3 * x)
length(3 * x)
```

18^4 - 16^3 + 14^2 - 12
