
# 1

cube <- function(x, n) {
  x ^ 3
}
cube(3)

# 2: Why does this following code produce a warning?

x <- 1:10
if(x > 5) {
    x <- 0
}

    # x is a vector of length 10, and "if" can only test a single
    #logical statement

# 3: 

f <- function(x) {
  g <- function(y) {
    y + z
  }
  z <- 4
  x + g(x)
}

f(3)

# 4: What's the value of y?

x <- 5

y <- if(x < 3) {
  NA
} else {
  10
}

y

# 5: What symbol is free?

h <- function(x, y = NULL, d = 3L) {
  z <- cbind(x, d)
  if(!is.null(y))
    z <- z + y
  else
    z <- z + f
  g <- x + y / z
  if(d == 3L)
    return(g)
  g <- g + 10
  g
}
      # f?

# 6: What is an environment?
  
  # a collection of symbol/value pairs

# 7: The R language uses what scoping for resolving free variables?

  # Lexical scoping

# 8: How are free variables in R resolved?

  # Values are searched for in the environment in which it was 
  # called.
