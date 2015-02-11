#===============================================================================
# author: Maykel L. Gonzalez-Martinez             date (last update): 11/02/2015
# status: finished, tested
#----------------------------- general information -----------------------------
# This is the solution to the Programming Assignment 2 in "R Programming" at
# Coursera.
# The functions below can be used to create a "special" object that stores an
# invertible matrix, and caches its inverse.
# Example:
#  x <- matrix(data = c(1, 2, 3, 4), nrow = 2, ncol = 2) # creates matrix
#  x1 <- makeCacheMatrix(x) # creates "special" R object (including methods)
#  y <- cacheSolve(x1) # calculates and caches x's inverse or gets it from cache
#  print(x %*% y) # multiplies x by its inverse to check it's the unit matrix
#===============================================================================
## makeCacheMatrix creates a special "matrix" (an R list), with the functions:
## . set - to set the value of the matrix;
## . get - to get the value of the matrix;
## . setinv - to set the value of the matrix's inverse; and
## . getinv - to get the value of the matrix's inverse.
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    return(list(set = set, get = get, setinv = setinv, getinv = getinv))
}

## cacheSolve calculates the inverse of the "special" matrix created with
## makeCacheMatrix.  It first checks to see if the inverse has already been
## calculated, and:
## . If so, it gets its value from the cache and skips the computation;
## . Otherwise, it calculates the matrix's inverse (using solve) and sets its
##   value in the cache via the setinv function.
cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    return(inv)
}