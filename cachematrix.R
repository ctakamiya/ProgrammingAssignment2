## This program follows the same structure of example code Caching the Mean of 
## a Vector. But, instead caching the mean of vector, this function caches the 
## inverse of a matrix


## As the same idea of makeVector, this function creates a special Matrix, which
## is really a list containing a function to: 
## 1 - set the value of the matrix
## 2 - get the value of the matrix
## 3 - set the value of the inverse of matrix
## 4 - get the value of the inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
        ## It is the best practice to validating input parameters.
        if (nrow(x) != ncol(x)) {
                stop("The matrix must be square matrix.")
        }
        s <- NULL
        set <- function(y) {             
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) s <<- solve
        getsolve <- function() s
        list (set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        s <- x$getsolve()
        if (!is.null(s)) {
                message("Getting cache data")
                return (s)
        }
        dataMatrix <- x$get()
        s <- solve(dataMatrix)
        x$setsolve(s)
        s
}
