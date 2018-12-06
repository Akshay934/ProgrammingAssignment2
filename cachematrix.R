## description of functions-
## 

# makeCacheMatrix creates -
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        invr <- NULL
        set <- function(y) {
                x <<- y
                invr <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) invr <<- inverse
        getinverse <- function() invr
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


# function returns the inverse of the matrix. It first checks if
# the inverse has already been computed. If yes then it gets the result and skip the
# computation step. If not, it computes the inverse, sets the value in the cache via function


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invr <- x$getinverse()
        if(!is.null(invr)) {
                message("getting cached data.")
                return(invr)
        }
        data <- x$get()
        invr <- solve(data, ...)
        x$setinverse(invr)
        invr
}
