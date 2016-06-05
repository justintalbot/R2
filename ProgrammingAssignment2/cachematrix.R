## The two functions below are designed to make a matrix, cache the results of the inverse, 
## and then retrieve the inverse from the cache for future use

## this function will invert the matrix and store the results for later use

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get, 
         setinverse = setinverse, 
         getinverse = getinverse)
}


## This program checks the cache to see if the matrix has already been inverted, if not
## it will find the inverse of the matrix. 

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if (!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
