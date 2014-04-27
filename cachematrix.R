## The two below functions are used to cache a matrix and the inverse.
##   When finding the inverse, the function first checks the cache
##      to see if the inverse first has previously been processed.


## this function creates the matrix and stores the results in the cache

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInverse <- function(solve) m <<- solve
    getInverse <- function() m
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## This function looks in the cache for the inverse.  
##  If it exists there, then it returns the inverse.  
##      Otherwise, it solves the matrix inversion

cacheSolve <- function(x, ...) {
    m <- x$getInverse()
    if(!is.null(m)){
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setInverse(m)
    m
}
