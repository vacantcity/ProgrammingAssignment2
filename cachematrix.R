## Stores a matrix and caches its inverse in an object in memory
## Returns a list of four functions:
## set(y)      Stores matrix y
## get()       Returns stored matrix
## setinv(inv) Caches inverse of stored matrix
## getinv()    Returns cached inverse of stored matrix

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinv <- function(inv) i <<- inv
    getinv <- function() i
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Computes the inverse of a matrix, caches and returns the result,
## and returns the cached result the next time it is called
## on the same matrix.
## x must be an object of the type returned by makeCacheMatrix()

cacheSolve <- function(x, ...) {
    i <- x$getinv()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinv(i)
    i
}
