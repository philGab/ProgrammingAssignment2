## makeCacheMatrix creates a special "matrix" object that can cache its inverse
## "matrix" object is really a list containing a function to:
## set the matrix, get the matrix, 
## set the inverse matrix, get the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    z <- NULL
    set <- function(y) {
        x <<- y
        z <<- NULL
    }
    get <- function() x
    setm <- function(solve) z <<- solve
    getm <- function() z
    list(Set = set, Get = get, SetMatrix = setm, GetMatrix = getm)    
}


## cacheSolve function computes the inverse of the special "matrix" object,
## that returned by makeCacheMatrix function

## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    z <- x$getm()
    if(!is.null(z)) {
        message("getting cached data")
        return(z)
    } 
    data <- x$get()
    z <- solve(data, ...)
    x$setm(z)
    z
}