## These two functions are used to both calculate and cache the inverse of a matrix
## makeCacheMatrix will return an object of type makeCacheMatrix which cacheSolve can
## use to calculate the inverse or retrieve the inverse from cached memory

## makeCacheMatrix will take a matrix and return an object of type makeCacheMatrix
## which contains a list of functions which aid in the setting and getting of both the
## initial matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) i <<- solve
    getinverse <- function() i
    list(set = set, get = get, 
         setinverse = setinverse, 
         getinverse = getinverse)
}


## cacheSolve will take an object of type makeCacheMatrix and either calculate and return its inverse, or if the inverse 
## has already been calculated, simply return the cached inverse.

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
