## Functions to create a cache matrix of inverse values for given matrices
## makeCacheMatrix - create or return an inverse value for a matrix
## cacheSolve - solve for the inverse of a matrix leveraging the cache if available

## creates a set of functions to manage a cache for inverse solutions for matrices
## 

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


## a function to solve for the inverse of a matrix x, using the cache if it's available

cacheSolve <- function(x, ...) {
    ## attempt to get the cached value
    m <- x$getinverse()    
    ## if m is not null, the value is cached, return the cached value
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    ## otherwise, get the matrix
    data <- x$get()
    ## solve for the inverse
    m <- solve(data, ...)
    ## set it in the cache
    x$setinverse(m)
    ## return the solved value
    m
}
