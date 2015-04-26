## Functions to create a cache matrix of inverse values for given matrices
## makeCacheMatrix - create or return an inverse value for a matrix
## cacheSolve - solve for the inverse of a matrix leveraging the cache if available

## creates a set of functions to manage a cache for inverse solutions for matrices
## 

makeCacheMatrix <- function(x = matrix()) {
    # initialize m to NULL
    m <- NULL
    # define the set function to initialize x and m with lexical scope
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    # define get function to return x
    get <- function() x
    # define a function to solve for inverse and assign to m
    setinverse <- function(solve) m <<- solve
    # define a function to return the inverse solution
    getinverse <- function() m
    # create the reference list of functions
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
