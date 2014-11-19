## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    
    ## set the inverse to NULL at start
    inv <- NULL
    
    ## set function used to reset the initial matrix
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    ## get just returns the value of x (the original matrix)
    get <- function() x
    
    ## this is called by cacheSolve() during the first execution
    ##  of cacheSolve() and it will store the value using superassignment
    setInv <- function(inverse) inv <<- inverse
    
    ## this returns the cached value to cacheSolve() on
    ##  subsequent accesses
    getInv <- function() inv
    
    ## this is accessed each time makeCacheMatrix() is called,
    ##  that is, each time we make a new object. This is
    ##  a list of the internal functions ('methods')
    ##  so a calling function knows how to access those methods
    list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## the input x is an object created by makeVector (a list)
    
    ## access the object 'x' and gets the function associated
    ##  with the label 'getmean'. It could have been called anything
    ##  That function returns the value of m
    
    ## get the inverse of the matrix.
    inverse <- x$getInv()
    
    ## check if there matrix is cached, if yes, return it
    if(!is.null(inverse)) {
        print("getting cached data")
        return(inverse)
    }
    
    ## if not cached, get the original input matrix
    data <- x$get()
    
    ## and get its inverse
    inverse <- solve(data, ...)
    
    ## set the inverse of the matrix for caching and return it
    x$setInv(inverse)
    inverse
    
}
