## makeCacheMatrix creates a matrix type object through the use of a similar
## framework as makeVector. cacheSolve takes the data present and solves the
## matrix found within.

## makeCacheMatrix initializes the inv as NULL, and the set function allows
## for each initialization to set the inv to NULL as to not return old cached
## data. It then initializes a list which is important for cacheSolve.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
        }



## cacheSolve is the workhorse of the equation pair. It takes the initialized
## matrix (if present) from makeCacheMatrix and returns it if it is present.
## if not, the matrix that is passed to it through x variable will be inversed
## by the 'solve' mechanic and then passed on via the 'set' function of
## makeCacheMatrix.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        ## cached
        if(!is.null(inv)) {
                message("getting cached matrix")
                return(inv)
        }
        ## if cache fails, calculate
        dat <- x$get()
        inv <- solve(dat, ...)
        x$setinverse(inv)
        inv
}
