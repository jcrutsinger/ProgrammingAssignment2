## cachematrix.R
## Created by: Joseph Crutsinger
## Date: 06/19/2014
## Course rprog-004
##
## These functions cache the inverse of a matrix, calculates the first time, 
## and uses a cached copy until the cache is invalidated.
##
## Usage:
##
##      x <- matrix(1:6, 2, 2)
##      y <- makeCacheMatrix(x)
##      cacheSolve(y) # This calculates the inverse and caches it
##
## Output:
##
##       [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
##
## cacheSolve(y) # This pulls the inverse from cache
##
## Gets cached data:
##
##       [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5

## makeCacheMatrix creates a wrapper for a matrix object, providing access
## to a cached copy of the matrix's inverse.
##
## Arguments:
## => x:      a square, invertible matrix
##
## Returns a list with the following functions:
##
## => get:    returns the original matrix.
## => set:    sets the matrix.
## => getinv: returns the inverse of the matrix.
## => setinv: sets the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {

        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }

        get <- function() x
        setinv <- function(newInv) inv <<- newInv
        getinv <- function() inv

        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## Returns the inverse of a matrix, caching the result of the inverse the
## first time it is calculated, and using the cached result for future calls.
##
## Arguments:
##
## => x:   a cache-matrix (the result returned from makeCacheMatrix)
## => ...  additional arguments to be passed to solve()
## Returns a matrix which is the inverse of the matrix stored in x.
## Gets the returned matrix from cache if possible, and if not, calculates
## it and caches it for future calls.

cacheSolve <- function(x, ...) {
        inv <- x$getinv()

        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }

        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
