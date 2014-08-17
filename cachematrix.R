## Functions for Programming Assignment 2.
## Demonstrates caching a potentially slow calculation using the lexical scoping
## system.
## The 2 requested functions have been added as well as an extra example function
## to demonstrate an immutable wrapper approach to adding the caching to a matrix.

## Create a "special" matrix that has the ability to cache its result.
## Copies the given function specification, although it is open to potential bugs
## because setinverse can be passed, and hence cache, an incorrect value that would
## then be propagated.
## A better approach would be to have cacheSolve use memoization to store a set of
## cached values based upon the matrix being used as a key. This would require a keyed
## collection such as a dictionary to be used, which doesn't appear to be a standard
## type within the R language.
## My preferred approach would be to implement the decorator pattern for "cacheSolve"
## and add the ability to store the cache, and calculate it, there. An example of this
## has been added below as: makeEncapsulatedCacheMatrix.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Return the inverse of the supplied variable.
## If a cached value is not null, it will be returned, else the value
## will be calculated, stored in the "special" matrix and returned.

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data)
    x$setinverse(i)
    ## Return a matrix that is the inverse of 'x'
    i
}

## An alternative approach to the matrix cache that maintains encapsulation.
## The exposed functions make the returned value appear immutable, despite the
## internal memoization.

makeEncapsulatedCacheMatrix <- function(x = matrix()) {
    i <- NULL
    get <- function() x
    getinverse <- function() {
        if (!is.null(i)) {
            message("getting cached data")
            return(i)            
        } else {
            i <<- solve(x)
            i
        }
    }
    list(get = get, getinverse = getinverse)
}