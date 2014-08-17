## Functions for Programming Assignment 2.
## Demonstrates caching a potentially slow calculation using the lexical scoping
## system.
## The 2 requested functions have been added as well as an extra example function
## to demonstrate an immutable wrapper approach to adding the caching to a matrix.
## The approach taken copies the given function specification, although it is open
## to potential bugs because setinverse can be passed in by any entity and will become
## an unquestioned cached value. If this were accidentally, or mischeviously, set
## incorrectly then an incorrect value would be stored and then propagated.
## A better approach would be to have cacheSolve use memoization to store a set of
## cached values based upon the matrix being used as a key. This would require a keyed
## collection such as a dictionary to be used, which doesn't appear to be a standard
## type within the R language.
## My preferred approach would be to implement the decorator pattern for "cacheSolve"
## and add the ability to store the cache, and calculate it, there. An example of this
## has been added below as: makeEncapsulatedCacheMatrix.

## makeCacheMatrix
## Summary: Creates a "special" matrix that wraps a matrix with a set of functions stored in the
##          returned list. These enable the matrix value to be changed, as well as a cached value
##          for the inverse of the wrapped matrix.
## Param x: The matrix to be wrapped by the function. The value defaults to an empty matrix.
## Returns: A list containing the following functions that are applied to the wrapped matrix:
##          - set
##          - get
##          - setinverse
##          - getinverse
makeCacheMatrix <- function(x = matrix()) {
    
    ## i
    ## Summary: The cached value of the inverse of the matrix, x. It is set to NULL by default
    ##          to show that the value has not yet been cached.
    i <- NULL
    
    ## set
    ## Summary: Sets the wrapped matrix to the passed in argument. Resets the cached value to null.
    ## Param y: An instance of matrix to wrap. It replaces the currently wrapped instance.
    set <- function(y) {
        ## Update the wrapped matrix to become y
        x <<- y
        
        ## Reset the cached inverse to NULL so that the value will be calculated next time it is needed.
        i <<- NULL
    }
    
    ## get
    ## Summary: Gets the wrapped matrix.
    ## Returns: The wrapped matrix.
    get <- function() x
    
    ## setinverse:
    ## Summary: Sets the internally held inverse value to the passed in inverse value.
    ## Param inverse: The new value to cache as the inverse.
    setinverse <- function(inverse) i <<- inverse
    
    ## getinverse:
    ## Summary: Gets the internally held inverse value.
    ## Returns: The internally held inverse, which will be NULL until it has been set.
    getinverse <- function() i
    
    ## Return the list of functions.
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## cacheSolve
## Summary: Return the inverse of the CacheMatrix, x. An attempt is made to use the
##          cached value held in x, but if it is not yet set the inverse will be calculated,
##          cached in the CacheMatrix, and returned.
## Param x: The CacheMatrix to use for retrieving or calculating the inverse.
## Returns: The inverse of x.
cacheSolve <- function(x, ...) {
    
    ## Retrieve the currently cached inverse value of x.
    i <- x$getinverse()
    
    if(is.null(i)) {
        
        ## There is not an existing cached value for the inverse of x, calculate and cache one.
        
        ## Retrieve the raw matrix from x.
        data <- x$get()
        
        ## Calculate the inverse of the matrix and store it to i.
        i <- solve(data)
        
        ## Cache the value of i inside the CacheMatrix, x.
        x$setinverse(i)        
    } else {
        
        ## A cached value exists. Output a message to inform the user of this.
        message("getting cached data")
    }
    
    ## Return a matrix that is the inverse of 'x'
    return(i)
}

## makeEncapsulatedCacheMatrix
## Foreword: An alternative approach to the matrix cache that maintains encapsulation.
##           The exposed functions make the returned value appear immutable, despite the
##           internal caching. This avoids potential corruption.
## Summary: Creates a "special" matrix that wraps a matrix with a set of functions stored in the
##          returned list. These enable the matrix to be retrieved, as well as the inverse of
##          the wrapped matrix, which may be cached.
## Param x: The matrix to be wrapped by the function.
## Returns: A list containing the following functions that are applied to the wrapped matrix:
##          - get
##          - getinverse
makeEncapsulatedCacheMatrix <- function(x) {
    
    ## i
    ## Summary: The cached value of the inverse of the matrix, x. It is set to NULL by default
    ##          to show that the value has not yet been cached.
    i <- NULL
    
    ## get
    ## Summary: Gets the wrapped matrix.
    ## Returns: The wrapped matrix.
    get <- function() x
    
    ## getinverse:
    ## Summary: Gets the internally held inverse value, if it has not yet been set then it will
    ##          be calculated and stored before being returned.
    ## Returns: The inverse of the matrix, x.
    getinverse <- function() {
        
        if(is.null(i)) {
            
            ## There is not an existing cached value for the inverse of x, calculate and cache one.
            
            ## Calculate the inverse of the matrix and store it to i.
            i <<- solve(x)      
        } else {
            
            ## A cached value exists. Output a message to inform the user of this.
            message("getting cached data")
        }
        
        ## Return a matrix that is the inverse of 'x'
        return(i)
    }
    
    ## Return the list of functions.
    list(get = get, getinverse = getinverse)
}