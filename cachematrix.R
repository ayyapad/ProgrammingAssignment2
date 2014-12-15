## Functions that can be used to cache the inverse of a matrix. 
## Advantage of cache is reduction of repetitive compute to get inverse 
## of a matrix.

## Function to create a special "matrix" that can cache its inverse 

makeCacheMatrix <- function(x = matrix()) {
    
    ## Initialize inverse matrix
    i <- NULL
    
    ## Initialize the matrix 
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    
    ## Return the matrix
    get <- function() x
    
    ## Set the inverse matrix in cache 
    setinverse <- function(inv) i <<- inv
    
    ## get cached inverse matrix 
    getinverse <- function() i
    
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Function that computes inverse of the special "matrix". 

cacheSolve <- function(x, ...) {
    
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    
    ## If the cache exists, then return the cached inverse matrix  
    if (!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    
    ## If no cache created yet, create the inverse of the matrix
    data <- x$get()
    i <- solve(data, ...)
    
    ## Set inverse matrix in the cache 
    x$setinverse(i)
    
    ## Return inverse matrix
    i
}
