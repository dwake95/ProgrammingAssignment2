## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        imatrix <- NULL
        set <- function(y) {
                x <<- y
                imatrix <<- NULL
        }

        get <- function() x
        setinverse <- function(solve) imatrix <<- solve 
        getinverse <- function() imatrix
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then the cachesolve should retrieve the inverse 
## from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        imatrix <- x$getinverse()
        if(!is.null(imatrix)) {
                message ("getting cached data")
                return(imatrix)
        }
        data <- x$get()
        imatrix <- solve(data, ...)
        x$setinverse(imatrix)
        imatrix
}
