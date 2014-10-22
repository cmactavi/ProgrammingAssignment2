## A pair of functions that can compute and cache the inverse
## of a matrix. Matrix supplied must be invertible.

## makeCacheMatrix: Creates a special "matrix" object that can cache 
## its inverse.
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) { ##sets value of matrix, inverse set to NULL
                x <<- y
                m <<- NULL
        }
        get <- function() x ## gets value of matrix
        setinverse <- function(inverse) m <<- inverse ## set inverse value
        getinverse <- function() m ## gets value of inverse
        list(set = set, get = get, #return list of defined functions
             setinverse = setinverse,
             getinverse = getinverse)

}


## cacheSolve: Computes the inverse of the special "matrix" returned by 
## makeCacheMatrix. If the inverse has already been calculated (and the 
## matrix has not changed), then cacheSolve retrieves the inverse from the cache.
cacheSolve <- function(x, ...) {
        m <- x$getinverse() ## get inverse
        if(!is.null(m)) { ## check if m is not NULL
                message("getting cached data")
                return(m) ## return cached inverse value and exit
        }
        data <- x$get() #get matrix 
        m <- solve(data, ...) #calculate inverse
        x$setinverse(m) #cache the inverse
        m
}
