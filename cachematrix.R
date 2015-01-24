## The makeCacheMatrix function creates a list of four functions.
## The cacheSolve function solves the inverse of a matrix 
## and when run twice, returns the cached computation as opposed
## to recomoputing the inverse of the matrix.

## This function creates a list of four functions

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


## This function solves the inverse of the given matrix

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
