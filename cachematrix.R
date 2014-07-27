## Put comments here that give an overall description of what your
## functions do
# These functions will first calculate and then cache the inverse of a matrix which can then be called from the cache instead of calculated again.

## Write a short comment describing this function
# makeCacheMatrix takes defaults a null matrix. Then creates a "list" with the # functions setinverse and getinverse which will will
# set the matrix
# get the matrix
# set the inverse of a matrix with "solve"
# get the (solved), inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function
# This function looks up to find if there is an inverse calculated. If not, it will solve the matrix. If the inverse already exists in the cache, it will get it and return it without calculating the inverse again.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m ## Return a matrix that is the inverse of 'x'
}
