## Put comments here that give an overall description of what your
## functions do
## Write a short comment describing this function

# makeCacheMatrix: This function creates a special “matrix” object that can cache its inverse.
# cacheSolve: This function computes the inverse of the special “matrix” returned by makeCacheMatrix above. 
#       If the inverse has already been calculated (and the matrix has not changed), 
#       then the cacheSolve should retrieve the inverse from the cache.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) m <<- inverse
        getInverse <- function() m
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        if(!is.null(m)) {
                message("getting cached data, inverse")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
        m
                
}
