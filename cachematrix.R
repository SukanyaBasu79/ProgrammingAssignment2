## The two functions in this R code cache the
## inverse of an invertible matrix

## The first function makeCacheMatrix creates a 
## special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
               Inv <- NULL
               set <- function(y) {
                        x <<- y
                        Inv <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) Inv <<- solve
        getinverse <- function() Inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## The second function cacheSolve computes the inverse 
## of the special "matrix" returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        Inv <- x$getinverse()
        if(!is.null(Inv)) {
                message("getting cached data")
                return(Inv)
        }
        data <- x$get()
        Inv <- solve(data, ...)
        x$setinverse(Inv)
        Inv
}
