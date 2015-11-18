## Caches the inverse of a matrix once it is calculated, so that it can be quickly retrieved without calculating again when needed.

## Creates a list that contains functions for setting and getting the value of a vector, and for setting and getting the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    inv_mat <- NULL
    set <- function(y) {
        x <<- y
        inv_mat <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv_mat <<- inverse
    getinverse <- function() inv_mat
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Attempts to retrieve the inverse of a matrix from the cache, and calculates the inverse if it cannot.

cacheSolve <- function(x, ...) {
    ## Returns a matrix that is the inverse of 'x'
    inv_mat <- x$getinverse()
    if(!is.null(inv_mat)) {
        message("getting cached data")
        return(inv_mat)
    }
    data <- x$get()
    inv_mat <- solve(data)
    x$setinverse(inv_mat)
    inv_mat
}
