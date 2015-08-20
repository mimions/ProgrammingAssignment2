## Matrix inversion is usually a costly computation and there may be some benefit
## to caching the inverse of a matrix rather than compute it repeatedly.
## This function calculates the inverse of a matrix and store it in the cache.
## Every time the program needs to get the inverse of a matrix, it would first
## check if it's already stored in the cache. 
## If so, it will simply fetch the result and skip complex computation.


## The first function makeCacheMatrix creates a spectial "Matrix",
## which is in fact list containing a function to do the following:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse matrix
## 4. get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setInverse <- function(inverseM) inverse <<- inverseM
    getInverse <- function() inverse
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## The second function cacheSolve calculates the inverse matrix with a parameter
## that is in fact the list returning by the first function. 
## It first checks to see if the inverse matrix has already been calculated.
## If so, it gets the result from cache without computation.
## If not, it calculate the inverse matrix and sets the value of the inverse 
## variable in cache via the setInverse function.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse <- x$getInverse()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    matrix <- x$get()
    inverse <- solve(matrix)
    x$setInverse(inverse)
    inverse
}
