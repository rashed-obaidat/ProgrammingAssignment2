## The below functions are made to cache and compute the inverse of a matrix

## The function makeCacheMatrix creates a special matrix object 
## that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## The function cacheSolve compute the inverse of the cached matrix
## returned by makeCacheMatrix function above.
## If the inverse have already been calculated and the matrix was 
## changed this function return the cached value instead of computing it

## This function is built on the assumption that the matrix is always invertible

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setInverse(inv)
    inv
}
