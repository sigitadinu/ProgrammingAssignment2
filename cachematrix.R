## Put comments here that give an overall description of what your
## functions do

## Reducing time needed to compute the inverse of a matrix by using a cache to store the inverse. 
## The inverse of a matrix is calculated just once and it is stored in a cache for later use.

## Write a short comment describing this function
## makeCacheMatrix creates a special matrix data structure. It contains the matrix itself, a cached inverse, as well as setter and getter for the matrix and its cached inverse

makeCacheMatrix <- function(x = matrix()) {
    cachedInverse <- NULL
    set <- function(inputMatrix) {
      x <<- inputMatrix
      cachedInverse <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) cachedInverse <<- inverse
    getInverse <- function() cachedInverse
    list(set = set, get = get,setInverse = setInverse,getInverse=getInverse)
}


## Write a short comment describing this function
## cacheSolve computes the inverse of a matrix. First the function looks up whether the inverse is already cached. If the cache is not null, the value is returned as inverse. 
## Otherwise, the inverse is computed using solve() and the result is cached before being returned by the function.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        cachedInverse <- x$getInverse()
        if(!is.null(cachedInverse)) {
          message("get cached inverse")
          return(cachedInverse)
        }
        data <- x$get()
        inverse <- solve(data)
        x$setInverse(inverse)
        inverse
}
