## Matrix inversion is usually a costly computation and there 
## may be some benefit to caching the inverse of a matrix rather 
## than compute it repeatedly. This assignment focuses on writing a pair of functions that 
## cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(original.matrix = matrix()) {
  
  # First check if we have correct input
  if (!is.matrix(original.matrix)) {
    stop("Please give a matrix")
  }
  
  inverted.matrix <- NULL
  
  set <- function(y) {
    original.matrix <<- y
    inverted.matrix <<- NULL
  }
  
  # Functions for getting and setting cached inverse matrix value
  get <- function() original.matrix
  # Inversing the matrix using build in solve() function in R
  set.inverse <- function(solve) inverted.matrix <<- solve
  get.inverse <- function() inverted.matrix
  
  list(
    set = set, 
    get = get,
    set.inverse = set.inverse,
    get.inverse = get.inverse)
  
}



## This function computes the inverse of the special "matrix" created by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}

