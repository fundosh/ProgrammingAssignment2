## Creates functionality to cache the inverse of the suplied matrix
## and retrieve it

## Creates list of functions that operate on matrix:
## - get/set matrix
## - get/set inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inverse <<- inverse
  getInverse <- function() inverse
  list(set = set, get = get,
       setInverse = setInverse, getInverse = getInverse
  )
}

## Takes 'special' matrix object that can store the matrix inverse by
## invoking setInverse() function.
## When the inverse of the matrix is already computed the function
## returns this value. Othervise the inverse is computed and stored.
cacheSolve <- function(x, ...) {
  inverse <- x$getInverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setInverse(inverse)
  inverse
}
