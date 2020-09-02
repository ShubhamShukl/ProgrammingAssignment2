## In this R program two functions have been written to give a formal understanding of Cache Concept used in R.

## makeCacheMatrix function creates a vector having the details of previous computed inverse of 
## a matrix and listed values on a list by set, get, setInverse and getInverse variable.

makeCacheMatrix <- function(x = matrix()) {
          inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve function perform the inverse computation of a matrix
## It first checks whether inverse for the matrix exist in cache or not, if not found it then it calculates it and set back into cache.

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
