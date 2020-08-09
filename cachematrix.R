## Put comments here that give an overall description of what your
## functions do

## Cache a matrix

makeCacheMatrix <- function(x = matrix()){
  inv <- NULL  #initializing the inverse as null
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() {x}  #function to get x matrix
  setInverse <- function(inverse) {inv <<- inverse}
  getInverse <- function() {inv}
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## Caching the Inverse of a Matrix
cacheSolve <- function(x, ...){
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)     #returns inverse value
  }
  mat <- x$get()
  inv <- solve(mat, ...)  #calculates inverse value
  x$setInverse(inv)
  inv
}
