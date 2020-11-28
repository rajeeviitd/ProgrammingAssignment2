## A pair of functions that cache the inverse of matrix



## The following creates a special matrix object that can cache its inverse
makeCacheMatrix <- function(x=matrix()){
  
  ## The following initialises the inverse property
  inv <- NULL
  
  ## The following method is used to set matrix
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  
  ## The following method is used to get the matrix
  get <- function() {x}
  
  ## The following method is used to set the inverse of the matrix
  setInverse <- function(inverse) {inv <<- inverse}
  
  ## The following method is used to get the inverse of the matrix
  getInverse <- function() {inv}
  
  ## The following method is used to return a list of the methods
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}
#Write a short comment describing this function
cacheSolve <- function(x, ...) {
  ##Return a matrix that is inverse of 'x'
  inv <- x$getInverse()
  if(!is.null) {
    message("getting cached data")
    return(inv)
  }
  
  ## The following method is used to get the matrix from our object 
  mat <- x$get()
  
  ## the following method is used to solve the inverse using matrix multiplication
  inv <- solve(mat, ...)
  
  ## The following method is used to set the inverse to the object
  x$setInverse(inv)
  
  ## the following method is used to return the matrix
  inv
}