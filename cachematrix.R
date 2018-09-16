## Caching the Inverse of a Matrix:
## Functions that are used to create a special object that 
## stores a matrix and caches its inverse.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  
  setMatrix <- function(matrix = matrix()){
    x <<- matrix
  }
  
  getMatrix <- function() x
  
  setInverse <- function(inverseMatrix = matrix()){ 
    inverse <<- inverseMatrix
  } 
  getInverse <- function() inverse
  list(get = getMatrix, set = setMatrix, getI = getInverse, setI = setInverse)
}


## This function computes the inverse of the special "matrix" created by 
## makeCacheMatrix above.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x' 
  
  if(is.null(x$getI())){
    print("Not cached. Recomputing...") 
    x$setI(solve(x$get()))
  }else {print("Found in cache")}
  
  x$getI()
}
