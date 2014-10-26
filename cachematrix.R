## This file has two functions
## 1. makeCacheMatrix: creates a special matrix that can cache its inverse
## 2. cacheSolve: computes the inverse of the special matrix in makeCacheMatrix


## This function creates a special matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  
  cachedinverse <- NULL
  
  set <- function(y){
    x <<- y
    inverse <<- NULL
  }
  
  get <- function() x
  
  setinverse <- function(solvedinverse = matrix()){
    cachedinverse <<- solvedinverse
  }
  
  getinverse <- function() cachedinverse
  
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function computes the inverse of the special matrix returned by makeCacheMatrix above.
## If the inverse is already calculated (and the matrix has not been changed), then this
## function retrieves the inverse from the cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  cachedinverse <- x$getinverse()
  
  if(!is.null(cachedinverse))
  {
    message("getting cached inverse matrix")
    return(cachedinverse)
  }
  
  solvedinverse <- solve(x$get())
  x$setinverse(solvedinverse)
  solvedinverse
}
