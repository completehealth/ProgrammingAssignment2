## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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


## Write a short comment describing this function

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
