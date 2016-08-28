## Since matrix inversion is usually a costly computation, so the pair of functions 
## (makeCacheMatrix and cacheSolve) cache the inverse of a given matrix.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()){
    i <- NULL
    set <- function(y){
      x <<- y
      i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
    
  }



## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then the 
## cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(mymat,...) {
  
  m <- mymat$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- mymat$get()
  m <- solve(data, ...)
  mymat$setinverse(m)
  m
}
