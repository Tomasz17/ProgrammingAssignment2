makeCacheMatrix <- function(x = matrix()) {
  
  ##set NULL value for inv
  inv <- NULL
  
  ##matrix functions
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  
  ##create output list
  makeVector<-list(
       set = set, 
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  
  ##check if inv exists or should be calculated
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  ##if doesn't exist - getting matrix and calculating inversion (inv)
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
