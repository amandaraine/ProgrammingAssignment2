## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##The first function, makeCacheMatrix creates a special "vector",  a list containing a function to
##set the matrix
##get the matrix
##set the matrix inverse
##get the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function
## cachesolve calculates the inverse of the matrix
#First checks to see if the matrix inverse has already been calculated. 
#If so, it gets the matrix inverse from the cache. 
#Otherwise, it calculates the matrix inverse and caches it with "setinverse"

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if (!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data,...)
  x$setsolve(m)
  m
}
