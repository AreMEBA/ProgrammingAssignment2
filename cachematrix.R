## Below are two functions that are used to create a special object that stores 
## a numeric matrix and cache's its inverse
##1. makeCacheMatrix, function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) { # set the value of the vector
    x <<- y # assign a value to x in an environment that is different from the current environment
    inv <<- NULL
  }
  get <- function() x # get the value of the vector
  setinverse <- function() inv<<- solve(x)#set the value of the inverse
  getinverse <- function() inv # get the value of the inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## 2. cacheSolve,function computes the inverse of the special "matrix" returned by makeCacheMatrix above

cacheSolve <- function(x, ...) {
  inv <- x$getinverse() # get the inverse from the cache
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()#otherwise compute the inverse of data
  inv <- solve(data, ...)
  x$setinverse(inv) # sets the value of the inv in the cache via setinverse()
  inv     

}
