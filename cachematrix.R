
##makeCacheMatrix stores 4 functions:
##get(): returns the matrix we input
##set(): changes the matrix we originally input to another matrix
##setinverse(): sets the inverse of input matrix
##getinverse(): retrieves the inverse of input matrix if setinverse() was called before
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  
  setinverse <- function() {
    m<<-solve(x)
  }
  
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

##cacheInverse retrieves the cached inverse of matrix if the matrix inverse was calculated before
##through the $setinverse function
##If the inverse of matrix was never calculated before, calculate it and return the value
cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  m
}