##This function makes a special matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<-solve
  getinverse <-function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

##This function calculates the inverse of a special matrix created by the cachematrix function

cacheSolve <- function(x, ...){
  m <- x$getinverse()
  if(!is.null(m)){
    meassage("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}