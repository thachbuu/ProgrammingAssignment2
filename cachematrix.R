## Put comments here that give an overall description of what your
## functions do

## MakeCacheMatrix will return a list containing 4 methods
## Arguments: 
## + x : the squared matrix can be inversed

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
      x <<- y
      m <<- NULL
  }
  
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get, setsolve= setsolve, getsolve = getsolve)
}


## CacheSovle will return the cached solved value of x if any
## Arguments:
## + x: the squared matrix can be inversed
## E.g.
## matrix <- makeCacheMatrix(rnorm(16),4,4)
## cacheSolve(matrix)
## cacheSolve(matrix) --> the messsage "getting cached data" should be presented


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}