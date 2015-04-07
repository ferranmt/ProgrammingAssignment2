#CACHEMATRIX

## First, we write a function that creates a matrix object that can cache its inverse:


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x<<-y
    m<<-NULL
  }
  get <- function() x
  setmatrix <- function(solve) m<<- solve
  getmatrix <- function() m 
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)
}


## Now, we create a function called cacheSolve.
## This function computes the inverse of the special "matrix", returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## cacheSolve retrieves the inverse from the cache.


cacheSolve <- function(x=matrix(), ...) {
  m<- x$getmatrix()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  matrix <- x$get()
  m<- solve(matrix, ...)
  x$setmatrix(m)
  m
}


## The outcome is a matrix that is the inverse of 'x'