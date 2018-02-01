#Creation on the functions makeCachematrix and makeSolve

## Write a short comment describing this function
##The  function makeCacheMatrix creates a special "vector", which is really a list 
#containing a function to:
# 1- set the value of the matrix
#  2- get the value of the matrix
#	3- set the value of the inverse
#	4-get the value of the inverse


makeCacheMatrix <- function(x = matrix()) {
  mat <- NULL
  set <- function (y){
    x <<- y
    mat<<-NULL 
  }
  get <- function() x
  setsolve<- function(solve) mat<<- solve
  getsolve <- function () mat
  list( set =set, get=get,
        setsolve= setsolve,
        getsolve=getsolve)
  
}


# The cacheSolve function computes the inverse of the special "matrix" returned by
#makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not
#changed), then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
  mat <- x$getsolve()
  if(!is.null(mat)) {
    message("getting cached data")
    return(mat)
  }
  datamat <- x$get()
  mat <- solve(datamat, ...)
  x$setsolve(mat)
  mat
  
}
