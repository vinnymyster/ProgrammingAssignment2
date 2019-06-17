## Put comments here that give an overall description of what your
## functions do

## in an attempt to understand how R Studio caches/stores values to save computation, we are leveraging two
## functions that will set and get matrices along with computing their inverse both initiall and after 
## it has attempted to retrieve the inverse from memory.

## Write a short comment describing this function

## this function below will set and get a matrix along with setting and getting its inverse that was solved within.

makeCacheMatrix <- function(x = matrix()) {

  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse 
  getinv <- function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## Write a short comment describing this function

## this function below will retrieve an inverse if available in memory. If it cannot find an inverse, it will then retrieve the
## matrix and compute the inverse. Then it stores(sets) that inverse to value inv and returns inv as proof.

cacheSolve <- function(x, ...) {
      
  inv <- x$getinv()
  if (!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  mat.data <- x$get()
  inv <- solve(mat.data, ...)
  x$setinv(inv)
  inv
}