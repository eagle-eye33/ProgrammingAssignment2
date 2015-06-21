## eagle-eye33, 21 June 2015
## makeCacheMatrix: This function creates a special "matrix" object that can 
## cache its inverse.
##
## cacheSolve: This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache

## makeCacheMatrix
## x      : a square invertible matrix
## return : Matrix that can cache its inverse, containing functions to
##        1. set the Matrix
##        2. get the Matrix
##        3. set the inverse
##        4. get the inverse
##
makeCacheMatrix <- function(x = matrix()) {
   
  ## set the Matrix
  mdata <- NULL
  set <- function(y) {
    x <<- y
    mdata <<- NULL
  }
  ## get the Matrix
  get <- function() x
  ## set the inverse
  setInv <- function(inverse) mdata <<- inverse
  ## get the inverse
  getInv <- function() mdata
  list(set=set, get=get, setInv=setInv, getInv = getInv)
}

## cacheSolve
## x      : output of makeCacheMatrix()
## return : inverse of the Matrix input
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  inv <- x$getInv()

  ## if the inverse has already been calculated
  if(!is.null(inv)) {
    ## get it from the cache
    message("getting cached data")
    return(inv)
  }
  
  ## otherwise, proceed with the calculation of inverse 
    mtxdata <- x$get()
    inv <- solve(mtxdata, ...)
  x$setInv(inv)

  return(inv)
}
