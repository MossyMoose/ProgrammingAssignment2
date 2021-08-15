## Functions in this file (from assignment):
##  makeCacheMatrix(): This function creates a special "matrix" object that
##    can cache its inverse.
##  cacheSolve(): This function computes the inverse of the special "matrix"
##    returned by makeCacheMatrix above. If the inverse has already been
##    calculated (and the matrix has not changed), then cacheSolve retrieves
##    the inverse from the cache.


##  makeCacheMatrix(x) creates a special matrix object from the matrix x
##    that can cache its inverse and check whether it has already done so.
##  Objects created by the functions to set, get, setInv, and getInv functions
##  Returns list of functions for the object

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  ## set sets the matrix
  set <- function(matrix) {
    x <<- matrix
    inv <<- NULL
  }
  
  ## get returns the matrix
  get<-function() x
  
  ## setInv sets the inverse of the matrix
  setInv <- function(solve) inv <<- solve(x)
  
  ## getInv gets the inverse of the matrix
  getInv <- function() inv
  
  ## Return list of functions for the object
  list(set = set, get=get,
       setInv = setInv,
       getInv = getInv)
}


##  cacheSolve(x) returns a matrix that is the inverse of x.
##  If the inverse is not cached (on first use of the function on x),
##    the function will calculate and cache the inverse.
##  If the inverse has been cached, cacheSolve will return the cached value
##    along with a note that it is returning the cached data.

cacheSolve <- function(x, ...) {
  ##  Check to see if inverse has been cached
  inv <- x$getInv()
  
  ##  If inverse has been cached, return it
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  ##  If inverse has not been cached, get matrix data, invert it using
  ##    solve(), cache the inverse, and return inverse
  data <- x$get()
  inv <- solve(data)
  x$setInv(inv)
  inv
}
