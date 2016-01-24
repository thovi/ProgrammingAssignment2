## Put comments here that give an overall description of what your
## functions do

## The first function, makeCacheMatrix creates a special "matrix", which is a list containing a function to
# set the value of the matrix
# get the value of the matrix
# set the value of the inverse (setinv) 
# get the value of the inverse (getinv)

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL # inverse not yet computed
    set <- function(y) {
      x <<- y
      inv <<- NULL
    }
    get <- function() x
    setinv <- function(solve) inv <<- solve #compute the inverse of the matrix
    getinv <- function() inv # return the computed inverse
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## This function checks if the inverse of the matrix x was already calculated. If it was, 
## it returns the cached matrix, otherwise it computes the inverse and then returns it

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv) ## Return a matrix that is the inverse of 'x'
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
