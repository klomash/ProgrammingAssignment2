## Contains functions to create and update a special matrix which caches its inverse.

## usage: makeCacheMatrix(a_matrix)
##
## makeCacheMatrix returns a list of 4 methods to set and get the matrix data
## and set and get the inverse of the matrix
##
## makeCacheMatrix also stores the inverse of the matrix which can be queried through the 
## getinverse function. The cached inverse is cleated whever the matrix is set


makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y)
  {
    x <<- y
    m <<- NULL
  }
  get <- function()
  {
    x
  }
  setinverse <- function(inverse)
  {
    i <<- inverse
  }
  getinverse <- function()
  {
    i
  }
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## usage: cacheSolve(special_matrix)
##
## cacheSolve returns the inverse of a "special matrix".
## The "special matrix" here is a value returned by the makeCacheMatrix function
##
## cacheSolve will either return the cached inverse of the special_matrix if it exists
## otherwise would calculate the inverse using the solve function and return it.
## cacheSolve will also set the cached inverse so that future calls to cacheSolve do not
## have to re-calculate the inverse.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv = x$getinverse()
    if(!is.null(inv))
    {
      message("getting cached data")
      return(inv)
    }
    inv <- solve(x$get())
    x$setinverse(inv)
    inv
}
