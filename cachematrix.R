## Functions to cache the inverse of matrix
## Write a short comment describing this function

## Below function will create a special matrix which is a list of functions to
## - Set the value of matrix
## - Get the value of matrix
## - set the value of inverse matrix
## - get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {

  icacheval <- NULL
  set <- function(y) 
  {
   x <<- y
   icacheval <<- NULL
  }
  
  get <- function() x
  
  setinverse <- function(inv) icacheval <<- inv
  getinverse <- function() icacheval
  list(
    set = set,
    get = get,
    setinverse = setinverse,
    getinverse = getinverse
  )
}


## Calculate the inverse of the special matrix created with the abovemakeCacheMatrix function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  m <- x$get()
  i <- solve(m, ...)
  x$setinverse(i)
  i
}
