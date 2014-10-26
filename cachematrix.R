## Creates a list of four functions that can cache a matrix and its inverse

makeCacheMatrix <- function(x = matrix()) 
{
  i <- NULL # declares variable 'i' within the 'makeCacheMatrix' environment, sets to NULL
  set <- function(y) # assigns y to x within the 'makeCacheMatrix' environment
                     # reassigns NULL to i
  {
    x <<- y
    i <<- NULL
  }
  get <- function() # returns the matrix whose inverse is to be computed
  {
    x
  }
  setinverse <- function(inverse) # assigns inverse to i within the 'makeCacheMatrix' environment
  {
    i <<- inverse
  }
  getinverse <- function() # returns the inverse of the original matrix
  {
    i
  }
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function computes the inverse of the special 'matrix' returned by makeCacheMatrix above. If
## the inverse has already been calculated (and the matrix has not changed), then cacheSolve should
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...)
{
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse() # assigns the value of 'getinverse()' within the list created by makeCacheMatrix
                      # to the variable 'i' declared in the cacheSolve environment
  if(!is.null(i))     # if i is not NULL (meaning the inverse has previously been calculated and stored),
                      # returns the value of i
  {
    message("getting cached data")
    return(i)
  }
  data <- x$get()     # if i is NULL (meaning the inverse has not yet been calculated and stored),
                      # solves for the inverse and passes the result to the setinverse() function
                      # within the makeCacheMatrix() function
  i <- solve(data, ...)
  x$setinverse(i)
  i                   # returns the inverse of the original matrix
}
