## Put comments here that give an overall description of what your
## functions do

## The first function, makeCacheMatrix creates a special "vector", which is really a list containing a function to
## set the value of the matrix
## get the value of the matrix 
## set the value for the inverse matrix
## get the value for the inverse matrix


makeCacheMatrix <- function(x = matrix()) {
# Initialize the inverse matrix
  inverse <- NULL
  
  # Set function, used to set the matrix data
  
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  # Get function, used to retrive the matrix content
  
  get <- function() x
  
  # Set Inverse function, used to set the Inverse matrix content
  
  setInverse <- function(inv) inverse <<- inv
  
  # GetInverse function, used to get the Inverse matrix content
  getInverse <- function() inverse
  
  #List function (vector) to return all the functions
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Pull the inverse matrix content, if exist, retrieve the cached data, else create a inverse matrix
 
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  # create an inverse matrix
  m <- solve(data, ...)
  
  # set the created inverse matrix for future gets
  x$setInverse(m)
  # return the inverse data
  m
  
}