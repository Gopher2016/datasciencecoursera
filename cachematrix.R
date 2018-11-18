
rm(list=ls())

## Create a special "matrix" that will...

## set the value of the matrix
## get the value of the matrix
## set the value of the mean
## get the value of the mean

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}



## calculates the mean of the special "matrix" created with the above function

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data) %*% data
  x$setinverse(m)
  m
}


## Example Usage

m <- matrix(rnorm(16), 4)  # Create a 4 x 4 matrix of random numbers

cache <- makeCacheMatrix(m)  # Create a cache list

cache$get()  # Get original matrix

cacheSolve(cache) # Compute matrix inverse

cacheSolve(cache) # Compute matrix inverse from cache
