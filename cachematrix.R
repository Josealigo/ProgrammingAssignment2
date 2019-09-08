## Because matrix inversion is usually a costly computacion the following
## functions basicly cache the inverse of a matrix


## makeCacheMatrix function will save us a matrix and its inverse matrix also
## a few functions to get that matrix, to change it and also its inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
  m_inverse <- NULL
  set <- function(y) {
    x <<- y
    m_inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m_inverse <<- inverse
  getinverse <- function() m_inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The cacheSolve function gets the output of the makeCacheMatrix (x), looks 
## if its inverse exists and if it does then give that matrix otherwise it
## computes the respective inverse matrix and stores it in the objet x. In both
## cases it returns the inverse matrix.

cacheSolve <- function(x, ...) {
  m_inverse <- x$getinverse()
  if(!is.null(m_inverse)) {
    message("getting cached data")
    return(m_inverse)
  }
  data <- x$get()
  m_inverse <- solve(data, ...)
  x$setinverse(m_inverse)
  m_inverse
}
