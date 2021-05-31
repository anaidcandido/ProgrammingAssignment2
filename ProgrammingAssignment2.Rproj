## Put comments here that give an overall description of what your
## functions do
# The functions 'makeCacheMatrix' and 'cacheSolve' compute the inverse of an square invertible matrix.

## Write a short comment describing this function
#The first function, `makeCacheMatrix` creates a special "matrix" object, 
#It is a list containing a function to set and get the value of a square invertible matrix
#and its inverse.


makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInv <- function(inverse) i <<- inverse
  getInv <- function() i
  list(set = set, get = get, 
       setInv = setInv,
       getInv = getInv)
}


## Write a short comment describing this function
#The function 'cacheSolve' caches the inverse of the special "matrix" created with 'makeCacheMatrix'
# First, it checks if the inverse of the matrix has been computed. Then, gets the inverse value from 
#the cache. Otherwise, it computes the inverse.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getInv()
  if(!is.null(i)) {
    message("getting inversed matrix")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setInv(i)
  i
}


#Test both functions to cache the inverse of a matrix

m <- matrix(rnorm(16),4,4)
m1 <- makeCacheMatrix(m)
cacheSolve(m1)

