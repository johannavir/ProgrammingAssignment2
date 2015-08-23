## This is how to test the functions:

## aa is a matrix to be given a input for makeCacheMatrix():
##   [,1] [,2] [,3]
##    1    0    4
##    1    3    4
##    4    1    0

## cacheSolve(makeCacheMatrix(aa)) returns the inverse of matrix aa (see below)
##          a           b       c
##[1,]  0.08333333 -0.08333333  0.2500
##[2,] -0.33333333  0.33333333  0.0000
##[3,]  0.22916667  0.02083333 -0.0625

# takes a matrix as input to cache it and returns a list of four functions to get and set the inverse
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInv <- function(solve) m <<- solve
  getInv <- function() m
  
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}



## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
 
  
  #gets the inverse from a cache...
  m <- x$getInv()
  if(!is.null(m)) {
    message("getting cached data")
    return
  }
  
  # ...or calculates the inverse
  data <- x$get()
  m <- solve(data, ...)
  
  # sets the value of the inverse in the cache
  x$setInv(m)
  return(m)
  
}
