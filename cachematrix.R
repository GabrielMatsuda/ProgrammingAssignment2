## The functions presented below are used to calculate the inverse of an invertible matrix using cache
## to reduce time-consuming computations. 

## This function creates a list of auxiliary functions used to get and set 
## the inverse matrix of an invertible matrix.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setSolvedMatrix <- function(solve) m <<- solve
  getSolvedMatrix <- function() m
  
  list( set = set, 
        get = get,
        setSolvedMatrix = setSolvedMatrix,
        getSolvedMatrix = getSolvedMatrix)
}


## This function is used to calculate the inverse of an invertible matrix using R's solve function.
## If the inverse of the matrix hasn't already been calculated, then the inverse is calculated and stored
## in cache. Else, the inverse is restored from cache.

cacheSolve <- function(x) {
  m <- x$getSolvedMatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setSolvedMatrix(m)
  m
}
