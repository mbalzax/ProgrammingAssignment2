## Couple of fuctions to calculate the inverse matrix 
## and then save it in the cache memory for future
## retrieval and use

## MakeCacheMatrix sets the structures to set and get
## the matrix and its inverse
## Returns a List

makeCacheMatrix <- function(Mx = matrix()) {
   
  Minv <- NULL
  setMx <- function(y) {
    Mx <<- y
    Minv <<- NULL
  }
  getMx <- function() Mx
  setsolve <- function(solve) Minv <<- solve
  getsolve <- function() Minv
  list(setMx = setMx, getMx = getMx,
       setsolve = setsolve,
       getsolve = getsolve)

}


## CacheSolve calculates the Inverse matrix of the
## Square matrix receiver as parameter in MakeCacheMatrix
## if not present in cache already

cacheSolve <- function(Lx, ...) {
        ## Return a matrix that is the inverse of 'Mx'
  
  m <- Lx$getsolve()
  if(!is.null(m)) {
    message("...getting cached inverse matrix")
    return(m)
  }
  data <- Lx$getMx()
  m <- solve(data, ...)
  Lx$setsolve(m)
  m
  
  
  
}
