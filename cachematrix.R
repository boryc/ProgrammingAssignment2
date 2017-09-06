## Functions, that use closures to cache inversions of the matrix

## Function that creates vector, that consists of functions manipulating the matrix

makeCacheMatrix <- function(x = matrix()) {

  invertedMtrx <- NULL
  
  setMtrx <- function(newMtrx) {
    x <<- newMtrx
    invertedMtrx <<- NULL
  }
  
  getMtrx <- function() {
    x
  } 
  
  setInvertedMatrix <- function(newInvertedMtrx) {
    invertedMtrx <<- newInvertedMtrx
  }
  
  getInvertedMtrx <- function() {
    invertedMtrx
  }
  
  list(setMtrx = setMtrx, getMtrx = getMtrx,
       setInvertedMatrix = setInvertedMatrix,
       getInvertedMtrx = getInvertedMtrx)
}


## Function that uses makeCacheMatrix vector with functions to manipulate matrices,
##   calculates matrix inversions and puts it in cache for future reference

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invertedMtrx <- x$getInvertedMtrx()
  
  if(!is.null(invertedMtrx)) {
    message("getting cached data")
    return(invertedMtrx)
  }
  
  mtrx <- x$getMtrx()
  invertedMtrx <- solve(mtrx) %*% mtrx
  x$setInvertedMatrix(invertedMtrx)
  invertedMtrx
}
