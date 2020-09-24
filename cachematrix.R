## First, create a function that sets a value of the matrix

## This function sets a value of the matrix and then sets
##value of the matrix. The << sets a function to another function


makeCacheMatrix <- function(x = matrix){
  a <- NULL  
  set <- function(b) {
    x <<- b
    a <<- NULL
  }
  get <- function() {x}
  setInverse <- function(inverse) {a <<- inverse}
  getInverse <- function() {a}
  ##create the value of the inverse, set the value of the inverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function computes the inverse of the above matrix

cacheSolve <- function(x, ...){
  ##return a matrix that is the inverse of 'x' and assigns it to the 
  ##'a' matrix from above
  a <- x$getInverse()
  if(!is.null(a)){
      message("getting cached data")
      return(a)
  }
  ##the above calculates if the matrix has already been cached 
  mat <- x$get()
  a <- solve(mat, ...)
  ##this computes the inverse of the matrix using the "solve" function
  x$setInverse(a)
  a
}
