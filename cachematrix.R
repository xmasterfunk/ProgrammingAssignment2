## create a matrix from the value passed in as x
makeCacheMatrix <- function(x = matrix()) {
  n <- NULL
  set <- function(y){
    x <<- y
    n <<- NULL
  }
  
  get <- function()x
  setMatrixInverse <- function(inverse) n <<- inverse
  getMatrixInverse <- function() n 
  list(set = set, get = get,
       setMatrixInverse = setMatrixInverse,
       getMatrixInverse = getMatrixInverse)
}

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  n <- x$getMatrixInverse()
  if(!is.null(n)){
    message("getting cached data")
    return(n)
  }
  mat <- x$get()
  n <- solve(mat,...)
  x$setMatrixInverse(n)
  n
}