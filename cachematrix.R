## Below function will construct a new type of Matrix, which has the aiblity to 
## cache the inverse of the original matrix. 
## It achieves this by using scoping assignment (<<-) and directly modify variables in the parent environment.
## Assume the original matrix is always invertible.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function will calculate the inverse of the input matrix.
## It takes the above defined new type of Matrix and will check whether it has already
## cached an inverse value. If yes, then it will directly return the inverse without calculating again.
## If no, it will do perform inverse on the matrix.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
