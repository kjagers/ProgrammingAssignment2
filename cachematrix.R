##set the value of the matrix
##get the value of the matrix
##set the value of the inverse
##get the value of the inverse
## Example Data! X = matrix(c(1, 1, 1, 3, 4, 3, 3, 3, 4), nrow = 3, ncol = 3)
## test <- makeCacheMatrix(X)
## cacheSolve(test)

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setsolution <- function(solve) i <<- solve
  getsolution <- function() i
  list(set = set, get = get,
       setsolution = setsolution,
       getsolution = getsolution)
}

## looks for existing solution in cache and prints
## if no solution exists, solves, and caches

cacheSolve <- function(x, ...) {
  i <- x$getsolution()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data)
  x$setsolution(i)
  i
}

