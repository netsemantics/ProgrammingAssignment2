## This pair of functions allow the use of lexical scoping to hold state i.e cache
## results

## creates a special "matrix", which is really a list containing 4 function to
## set and get the function values and set and get the inverse of the input matrix
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inv) i <<- inv
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
}


## This function takes the output of the 'makeCacheMatrix' function as input
## and either gets a cached value or "solves" the inverse and stores that in the cache
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data) ##%*% data
  x$setinv(i)
  i

}
