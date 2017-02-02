
## makeCacheMatrix function receives matrix parameter thus returning
## a new object list with 'set', 'get', 'setinver' and 'getinver' functions
## those will manage the values to be required from cacheSolve function
## it also initializes i element with will store the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinver <- function(inver) i <<- inver
  getinver <- function() i
  list(set = set, get = get,
       setinver = setinver,
       getinver = getinver)  
}

## cacheSolve function retrieves the inverse for the matrix given
## if it was previously calculated and stored. if not it calculates
## and stores inverse to i for later calls

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinver()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinver(i)
  i
}
