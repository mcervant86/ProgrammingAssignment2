## The following functions allow caching the inverse of a matrix taking advantage
## of lexical scoping in R

## makeCacheMatrix builds a set of functions and returns the functions within a 
## list to the parent environment

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  
  get <- function() x
  setinversem <- function(inversem) i <<- inversem
  getinversem <- function() i
  list(set = set, get = get, setinversem = setinversem, getinversem = getinversem)
}


## cacheSolve allows to populate and/or retrieve the inverse of a matrix of type
## makeChacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinversem()
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinversem(i)
  i
}
