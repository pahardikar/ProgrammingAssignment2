## Put comments here that give an overall description of what your
## functions do

# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {

  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setSolve <- function(solve) m <<- solve
  getSolve <- function() m
  list(set = set, get = get,
       setSolve = setSolve,
       getSolve = getSolve)
}


## This function returns the inverse of a matrix. Since inverse computation is a CPU
## intensive operation, it caches the inverse of the matrix. If the inverse is already
## cached, it returns the cached inverse or else the inverse is computed and stored
## in the cache and then returned.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getSolve()
  if(!is.null(m)){
    message("getting cached matrix inverse")
    return(m)
  }
  matrix <- x$get()
  m <- solve(matrix, ...)
  x$setSolve(m)
  m
}
