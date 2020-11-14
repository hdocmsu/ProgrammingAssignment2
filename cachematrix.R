## Put comments here that give an overall description of what your
## functions do

# Below are the two functions, which are used to create a special matrix object 
# and to calculate it's inversion. Specifically, if existed the inversion is stored in cache
# so that it can be quickly retrieved for future use avoiding long and expensive 
# matrix inversion computation


## Write a short comment describing this function
# makeCacheMatrix() creates special matrix object that can caches its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## Write a short comment describing this function
# cacheSolve() is a function that checks if the inversion is already existed and 
# output the saved inversion. Otherwise, matrix inversion will be executed.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if (!is.null(m)){
    message("cached data existed")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
