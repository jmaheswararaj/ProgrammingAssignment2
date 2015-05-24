## There are 2 functions defined here --> makeCacheMatrix & cacheSolve
## makeCacheMatrix is a list of 4 matrix functions & cacheSolve gives the inverse
## of the input matrix. But a re-run of cacheSolve() function gives the cached
## data

## makeCacheMatrix has 4 matrix functions which:

## 1) sets the value of the matrix
## 2) gets the value of the matrix
## 3) sets the value of the inverse matrix
## 4) gets the value of the cache inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
      x <<- y
      m <<- NULL
  }
  get <- function() x
  setinverse <- function (inverse){
      m <<- inverse
  }
  getinverse <- function (){
      m
  }
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## The cacheSolve calculates the inverse of the special "matrix"
## created with the above function. However, it first checks to see if the
## inverse has already been calculated. If so, it gets the inverse from the
## cache and skips the computation. Otherwise, it calculates the inverse of
## the data and sets the value of the inverse in the cache via the 'setinverse'
## function.

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data,...)
    x$setinverse(m)
    m
}
        ## Return a matrix that is the inverse of 'x'
