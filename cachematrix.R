## This file has 2 functions. makeCacheMatrix function takes a matrix as argument and 
## converts it to special makeCacheMatrix that can hold its inverse. 
## Second function cacheSolve takes the special matrix object returned by makeCacheMatrix 
## and returns cached inverse if present or calculates a new matrix inverse, stores it 
##back, and returns it 

## This function creates special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix() ) {
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)  
}


## This function finds special matrix's inverse either by retrieving the existing 
##inverse or calculating and caching a new one. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m) && !is.na(m)) {
    message("getting cache data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
