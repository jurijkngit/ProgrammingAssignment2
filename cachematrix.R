## cached inversed matrix. 

## create CacheMatrix object r
makeCacheMatrix <- function(x = matrix()) {
  ## initialize inverse matrix,x is already set trough parameter transfer
  m <- NULL
  ## set new matrix and clear inversed matrix
  set <- function(y) {
    ## set x and m in outer frame 
    x <<- y
    m <<- NULL
  }
  get <- function() x
  ## set inverse matrix
  setinverse <- function(x,...) m <<- solve(x,...)
  getinverse <- function() m
  ## return accessible elements
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse  )
}   

## get inverse matrix. If needed store it in object 
cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    ## already have inversed matrix
    message("getting cached data")
    return(m)
  }
  ## get matrix data
  ## transfer remaining parameters to solve function inside object
  ## set inverse matrix in object
  x$setinverse(x$get(),...) 
  ## return inverse matrix
  x$getinverse()
}

## Test sample:
## source("cachematrix.R")
## m <- matrix(sample(-1000:1000,100),10,10)  
## mc <- makeCacheMatrix(m)  
## this should produce identity matrix (evaluate twice !)
## mc$get() %*% cacheSolve(mc)