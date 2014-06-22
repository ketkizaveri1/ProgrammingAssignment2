## A pair of functions that cache the inverse of a matrix.
## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  inverse<-null
  set<- function(y)
  {
    x<<-y
    inverse<<- NULL
  }
  get <- function()x
  setinverse<- function(inv) inverse<<-inv
  getinverse<-function() x
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated then the cachesolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse<-x$getinverse()
  if(!is.null(inverse))
  {
    message("Getting Cached Data")
    return(inverse)
  }
  theMatrix <-x$get()
  theInverse <-solve(theMatrix,...)
  x$setinverse(theInverse)
  theInverse
}
