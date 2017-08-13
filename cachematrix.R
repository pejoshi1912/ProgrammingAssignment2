## makeCachedMatrix stores a matrix and its inverse in the cache. 
## set(matrix)        - to store a matrix (by default its a NULL matrix)
## get()              - to get the stored matrix
## setInverse(matrix) - to store inverse of a matrix
## getInverse()       - to get inverse of the matrix  

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  
  set <- function(matrix){
    x<<- matrix
    inverse <- NULL
  }
  
  get <- function() {
    x
  }
  
  setInverse<- function(inv){
    inverse <<- inv
  }
  
  getInverse <- function(){
    inverse
  }
  
  list(set=set, get= get, setInverse= setInverse, getInverse= getInverse)
}


## cacheSolve(x) is used to get inverse of a non-singular matrix x from 
## cache if present, otherwise it stores as well as returns inverse of
## the matrix x. 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  
  if(!is.null(inv)){
    message("getting inverse from cache")
    return (inv)
  }
  
  ## get matrix from the makeCacheMatrix object x
  m <- x$get()  
  inv <- solve(m)
  x$setInverse(inv)
  inv
}
