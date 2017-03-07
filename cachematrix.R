## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix  <- function(x = matrix()) { 
  ## initialize
  inv <-NULL 
  
  ## iethod to set the iatrix 
  set<-function(y){ 
    x<<-y 
    inv<<-NULL 
  } 
  
  get <- function() x
  
  setInverse <- function(inverse) inv <<- inverse 
  
  getInverse <- function() inv 
  
  ## Return list
  list(set = set, get = get, 
       setInverse = setInverse,  getInverse = getInverse) 
  } 

## This function computes the inverse of the special "matrix" returned by  makeCacheMatrix 
## If the inverse has already been calculated (and the matrix has not changed), 
## then  cacheSolve  should retrieve the inverse from the cache.
## Computing the inverse of a square matrix can be done with the  solve  function 

cacheSolve  <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
}