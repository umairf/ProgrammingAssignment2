## The function makeCacheMatrix creates the special matrix object that can cache its inverse. 
## The function solveCache returns the inverse of the special matrix object. If the inverse was previously computed then it is fetched from the cache. 

## The input to the function should be a square invertible matrix


makeCacheMatrix <- function(x = matrix()) {
  inv<- NULL
  set <- function(y)
  {
    x<<-y;
    inv<<-NULL;
  }
  get <- function() x
  setinverse<- function(inverse1) inv <<- inverse1;
  getinverse<- function() 
    {
     return(inv)
    
    };
  list(set= set, get= get, setinverse= setinverse, getinverse= getinverse)
}


## The input to this function should be the special matrix object that was created using the function 'makeCacheMatrix' of this assingment. 
## It returns the inverse of the matrix. 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv<- x$getinverse()
 
  
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
  
}
