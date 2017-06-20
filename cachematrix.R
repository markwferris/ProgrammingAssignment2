## These functions create, and cache the inverse of a matrix
## taking advantage of lexical scoping

## makeCacheMatrix creates a special "matrix" object that caches
## its inverse

makeCacheMatrix <- function(x = matrix()) {
  
  #initialize m, which will hold our inverse
  m <- NULL
  
  ## define the set function which can be used to set input matrix
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  
  ## define get function, which returns the matrix currently
  ## being used
  get <- function() x
  
  ## define the setinverse function, which can be used
  ## to set the inverse of x
  setinverse <- function(inverse) m <<- inverse
  
  ##define getinverse which returns the inverse of x
  getinverse <- function() m
  list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  ## attempt to retreive a chached version of the inverse
  m <- x$getinverse()
  
  ## if is an inverse cached
  if(!is.null(m)){
    message("getting cached inverse")
    ## return the cached inverse
    return(m)
  }
  ## if there is no inverse cached
  ##get the matrix from x
  data <- x$get()
  
  ## calculate the inverse
  m <- solve(data, ...)
  
  ## set the inverse
  x$setinverse(m)
  
  ## return the inverse
  m
}