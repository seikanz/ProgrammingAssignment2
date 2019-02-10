## These functions create a matrix object, calculates its inverse and 
## saves/retrieves both from the cache as required.

## This function creates a matrix object that can cache its inverse.
## sets (caches) the value of the matrix
## gets the value of the matrix
## sets (caches) the inverse value of the matrix
## gets the inverse value of the matrix

makeCacheMatrix <- function(x = matrix()) {

  matrixinverse <- NULL
  
  set <- function(y) {
    print("caching matrix")
    x <<- y
    matrixinverse <<- NULL
  }
  
  get <- function() { 
    print("getting matrix")
    x
  }
  
  setinverse <- function(solve) {
    print("caching inverse")
    matrixinverse <<- solve 
  }
  
  getinverse <- function() {
    print("getting inverse")
    matrixinverse
  }
  
  list(set = set, 
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the matrix returned by makeCacheMatrix. 
## If the inverse has already been calculated, then cacheSolve retrieves the 
## inverse from the cache.

cacheSolve <- function(x, ...) {
  
  matrixinverse <- x$getinverse()
  
  if(!is.null(matrixinverse)) {
    print("Retrieving matrix inverse from cache")
    return(matrixinverse)
  }
  
  matrixdata <- x$get()
  print("Calculating matrix inverse")
  print(matrixdata)
  matrixinverse <- solve(matrixdata)
  x$setinverse(matrixinverse)
  matrixinverse
}
