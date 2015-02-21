## Functions create special "matrix" object for which inverse can be calculated
## and stored until input matrix is changed

## Function creates special object that can operate with matrix: set value,
## get value, set inverse, get inverse (note that inverse here is dumb matrix)

makeCacheMatrix <- function(x = matrix()) {

  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## If inverse matrix was already calculated returnes cached inverse. Otherwise
## calculates new inverse and caches it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv<-x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data<-x$get()
  inv<-solve(data, ...)
  x$setinverse(inv)
  inv
}
