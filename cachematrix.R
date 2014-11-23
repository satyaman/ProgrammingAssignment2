## Functions to cache and return cached value of matrix inverse (as calculated by solve() function) 
## because calculating matrix inverse is a costly function, we want to avoid it as long as the matrix
## has not changed.

## Function that creates a special "matrix" object which is nothing but a list containing function to 
## (a) set a matrix (b) get a matrix
## (c) set inverse of a matrix and (d) get inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
      if (!is.matrix(x)) {
            stop("Input to this function should be a matrix")
      }
      inverse <- NULL
      set <- function(y) {
            ## If matrix changes, then inverse is re-set to NULL. Forcing a re-calculation
            x <<- y
            inverse <<- NULL
      }
      get <- function() x
      setinverse <- function(inv) inverse <<- inv
      getinverse <- function() inverse
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}


## Function that computes the inverse of special "matrix" object created by 'makeCacheMatrix' function
## If the inverse has already been calculated then this function returns the cached value.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      inverse <- x$getinverse()
      if (!is.null(inverse)) {
            message("getting cached data")
            return(inverse)
      }
      matrixdata <- x$get()
      inv <- solve(matrixdata)
      x$setinverse(inv)
      inv
}
