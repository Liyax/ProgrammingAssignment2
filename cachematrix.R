## Matrix inversion is usually a costly computation and there may be some benefit to cache the inverse of a matrix rather than compute it repeatedly. The following two functions are used to cache the inverse of a matrix.

## The first function, makeCacheMatrix creates a list containing a function to
## 1. Set the value of the matrix
## 2. Get the value of the matrix
## 3. Set the value of the inverse
## 4. Get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
      set <- function(y) {
            x <<- y
            inv <<- NULL
      }
      get <- function() x
      setinverse <- function(inverse) inv <<- inverse
      getinverse <- function() inv
      list(set= set, get=get, setinverse= setinverse, getinverse= getinverse)
}


## The second function calculates the inverse of the matrix. It first
## checks to see if the inverse has already been calculated. If so, it
## gets the inverse from the cache and skips the computation. Otherwise,
## it calculates the inverse of the data and sets the value of the
## inverse in the cache via the setinverse function.

## This function assumes the matrix supplied is always invertible.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
      if (!is.null(inv)) {
            message("getting cached data")
            return(inv)
      }
      data <- x$get()
      inv <- solve(data, ...)
      x$setinverse(inv)
      inv
}
