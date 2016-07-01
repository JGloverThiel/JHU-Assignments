## The functions cache the inverse of a matrix.

## makeCacheMatrix creates a "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL      
  set <- function(y){
              x <<- y
              inv <<- NULL
        }
        get <- function() x
        setinv <- function(solve) inv <<- solve
        getinv <- function() inv
        list(set = set, get = get, setinv = setinv, 
             getinv = getinv)
}

## cacheSolve computes the "matrix" inverse returned by makeCacheMatrix
## or, retrieves the inverse if it has already been calculated and the
## matrix hasn't changed. 
cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)){
          message("getting cached data")
          return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinv(inv)
        return(inv)
}
