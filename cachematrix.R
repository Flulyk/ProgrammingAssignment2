## These two functions, when used together, store a matrix and cache its inverse

## This function takes in a matrix and returns a list of functions that allow
## you to set/get the matrix/inverse

makeCacheMatrix <- function(x = matrix()) {
     inv <- NULL
     set <- function(y){
          x <<- y
          inv <<- NULL
     }
     get <- function() x
     setinv <- function(inverse) inv <<- inverse
     getinv <- function() inv
     list(set = set, get = get,
          setinv = setinv, getinv = getinv)
}


## This function takes in the list made from makeCacheMatrix. First it checks
## to see if the inverse has been computed before. If so, it prints a message
## and returns the inverse. If not, it retrieves the matrix and computes the
## inverse via the solve function. It then sets the inverse via x$setinv()
## and returns the inverse of the matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
     inv <- x$getinv()
     if(!is.null(inv)){
          message("getting cached data")
          return(inv)
     }
     data <- x$get()
     inv <- solve(data)
     x$setinv(inv)
     inv
}
