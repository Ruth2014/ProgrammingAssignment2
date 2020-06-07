## This function will catch the inverse of a matrix
## The function will create a matrix and  catch its inverse

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv = NULL
  set = function(y) {
    x <<- y
    inv <<- NULL
  }
  get = function() x
  setinv = function(inverse) inv <<- inverse 
  getinv = function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}

## This function will the inverse of a matrix created by the above function. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
       inv = x$getinv()
         if (!is.null(inv)){
    return(inv)
  }
  data = x$get()
  inv = solve(data, ...)
  x$setinv(inv)
  return(inv)
}
