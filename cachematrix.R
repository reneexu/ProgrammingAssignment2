
## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse,which 
## returns a list consisting 4 functions to (1)set the value of the matrix (2)get the value of the matrix
## (3)commpute the value of the inverse of the matrix
## (4)get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL                                 #initialization
  # a function to set the value of the matrix 
  set <- function(y) {                        
    x <<- y
    inv <<- NULL
  }
  # a function to get the value of the inputted matrix(argument of makeCacheMatrix)
  # or the matrix set by the above set function
  get <- function() x
  # a function to calculate the inverse of the matrix 
  setinv <- function(solve) inv <<- solve
  # a function to get the value of the inverse
  getinv <- function() inv
  # return a list consisting the above 4 functions
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


##The function cacheSolve returns the inverse of the special "matrix" returned by makeCacheMatrix.
##However, it first checks to see if the inverse has already been calculated. 
##If so, it gets the inverse from the cache and skips the computation. 
##Otherwise, it calculates the  inverse of the matrix and sets the value of the inverse matrix in the cache 
##via the setinv function.

cacheSolve <- function(x, ...) {
  
  #check if the inverse has already been calculated,i.e.,if cache exists
  inv <- x$getinv()
  #retrieve the inverse from the cache and skips the computation. 
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  # if there is no cache, then compute the inverse
  data <- x$get()
  inv <- solve(data, ...)
  # save the result to cache
  x$setinv(inv)
  # return a matrix that is the inverse of 'x'
  inv
}
