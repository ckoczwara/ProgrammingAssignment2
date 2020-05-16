## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function 

## The function "makeCacheMatrix" generates a specialized matrix element
## containing functions to set and get values of the matrix, as well as
## set and get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  x_inv=NULL
  set <- function(y) 
  {
    x <<- y ## sets matrix values
    x_inv <<- NULL ## sets x_inv to null to make sure nothing is in the 
                   ## cache when the values of the matrix are changed
  }
  get <- function() x ## function to get/show the matrix values
  setinv <<- function(solve) x_inv <<- solve
  getinv <<- function() x_inv ## function to get/show the inverse matrix values
  list(set = set, get = get, setinv = setinv, getinv = getinv)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  x_inv <- x$getinv()
  if (!is.null(x_inv)) ## checks if x_inv is NOT null
  {
    message("cached data was used")
    return(x_inv)  ## if x_inv is NOT null return the cached value
    
  }
  ## if x_inv is null the inverse has to be calculated
  data <- x$get() ## get values of the matrix
  x_inv <- solve(data, ...) ## calculate inverse
  x$setinv(x_inv) ## set the inverse values in x to the calculated values
  x_inv ## returns the inverse matrix of x
  
}
