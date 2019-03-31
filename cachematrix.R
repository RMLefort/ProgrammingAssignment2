## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL                            
  set <- function(y) {                    # define the set function to assign new 
    x <<- y                             # value of matrix in parent environment
    inv <<- NULL                        # if there is a new matrix, reset inv to NULL
  }
  get <- function() x                     #get the value of the Matrix
  
  setinverse <- function(inverse) inv <<- inverse  #set the value of the invertible matrix
  getinverse <- function() inv                     #get the value of the invertible matrix
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse) 
  ## to the functions with the $ operator
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("Getting Cached Invertible Matrix")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
