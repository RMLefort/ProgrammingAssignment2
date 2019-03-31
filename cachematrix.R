## Put comments here that give an overall description of what your
## Coursera week 3
## functions do
## 

## Write a short comment describing this function
##makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
##cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
##Computing the inverse of a square matrix can be done with the solve function in R. For example, if X is a square invertible matrix, 
##then solve(X) returns its inverse.

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

##Tests
> TestsMatrix <- matrix(1:4,2,2)
> Cache <- makeCacheMatrix(TestsMatrix)
> Cache$get()
[,1] [,2]
[1,]    1    3
[2,]    2    4
> Cache$getinverse()
NULL
> cacheSolve(Cache)
[,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5

> TestsMatrix <- matrix(c(2,3,5,1,3,7,4,5,6,8,0,0,4,5,6,0),4,4)
> Cache <- makeCacheMatrix(TestsMatrix)
> Cache$get()
[,1] [,2] [,3] [,4]
[1,]    2    3    6    4
[2,]    3    7    8    5
[3,]    5    4    0    6
[4,]    1    5    0    0
> Cache$getinverse()
NULL
> cacheSolve(Cache)
[,1] [,2]       [,3]       [,4]
[1,]  40.0  -30 -1.6666667  19.333333
[2,]  -8.0    6  0.3333333  -3.666667
[3,]   9.5   -7 -0.5000000   4.500000
[4,] -28.0   21  1.3333333 -13.666667


> TestsMatrix <- matrix(c(2, 2, 1, 4), 2, 2)
> Cache <- makeCacheMatrix(TestsMatrix)
> Cache$get()
[,1] [,2]
[1,]    2    1
[2,]    2    4
> Cache$getinverse()
NULL
> cacheSolve(Cache)
[,1]       [,2]
[1,]  0.6666667 -0.1666667
[2,] -0.3333333  0.3333333


