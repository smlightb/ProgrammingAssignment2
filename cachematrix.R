## Sarah Lightbody
## Data Science - R Programming
## Johns Hopkins University; Coursera
## November 23, 2014

## Matrix inversion is a costly computation. The goal of the functions
## here is to aleviate potential redundant calculations by only calculating
## the inverse of a matrix if it has not been done so previously.

## The first function, makeCacheMatrix creates a special "matrix", which 
## is really a list containing a function to: 
    ## set the value of the matrix
    ## get the value of the matrix
    ## set the value of the inverse
    ## get the value of the inverse
## This function does not solve for the inverse, but the function can cache its 
## computed inverse from the second function, cacheSolve

makeCacheMatrix <- function(x = matrix()) {
  ##initialize variable 'inverse'
  i <- NULL
  ##function to set the matrix
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  ##function to retrieve matrix
  get <- function() x
  setInverse <- function(inverse) i <<- inverse
  getInverse <- function() i
  list (set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}

## The cacheSolve function returns the inverse of a matrix from the makeCacheMatrix
## above. Before solving for the inverse, it first checks if it has been previously 
## computed. If it has, then it returns the inverted matrix without using the solve 
## function. If it has not, then the function utilizes the solve function to compute 
## the inverse and returns it. 

cacheSolve <- function(x, ...) {
  i <- x$getInverse()
  ##if the inverse is in cache, return it
  if(!is.null(i)){
    message("getting cached data...")
    return(i)
  }
  ##otherwise, solve for the inverse and return it
  data <- x$get()
  i <-solve(data)%*%data
  x$setInverse(i)
  i
}

# ## TEST RUN ## 
# x = rbind(c(1, -1/4), c(-1/4, 1))
# test = makeCacheMatrix(x)
# test$get()
# ##this first cacheSolve should have to compute the inverse
# cacheSolve(test)
# ##this second cacheSolve should retrieve the cache value
# ##because the matrix has not changed
# cacheSolve(test)
