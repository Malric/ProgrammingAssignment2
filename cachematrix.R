## Author: Mikko Västi 2015
## Matrix inversion is typically a costly computation. If inversion of some 
## specific matrix is required often, it might be useful to cache the result
## of the inversion. There are several ways how to accomplice this: e.g. 
## storing the result in some variable or using data structures or using
## object-oriented programming paradigm and objects.
## This functionality is performed with 2 separate functions:
## 1. makeCacheMatrix, which is the data structure or "object"
## 2. cacheSolve, which calculates the matrix inverse for makeCacheMatrix.

## makeCacheMatrix creates a list of functions e.g. public methods:
# 1. set the value of the matrix (set-function, takes a matrix as input, no output)
# 2. get the value of the matrix (get-function, returns matrix as output)
# 3. set the value of the inverse of the matrix (setInverse-function,
# takes a matrix as input, no output)
# 4. get the value of the inverse of the matrix (getInverse-function,
# returns the inverse of the matrix)

makeCacheMatrix <- function(x = matrix()) {
  matrixInv <- NULL
  # Constructor / setter
  set <- function(y) {
    x <<- y
    matrixInv <<- NULL
  }
  # getter
  get <- function() x
  # setter for inversion
  setInverse <- function(inverse) matrixInv <<- inverse
  # getter for inversion
  getInverse <- function() matrixInv
  # public functions
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## cacheSolve-function returns inverse of a matrix.
## First check, if matrix inverse can be found in cache and return it.
## If not, then calculate the matrix inversion and cache it.
## After caching the result return the matrix inversion.
## The function uses R-function for matrix inversion (solve-function)
## The correct operation requires the matrix to be invertible.

cacheSolve <- function(x, ...) {
  matrixInv <- x$getInverse()
  # If result is cached, return cached result
  if (!is.null(matrixInv)) {
    message("Loading data from cache!")
    return(matrixInv)
  }
  #if not calculate and cache the result.
  data <- x$get()
  matrixInv <- solve(data)
  x$setInverse(matrixInv)
  matrixInv
}

## Sample usage:
# 1. Create an matrix: x = rbind(c(1,2),c(2,1))
# 2. Create caching matrix m = makeCacheMatrix(x)
# 3. Check that matrix is set using m$get()
# The result should be:
#       [,1] [,2]
# [1,]    1    2
# [2,]    2    1
# 4. Solve inverse with function cacheSolve(m)
# The result should be:
#            [,1]       [,2]
# [1,] -0.3333333  0.6666667
# [2,]  0.6666667 -0.3333333
#
# There are no messages about loading data from cache, thus the inversion was calculated.
#
# 5. Now, when solving the inverse with function cacheSolve(m) for the second
# time, it should result in retrieving the data from cache.
# The result should be:
# Loading data from cache!
#            [,1]       [,2]
# [1,] -0.3333333  0.6666667
# [2,]  0.6666667 -0.3333333