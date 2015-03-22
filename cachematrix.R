#Assignment: Caching the Inverse of a Matrix

#Computing the inverse of a square matrix can be done with the solve function in R. 
#For example, if X is a square invertible matrix, then solve(X) returns its inverse

# makeCacheMatrix creates a list containing a function to
# 1. set : value of the matrix
# 2. get : value of the matrix
# 3. set : value of inverse of the matrix
# 4. get : value of inverse of the matrix

# ASSUMPTION: This function assumes that the matrix is always invertible.
# solve() function used in the code below uses "Gauss-Jordan method" for inverse calculation.

makeCacheMatrix <- function(x = matrix()) 
{
  inv <- NULL
  set <- function(y) 
  {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

# cacheSolve function returns the inverse of the matrix. 
# It first checks if the inverse has already been computed. 
# If yes, then it gets the result from the cache memory and skips thecomputation.
# If no, then it computes the inverse, sets the value in the cache via setinverse function.

cacheSolve <- function(x, ...) 
{
  inv <- x$getinverse()
  if(!is.null(inv)) 
  {
    message("getting cached data.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)                    
  x$setinverse(inv)
  return(inv)
}