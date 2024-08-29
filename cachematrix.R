## Put comments here that give an overall description of what your
## functions do
# In this R program there are two functions, namely, "makeCacheMatrix"
# and "cacheSolve". The "makeCacheMatrix" creates a special matrix object
# that can cache its inverse, while the "cacheSolve" function computes
# the inverse of the special matrix returned by "makeCacheMatrix". 


# Matrix inversion is computationally costly. There may be some benefits
# to caching the inverse of a matrix rather than compute it repeatedly. 
# Thus, this function creates a matrix object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  # Initialize the inverse property
  m <- NULL

  # Function to set the matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }

  # Function to get the matrix
  get <- function() x

  # Function to set the inverse of the matrix
  setinverse <- function(solve) m <<- solve

  # Function to get the inverse of the matrix
  getinverse <- function() m

  # Return a list of the methods/functions
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

# This part of the R program function computes the inverse of the special 
# matrix returned by "makeCacheMatrix" above. If the inverse has already
# been calculated (and the matrix has notchanged), then the "cacheSolve" 
# function should retrieve the inverse from the cache. 
cacheSolve <- function(x, ...) {
  # Return a matrix that is the inverse of 'x'
  m <- x$getinverse()

  # Return the inverse if it is already set
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  # Get the matrix from the object
  data <- x$get()
  # Calculate the inverse using matrix multiplication
  m <- solve(data, ...)
  # Set the inverse to the object and return the matrix
  x$setinverse(m)
  m
}

# Testing the functions 

#Test 1
#Testing using a two by two invertible square matrix, A. 
A<- matrix(c(5,4,6,5), nrow = 2, ncol = 2)
B<- makeCacheMatrix(A)
cacheSolve(B)

#Test 2
#Testing using a three by three invertible square matrix, C.
C<- matrix(c(1,0,0,0,1,0,0,0,1), nrow=3, ncol=3)
D<- makeCacheMatrix(C)
cacheSolve(D)

#Test 3
#Testing using a four by four invertible square matrix, F. 
F<-matrix(c(1,8,4,2,4,5,5,1,2,2,5,4,4,1,1,2),nrow=4, ncol=4)
G<- makeCacheMatrix(F)
cacheSolve(G)