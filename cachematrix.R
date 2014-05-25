###############################
## These functions 
##  1. Cache what is by definition
## an invertible, square matrix.
##  2. Determine if the inverse 
## of that matrix has already.
## been calculated and cached. 
##  3. If it has been calculated
##  and cached, return the inverse 
##  from cache and print to console
##  the message:
##  "Getting cached data"
#################################
##  The function "makeCachematrix"
## creates and manages a cached
## matrix and its cached inverse
## functions to both retrieve and
## replace that matrix.
##  set() sets the cached value of the matrix.
##  get() retroeves the value of the matrix
##  setinverse() sets the value of the
##  inverse.
## getinverse() retrieves the value of 
## inverse.
## Typical use:
## amatrix = makeCacheMatrix(matrix(c(1,2,3,4), nrow=2, ncol=2))
## 
## unalteredMatrix <- amatrix$get()
## inverseMatrix <-  amatrix$getinverse()
##

makeCacheMatrix <- function(x = matrix()) {
  inverse_x <- NULL
  set <- function(y) {
      x <<- y
      inverse_x <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inverse_x <<- inverse
  getinverse <- function() inverse_x 
list(set = set, get = get,
  setinverse = setinverse, getinverse = getinverse)
}
############
## cacheSolve manages the calculation
##  of an inverse matrix from a new candidate
##  matrix.
##  First it determines whether there is data
## in the cached inverse. If it is not NULL,
## then the candidate is matrix-multiplied by the
## cached inverse. If the cached inverse was created
## from a matrix identical to the candidate 
## (an "unaltered copy"), the rounded result is an identity matrix.
## That result (createdIDMatrix) is compared to a test identity matrix
## (testIDMatrix) which has the  same dimensions as the candidate matrix. 
## If the two are identitical (identity matricies of the same size),
## the candidate is an "unaltered copy of the original."
## No further calculation required. A message saying "Getting cached data"
## is printed to the console and the function returns.
## Otherwise, the candidte materix is not an "unaltered
## copy" of the one from which the inverse was created.
## A new inverse matrix is calculated, using solve(), and
## cached.
## 
## Typical use:
## amatrix = makeCacheMatrix(matrix(c(1,2,3,4), nrow=2, ncol=2))
##
## cacheSolve(amatrix)
##
cacheSolve <- function(x) {
  data <- x$get()   # Get a copy of the candidate matrix
  inverse_x <- x$getinverse() # And a copy of of the inverse
  if(!is.null(inverse_x)){   # If not null, test whether we have an "unaltered copy"
      testIDMatrix <- diag(nrow = nrow(data)) # Create the test identity matrix
      createdIDMatrix <- round(inverse_x %*% data) # Attempt to generate an ID matrix
      if(identical(testIDMatrix,createdIDMatrix)){ # Are they identical?
        message("Getting cached data.")            # Message to console
        return
      }
}
        inverse <- solve(data) 
        x$setinverse(inverse)
}

