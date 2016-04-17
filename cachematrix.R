## Create two functions to create a special object that stores a matrix and cache's its inverse.

##The first function, makeCacheMatrix creates a function which is a list containing functions to
## set the value of the matrix
## get the value of the matrix
## set the value of the  Inverse matrix
## get the value of the Inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) 
  {
    x <<-y	
    inverse <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inverse <<- solve
  getInverse <- function()inverse
  list( set= set, get= get, setInverse = setInverse, getInverse = getInverse)

}


##The following function get the inverse of the matrix created with the above function. 
##However,it first checks to see if the inverse has already been calculated.
## If so, it gets theinverse of the matrix from the cache and skips the computation.
## Otherwise, it calculates the inverse matrix and sets the value of the inverse matrix in the cache via the setmean function.

cacheSolve <- function(x, ...) {
  inverse <-x$getInverse()
  if(!is.null(inverse)) {
    message("getting cache data")
    return(inverse)
  }
   data <-x$get()
   inverse <- solve(data)
  x$setInverse(inverse)
  inverse
}


##Sample output
##> x <- matrix(c(2,2,3,2), nrow=2, ncol=2)
##> x
##     [,1] [,2]
##[1,]    2    3
##[2,]    2    2
## my_matrix <-makeCacheMatrix(x)
##> my_matrix$get()
##     [,1] [,2]
##[1,]    2    3
##[2,]    2    2
##> cacheSolve(my_matrix)
##     [,1] [,2]
##[1,]   -1  1.5
##[2,]    1 -1.0