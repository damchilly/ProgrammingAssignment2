## These functions compute the inverse of a square matrix in R.

## makeCacheMatrix creates a special matrix object that can cache its inverse.


makeCacheMatrix <- function(x = matrix()) {
  
  inv_m <- NULL #Initialise inverse matrix holder
  #Define set
  set <- function(y) {
    x <<- y
    inv_m <<- NULL
  }
  ## getting the value of the matrix
  get <- function() x        
  # setting the value of the matrix
  setinverse <- function(solve) inv_m <<- solve
  # getting the vaue of the inverse matrix
  getinverse <- function() inv_m
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## cacheSolve computes the inverse of the matrix 
## If the inverse has already been calculated 
## the cachesolve retrieves the inverse.


cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  inv_m <- x$getinverse()
  if(!is.null(inv_m)) {
    message("got cached matrix")
    return(inv_m)
  }
  matrixdata <- x$get()
  inv_m <- solve(matrixdata)
  x$setinverse(inv_m)
  inv_m   
  
}

## Example of my solution at work

##Browse[2]> m <- matrix(c(-1, -2, 1, 1), 2,2)
##Browse[2]> m
##     [,1] [,2]
## [1,]   -1    1
## [2,]   -2    1
##Browse[2]> x <- makeCacheMatrix(m)
##Browse[2]> x
##$set
##function (y) 
##{
##   x <<- y
##    inv_m <<- NULL
##}
##<environment: 0x105a01860>
##
##$get
##function () 
##x
##<environment: 0x105a01860>

##$setinverse
##function (solve) 
##inv_m <<- solve
##<environment: 0x105a01860>

##$getinverse
##function () 
##inv_m
##<environment: 0x105a01860>
##Browse[2]> x$get()
##    [,1] [,2]
## [1,]   -1    1
## [2,]   -2    1
##Browse[2]> inv <- cacheSolve(x)
##Browse[2]> inv
##    [,1] [,2]
## [1,]    1   -1
## [2,]    2   -1
##Browse[2]> inv <- cacheSolve(x)
##got cached matrix
##Browse[2]> inv
##     [,1] [,2]
## [1,]    1   -1
## [2,]    2   -1





