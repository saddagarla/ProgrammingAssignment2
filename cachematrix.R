## Put comments here that give an overall description of what your
## functions do

## makeVector creates a special "vector", which is really a list containing
## a function to

#    set the value of the matrix
#    get the value of the matrix
#    set the value of the inverse
#    get the value of the inverse

##  The big assumption this function makes is that the provided 
##  matrix is invertible.

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y){
		x   <<- y
		inv <<- NULL
	}
  
	get <- function()  x
	setinverse <- function(minverse) inv <<- minverse
	getinverse <- function() inv
	list(set = set, get = get, 
	   setinverse = setinverse, 
	   getinverse = getinverse)
}


## The following function calculates the inverse of the special "matrix" 
## created with the above function. However, it first checks to see if the 
## inverse has already been calculated. If so, it gets the inverse from the 
## cache and skips the computation. Otherwise, it calculates the inverse of 
## the matrix and sets the value of the inverse in the cache via the setinverse
## function.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if (!is.null(inv)){
      message("getting cached inverse")
      return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
