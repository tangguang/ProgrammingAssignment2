## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
	  
    ## Initialize the inverse matrix
    inv <- NULL

    ## Set the matrix
    set <- function(y) {
            x <<- y
            inv <<- NULL
    }

    ## Get the original matrix
    get <- function() x

    ## Set the inverse of the matrix
    setInverse <- function(inverse) inv <<- inverse

    ## Get the inverse of the matrix
    getInverse <- function() inv

    ## Return a list of the methods
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
      
      ## Check if the inverse matrix has been calculated
      inv <- x$getInverse()

	    ## Return the inverse if its already set
	    if(!is.null(inv)) {
	            message("getting cached data")
	            return(inv)
	    }

	    ## Get the original matrix from our object
	    data <- x$get()

	    ## Calculate the inverse
	    inv <- solve(data, ...) %*% data

	    ## Set the inverse to the object
	    x$setInverse(inv)

	    ## Return the inverse matrix
	    inv
}
