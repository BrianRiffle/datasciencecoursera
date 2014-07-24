## The following pair of functions can create a matrix to cache the
## inverse so to save the computational resources needed to
## recalculate the inverse for the same matrix more than once.

## This function that stores the inverse of a given matrix.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL  ## sets value of m to null
        set <- function(y) {
                x <<-y 
                m <<- NULL
        }  ## sets the value of the matrix
        get <- function()x  ## gets the value of the matrix
        setinverse <- function(solve) m <<- solve  ## sets the  inverse
        getinverse <- function() m  ## gets the inverse
        list(set = set, get=get, 
             setinverse = setinverse,
             getinverse = getinverse)
}



## This function computes the inverse of the matrix returned above.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <-x$getinverse()
        if(!is.null(m)) {
                message("getting cachaed data")
                return(m)
                       
        }  ## returns message and inverse matrix if the matrix has 
           ## been cached
        data <-x$get()
        m <- solve(data,...) ## calculates inverse if not found
        x$setinverse(m)
        m
      }
