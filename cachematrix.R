## Put comments here that give an overall description of what your
## functions do
## Functions for creating and using inverted matrices with caching mechanism

## This makeCacheMatrix function is used to get/set the matrix data and 
## compute the inverse matrix data

makeCacheMatrix <- function(x = matrix()) {
  
  ## Creates a list of functions that
  ## can get/set the matrix data as well as the inverse of the matrix data
  
        m <- NULL

        ### 1. set the value of the matrix 
        set <- function(y) {
                x <<- y
                m <<- NULL
        }

        ### 2. get the value of the matrix provided
        get <- function() x

	### 3. set the value of the inverse of the matrix
        setsolve <- function(solve) m <<- solve

	### 4. get the value of the inverse of the matrix
        getsolve <- function() m

        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}

## Write a short comment describing this function

## This function returns the cached inverse matrix if it is already 
## else it gets computed by makeCacheMatrix function and cached for the future use.

cacheSolve <- function(x, ...) {
        # Return a matrix that is the inverse of 'x'

        m <- x$getsolve() 
        
        # Do we have the cached inverse matrix available?
        if(!is.null(m)) { # we got something...
                message("getting cached inverse matrix data")
                return(m)
        }

        # Create an inverse matrix in case it is not present in the cache
        
        data <- x$get()       # get it
        m <- solve(data, ...) # solve it
        x$setsolve(m)	      # set it
        m		      # return it
}
