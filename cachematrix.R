## This script creates two functions: makeCacheMatrix and cacheSolve
## These functions can store a matrix and cache it's mean
## They can be used to prevent repetitive calculation of an inverse

## makeCacheMatrix takes a matrix as an input.
## It turns this into a 'special' matrix, actually a list.
## This list contains both the matrix and, when calculated,
## the inverse of the input matrix

makeCacheMatrix <- function(x = matrix()) {
		
		# m is the variable that will contain the inverse
		# once it is calculated. Initially, it is set to null
		m <- NULL
		
		# set allows for changing the input matrix
        set <- function(y) {
                x <<- y
				# the mean is cleared when the matrix is set
                m <<- NULL
        }
		# get allows for retrieval of the stored matrix
        get <- function() x
		# setinverse allows for storage of the inverse
        setinverse <- function(inverse) m <<- inverse
		# getinverse allows for retrieval of the inverse
        getinverse <- function() m
		
		# return the list containing the above functions
		# and embedded in the functions, the input matrix
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve requires a 'special' matrix as input.
## use makeCacheMatrix to create a 'special matrix.
## cacheSolve returns the inverse of the matrix.
## It will retrieve the cached inverse if available.
## otherwise, it will calculate the inverse and
## it will store the inverse in the 'special' matrix.
## Extra arguments can be passed on to the 'solve' function
## via the ",..." arguments

cacheSolve <- function(x, ...) {
        # retrieve the inverse from the 'special' matrix.
		m <- x$getinverse()
		# if the inverse has been set, return the cached inverse
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
		# if the inverse is not yet set, retrieve the input matrix		
        data <- x$get()
		# solve for the inverse of the input matrix
        m <- solve(data, ...)
		# store the inverse in the 'special' matrix
        x$setinverse(m)
        m
        ## Return a matrix that is the inverse of 'x'
}
