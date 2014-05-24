## Put comments here that give an overall description of what your
## functions do

################################################################################
#  Method makeCacheMatrix 
#     creates an object that stores the matrix
#     and a cached version of the inverse of that matrix.
#  Use methods: 
#     set(x) given matrix var x
#     get()  returns matrix
#     setInvMatrix(iM) sets the inverse matrix to iM
#     getInvMatrix() returns the inverse matrix if cached, or NULL if not set.
#
#     See the method cacheSolve() below to obtain obtain the inverse matrix
################################################################################

makeCacheMatrix <- function(x = matrix()) {
    
    regMatrix <- x
    invMatrix <- NULL

    set <- function(y) {
        regMatrix <<- y
        invMatrix <<- NULL
    }

    get <- function() regMatrix

    setInvMatrix <- function(iM) invMatrix <<- iM

    getInvMatrix <- function() invMatrix

    cacheMatrix <- list(set=set, get=get, 
		 			   setInvMatrix=setInvMatrix, 
		 			   getInvMatrix=getInvMatrix)
    
	return(cacheMatrix)  # I prefer explicit return statements.
    
}


################################################################################
# Method cacheSolve
#    Given a 'makeCacheMatrix' object, returns the inverse matrix if already
#    cached, or computes, caches, and then returns it.  
#
################################################################################


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

	iM <- x$getInvMatrix()

	if (! is.null(iM)) {
		message("getting cached inverse matrix")
		return(iM)
	}
	else {
		# not yet cached the inv matrix, so do it now

		regMatrix <- x$get()

		iM <- solve(regMatrix, ...)
		
		x$setInvMatrix(iM)

		return(iM)
	}
	
}

########################################
## Example usage
#
# > source("cachematrix.R")
# > myMatrix = matrix(c(1,2,3,4), nrow=2)
# > m = makeCacheMatrix(myMatrix)
# > cacheSolve(m)
#      [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
# > cacheSolve(m)
# getting cached inverse matrix
#      [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
#
#########################################

