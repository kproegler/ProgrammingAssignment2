# Coursera R programming online class
# Week 3 - Programming Assignment
# 2016-05-07
# K Proegler
# 
# This pair of functions calculates the inverse of a matrix, and for each unique
# matrix does it only once.  The intended use is situations in which program logic
# results in repeated calls (such as in nested loops) for inversion of the same
# matrix.
#
# With this pair of functions, the potentially resource-intensive work of 
# inversion is done only once.
#
# Use:
#	  Given: mymatrix is a square, invertable matrix
#
#     cachedmatrix   <- makeCacheMatrix(mymatrix) 
#     invertedmatrix <- cacheSolve(cachedmatrix) # call cacheSolve with the result 
#                                                  of makeCacheMatrix
#
# Design: Approach and code re-used without shame from the course-provided example
#         for doing similar for mean of a vector.
#
#         They depend on "<<-" assignment operator, allowing assignment that
#         are in environments other than the current one.  These are types of
#         globals. A nice reference on this and the concept of closures can be found at
# http://stackoverflow.com/questions/2628621/how-do-you-use-scoping-assignment-in-r
#
#         Variable names were changed to add to understanding - primarily mine
#         initially.   
#

makeCacheMatrix <- function(It = matrix()) {
#   This constructs functions for creating and using the cache.  
#       setIt, getIt : store and retrieve, respectively, the matrix.  Consider 
#                      this as the "key" to look up saved values.  Very much
#                      like a perl hash
#       setSoln, getSoln : store and retrieve, respectively, the inverted matrix.
#  
#       "It" and "Soln" are purposefully independent of the object class and the
#                       operation generating the results to be cached.  This 
#                       makes it easier to use as a pattern for other, similar
#                       paired functions.
#    
#       See function "cacheSolve", which consumes the output of this function
#          
	ItsSoln <- NULL			 # The solution (inverted matrix). Set initially to null
	setIt  <- function(y) {  # Initialize object with its value and a null solution
			It 		<<- y	 #      its value is passed
			ItsSoln <<- NULL #      its solution initially null
	} 
	getIt	 <- function() It	 #  Get the object
	setSoln <- function(solve) ItsSoln <<- solve  # Set the value of the solution
	getSoln <- function() ItsSoln 			     # Get the value of the solution
	## returned is a list of these 4 functions
	list(setIt=setIt, getIt = getIt, setSoln = setSoln, getSoln = getSoln)
}

cacheSolve <- function(It, ...) {
# 
# This function returns a matrix that is the inverse of that in "It"
# If an inverse for "It" has previously been calculated, it is retrieved from
# a cache, avoiding additional calculations.
# If "It" is being seen for the first time, the calculation is done.  It is
# stored in the cache as well as returned.
#       See function "cacheSolve", which creates the input to this function

            ItsSoln <- It$getSoln()
            if(!is.null(ItsSoln)) {
                    message("getting cached data")
                    return(ItsSoln)
            }
            data <- It$getIt()
            ItsSoln <- solve(data, ...)
            It$setSoln(ItsSoln)
            return(ItsSoln)
}
