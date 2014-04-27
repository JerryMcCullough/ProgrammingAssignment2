##The first function, makeCacheMatrix, Takes a matrix as it's argument
## and calculates the inverse using the solve() function and caches
## the inverted matrix. A vector of 4 fucntions is returned.
##
## The second function cacheSolve, Takes the vector list of functions
## as its input and gets the cached inverted matrix or calculates it.
##
##
## function makeCacheMatrix - Takes a matrix as it's argument and
##	creates a vector list of four functions
##      to store and retrieve the matrix and 
##      it's inverse.  The data is cached so it 
##      does not have to be calulated if it 
##      already exists. 
makeCacheMatrix <- function(x = matrix()) {
   m <- NULL
   set <- function(y) {
   	  x <<- y
   	  m <<- NULL
   }
   get <- function() x
   setinverted <- function(solve) m <<- solve
   getinverted <- function() m
   list(set = set, get = get,
        setinverted = setinverted,
        getinverted = getinverted)
}
## function cacheSolve() takes as input the vector of functions 
##     created in makeCacheMatrix() 
##     cacheSolve() checks for the existence of the inverted matrix 
##     cached in variable m.
##     If it exists the "cached" data is used
##     else it gets the data from x and calculates the inverted matrix
##     and stores it in m.
cacheSolve <- function(x, ...) {
	        ## Return a matrix that is the inverse of 'x'
	m <- x$getinverted()
	if(!is.null(m)) {
		message("getting cached data")
		return(m)
	}
	data <- x$get()
	m <- solve(data, ...)
	x$setinverted(m)
	m
}
## To test these function: 
## create a matrix c.
## > c = rbind(c(4,7), c(2,6))
##> source("matrixsolve.R")
## create the vector of functions
##> im <- makeCacheMatrix(c)
## Calculate the inverse
##> cacheSolve(im)
##     [,1] [,2]
##[1,]  0.6 -0.7
##[2,] -0.2  0.4
##> 
## Run cacheSolve(im) again and it display "getting cached data"
##> cacheSolve(im)
##getting cached data
##     [,1] [,2]
##[1,]  0.6 -0.7
##[2,] -0.2  0.4
##> 
