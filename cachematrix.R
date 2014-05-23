## Philip Coyne
## May 23, 2014
## R Programming Course 
## Assignment 2

## The following function, makeCacheMatrix, will take a matrix and store it
## This is accomplish by a series of functions nested within makeCacheMatrix.

makeCacheMatrix <- function(x = matrix()) {
	## Variable inverse is initialized as NULL
	inverse<-NULL	

	## Function set takes an input of mat and stores it to x as a global variable
	## That is to say, this x is a different variable than the x inputed at the beginning of the function
	set<-function(mat){
		x<<-mat
		inverse<<-NULL	
	}

	## The following functions are intended to format the output into a list format 
	## with the intention of easy retrieval in the following function "cacheSolve"

	get<- function()x
	setInverse<-function(inputInverse) inverse<<-inputInverse
	getInverse<- function() inverse
	list( set=set, get=get, setInverse=setInverse, getInverse=getInverse)

}


## The function cacheSolve is intended to retrieve the list output from "makeCacheMatrix"
## and return the inverse of that matrix.  It is intended that if the matrix has already been calculated
## then a message displaying "getting cached data" should appear and output the same inverse matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inverse<-x$getInverse()
	if(!is.null(inverse)){
		message("getting cached data")
		return(inverse)
	}
	matrix<-x$get()
	inverse<-solve(matrix,...)
	x$setInverse(inverse)
	inverse
}
