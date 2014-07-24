## Put comments here that give an overall description of what your
## functions do

##	makeCacheMatrix is a list of function attatched to a matrix(set, get, 
##	setInverse and getInverse)
#################################################################################


## Write a short comment describing makeCacheMatrix 
##	set: assign a matrix to x and reset to matrix Inverse to NULL
##
##	get: return the matrix
##
##	setInverse: assign the matrix to matrixInverse
##	
##	getInverse: returns the matrix inverse 
#################################################################################

makeCacheMatrix <- function(x = matrix()) {
	
	matrixInverse <- NULL

	set<- function(y){
		x<-- y
		matrixInverse <-- NULL
	}
	
	get <- function()x

	setInverse <- function(inverse){
		matrixInverse <<- inverse
	}

	getInverse <- function(){
		matrixInverse 
	}

	list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing cacheSolve 
## the function calls getInverse to retrieve the cached value.
## If the value is NULL, it calculates the inverse using solve(), cache 
## the result and return the value 
## If not null, it returns the cached inverse matrix
#################################################################################

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

	matrixInverse <- x$getInverse()
	if(!is.null(matrixInverse)){
		message("getting cached data")
		return(matrixInverse)
	}
	matrixData <- x$get()
	matrixInverse<- solve(matrixData, ...)
	x$setInverse(matrixInverse)
	matrixInverse
}


## Example in R console
## > source("cachematrix.R")
## > a<-makeCacheMatrix(matrix(c(1,0,0,5,1,2,1,2,0),3,3))
## > a$get()
##      [,1] [,2] [,3]
## [1,]    1    5    1
## [2,]    0    1    2
## [3,]    0    2    0
## > a$getInverse()
## NULL
## > cacheSolve(a)
##      [,1] [,2]  [,3]
## [1,]    1 -0.5 -2.25
## [2,]    0  0.0  0.50
## [3,]    0  0.5 -0.25
## > cacheSolve(a)
## getting cached data
##      [,1] [,2]  [,3]
## [1,]    1 -0.5 -2.25
## [2,]    0  0.0  0.50
## [3,]    0  0.5 -0.25
## > cacheSolve(a) %*% a$get() ## to check the result is correct
## getting cached data
##      [,1] [,2] [,3]
## [1,]    1    0    0
## [2,]    0    1    0
## [3,]    0    0    1
## > a$getInverse()
##      [,1] [,2]  [,3]
## [1,]    1 -0.5 -2.25
## [2,]    0  0.0  0.50
## [3,]    0  0.5 -0.25
## > 

