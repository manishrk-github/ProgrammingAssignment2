## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        solveM <- NULL
	set <- function(y) {
		x <<- y
		solveM <<- NULL
	}
	get <- function() x
	setSolvedMatrix <- function(solvedMatrix) solveM <<- solvedMatrix
	getSolvedMatrix <- function() solveM
	list(set = set, get = get, setSolvedMatrix = setSolvedMatrix,
	     getSolvedMatrix = getSolvedMatrix)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        solvedMatrix <- x$getSolvedMatrix()
	  if(!is.null(solvedMatrix)) {
		message("getting cached data")
		return(solvedMatrix)
	  }
	  dataMatrix <- x$get()
	  solvedMatrix <- solve(dataMatrix)
	  x$setSolvedMatrix(solvedMatrix)
	  solvedMatrix
}
