## Implementation to cache inverse of a matrix.
## Implementation has two functions makeCacheMatrix and cacheSolve.

## makeCacheMatrix keeps track of the matrix for which inverse is already available.

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


## When Matrix is provided as input in x cacheSolve will check if the 
## Matrix is already solved it is returned if not we store the input matrix in cache. 

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
