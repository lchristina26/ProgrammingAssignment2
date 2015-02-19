## Put comments here that give an overall description of what your
## functions do

## Function that takes in a matrix and caches it to be retrieved later

makeCacheMatrix <- function(original_m = matrix()) {
	inverse_m <- NULL
	set <- function(y) {
		original_m <<- y
		inverse_m <<- NULL
	}
	get <- function() original_m
	setInverse <- function(inverse) inverse_m <<- inverse
	getInverse <- function() inverse_m
	list(set = set, get = get, setInverse = setInverse, 
			getInverse = getInverse)
}


## Function that takes in a matrix and inverts it

cacheSolve <- function(original_m, ...) {
	inverse_m <- original_m$getInverse()
	## check if inverse has been calculated
	if(!is.null(inverse_m)) {
		message("getting cached data")
		return(inverse_m)
	}
	## if not, calculate the inverse
	data <- original_m$get()
	inverse_m <- solve(data, ...)
	original_m$setInverse(inverse_m)
	inverse_m
}
