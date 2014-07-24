## A set of functions to create and use a matrix with cached inverse

## this function creates a new cache matrix that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
	inverse <- NULL
	self <- NULL
	## replace original matrix
	set <- function(y) {
			x <<- y
			inverse <<- NULL
	}
	## get original matrix object
	get        <- function() { x }
	##  set inverse
	setInverse <- function(inv) { inverse <<- inv }
	##  get inverse
	getInverse <- function() { inverse }
	## solve and return the inverse of the matrix using cacheSolve
	solve      <- function() { cacheSolve(self) }
	## some tricks to save internal reference to self Object
	self <- list(
		set = set,
		get = get,
		getInverse = getInverse,
		setInverse = setInverse,
		solve = solve
	)
	self
}


## Solve a cacheMatrix if needed and set the cache for it
cacheSolve <- function(x, ...) {
	inverse <- x$getInverse()
	## we only need to calculate the inverse if its null
	if(is.null(inverse)) {
		inverse <- solve(x$get())
		x$setInverse(inverse)
	}
	inverse
}