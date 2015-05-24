## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then cacheSolve should retrieve the inverse from the cache.


## The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to
## -set the value of the matrix
## -get the value of the matrix
## -set the value of the inverse
## -get the value of the inverse

makeCacheMatrix <- function(x = matrix()) 
{
	xinv <- NULL
	set <- function(y)
	{
		x <<- y
		xinv <- NULL
	}
	get <- function() 
	{
		x
	}
	setinv <- function(inverse)
	{
		xinv <<- inverse
	}
	getinv <- function()
	{
		xinv
	}
	list(set = set , get = get , setinv = setinv , getinv = getinv)
}


## The following function calculates the inverse of the special "matrix" created with the above function. 
## However, it first checks to see if the inverse has already been calculated.
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the matrix and sets the value of the inverse in the cache via the setinv function.

cacheSolve <- function(x, ...) 
{
	xinv <- x$getinv()
	if(!is.null(xinv))
	{
		return(xinv)
	}
	data <- x$get()
	xinv <- solve(data,...)
	x$setinv(xinv)
	xinv
}
