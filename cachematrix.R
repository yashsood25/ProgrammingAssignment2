## Caching the inverse of a matrix

## Creates a special matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
	m<- NULL
	set<- function(y) {
		x<<-y
		m<<-NULL
	}
	get<- function()x
	setinverse<- function(solve) m<<-solve
	getinverse<- function() m
	list( set=set, get=get, 
	setinverse=setinverse, 
	getinverse=getinverse)
}


## Calculates the inverse of the special matrix created with the above function

cacheSolve <- function(x, ...) {
	inv<-x$getinverse()
	if(!is.null(inv)) {
		message("getting cached data")
		return(inv)
	}
	data<- x$get()
	inv<-solve(data,...)
	x$setinverse(inv)
	inv
        ## Return a matrix that is the inverse of 'x'
}