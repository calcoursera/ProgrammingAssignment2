
## The following functions use the <<- operator to assign values to matrix objects in an environment that serves as a cache 
## and is different from the current environment.  This is used to store the inverse of a given matrix so that the calculation 
## which is intensive for large matrices does not have to be repeated everytime, especially when it the original matrix is
## unchanged.   

## example run :   x<-matrix(c(1,2,2,1),nrow=2,ncol=2)
## a<-makeCacheMatrix(x)
## cacheSolve(a)



## The functon "makeCacheMatrix" creates a special vector which is a ready list containing functions to set
## the values of the matrix, get the values of the matrix, set the values of the inverse matrix and get the values of the
## inverse matrix;  



makeCacheMatrix <- function(x = matrix()) {
	m<-NULL

	set<-function(y){
		x<<-y
		m<<-NULL
	}	

	get<-function()x

	setinverse<-function(solve)
		m<<-solve

	getinverse<-function()m
	list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)

}


## The "cacheSolve" function calculates the inverse of a given matrix using the solve function.  Before calculating, it
## checks to see if the inverse has already calculated, in which case, it "get"s it from the cache and skips calculating.
## If this is not the case, it will compute the inverse and stores it in the cache using the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

	m<-x$getinverse()

	if (!is.null(m)){
                message("Getting cached matrix inverse")
		return(m)
	}

	data<-x$get()
	
	m<-solve(data,...)

	x$setinverse(m)
	
	m
}
