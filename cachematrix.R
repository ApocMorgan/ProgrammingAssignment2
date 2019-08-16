##Calculating and Caching the Inverse of a Matrix


##makeCacheMatrix -- creates a special "vector", which is really a list containing a function to:
##set the value of the matrix
##get the value of the matrix
##set the value of the inversed matrix
##get the value of the inversed matrix
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m 
        list(set=set,get=get,
             setinverse=setinverse,
             getinverse=getinverse)

}


##cacheSolve -- calculates the inverse of the matrix contained in the special "vector" 
## created with the above function. However, it first checks to see if the inverse 
## has already been calculated

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inver <- x$getinverse()
        if(!is.null(inver)){
                message("getting cached data...")
                return(m)
        }
        data <- x$get()
        result <- solve(data)
        x$setinverse(result)
        result
}
