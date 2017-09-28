## The following pair of functions cache the inverse value of a matrix.

## The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to
## set the value of the matrix
## get the value of the matrix
## set the inverse value of the matrix
## get the inverse value of the matrix

makeCacheMatrix <- function(x = matrix()) {
        
        m<- NULL
        set<- function(y){ ## set the value of the matrix
                x<<- y
                m<<- NULL
        }
        get<- function() x ## get the value of the matrix
        setinverse<- function(solve) m<<- solve ## set the inverse value of the matrix
        getinverse<- function() m ## get the inverse value of the matrix
        list(set=set, get=get,
             setinverse=setinverse,
             getinverse=getinverse)
}

## The second function, cacheSolve, calculates the inverse value of the special matrix created with the makeCacheMatrix function. 
## However, it first checks to see if the inverse value of the matrix has already been calculated. If so, it gets the inverse value
## from the cache and skips the computation. Otherwise, it calculates the inverse value of the data and sets the value in the cache 
## via the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        m<- x$getinverse()
        if(!is.null(m)){ ## If the inverse value of the matrix is in cache, skip the computation and return the stored value.
                message("getting cached data")
                return(m) 
        }
        ## If the inverse value of the matrix in not in cache, compute the value and set the value in the cache.
        data<-x$get() 
        m<- solve(data, ...)
        x$setinverse(m)
        m
}
