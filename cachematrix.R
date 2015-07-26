## The functions in this scripts are used to create a special object
## that store a matrix and cache's its inverse.

## makeCacheMatrix create a special "matrix", which is really a list 
## containing a function to:
## 1.- set the value of the matrix
## 2.- get the value of the matrix
## 3.- set the value of the matrix's inverse
## 4.- get the value of the matrix's inverse

makeCacheMatrix <- function(x = matrix()) {
    minv <- NULL
    
    set <- function(y) {
        x <<- y
        minv <<- NULL
    }
    
    get <- function() x
    
    setinverse <- function(minverse) minv <<- minverse
    
    getinverse <- function () minv
    
    list(set = set, get = get, setinverse = setinverse, 
         getinverse = getinverse)
}


## The following function calculates the matrix's inverse of the special
## "vector" created with the above function. However, it first checks to
## see if the inverse has already been calculated. If so, it gets the 
## inverse from the cache and skips the computation. Otherwise, it 
## calculates the matrix's inverse of the data and sets the value of the
## inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    minverse <- x$getinverse()
    if ( !is.null(minverse) ) {
        message("getting cached data")
        return(minverse)
    }
    
    data <- x$get()
    minverse <- solve(data)
    x$setinverse(minverse)
    minverse
}
