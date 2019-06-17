## class to store matrix and inverse matrix 
## and functions to set and get those objects

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        ix<-matrix()
        set <- function(y) {
                x <<- if (is.matrix(y)) y else message("parameter must be a matrix!")
                ix <<- NULL
        }
        get <- function() x
        setinverse <- function(out_ix) ix <<- if (is.matrix(out_ix)) out_ix else message("parameter must be a matrix!")
        getinverse <- function() ix
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## take inversion of matrix from object of class makeCacheMatrix
## if does not exist compute it according to algebra rules and store it inside makeCacheMatrix class 
## test whether inversion of matrix stored in makeCacheMatrix is correct i.e. does not contain NA values

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ix<-x$getinverse()
        if(prod(!is.na(ix))) {
                message("getting cached data")
                return(ix)
        }
        data <- x$get()
        ix <- solve(data, ...)
        x$setinverse(ix)
        ix
}
