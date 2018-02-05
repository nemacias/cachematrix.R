makeCacheMatrix <- function(x = matrix()) { ## makeCacheMatrix creates a matrix that can cache its inverse
        g <- NULL
        set <- function(matrix) {
                x <<- matrix
                g <<- NULL      ## Then, inicialize the inversion, set the matrix (set), get the matrix (get).
        
        }
        get <- function() 
                x
        setinverse <- function(inverse) 
                g <<- inverse  ## Set and get the inverse of the matrix
        getinverse <- function() 
                g
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)  ## creates a list
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        g <- x$getinverse()
        if(!is.null(g)) {  ## check if the inversed has been calculated
                message("getting cached data")
                return(g)
        }
     mat <- x$get() ## get the matrix and calculate the inverse
     g <- solve(mat, ...)
     x$setinverse(g) ## set the invserse of g and return the matrix
     g
}
