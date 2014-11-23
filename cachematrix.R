## R Programming Assignment 2. These are functions to get the inverse of a matrix while  ensuring
## that the costly computation is one performed one for any particular matrix by employing cache


## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        
        set <- function(m){
                x <<- y
                inverse <<- NULL
        }
        
        get <- function() x
        
        setInverse <- function(i) inverse  <<- i

        getInverse <- function() inverse
        
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)

}


## This function computes the inverse of the special "matrix" returned by `makeCacheMatrix` above. If the inverse has
##already been calculated (and the matrix has not changed), then 
##`cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        inverse <- x$getInverse()        
        
        if(!is.null(inverse)){
                message("Getting the inverse of the matrix from cache")
                return(inverse)
        }
        
        mat <- x$get()
        inverse <- solve(mat)
        x$setInverse(inverse)
        inverse
}
