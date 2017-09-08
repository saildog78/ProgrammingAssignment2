#### makeCacheMatrix and cacheSolve are two objects that work together on a matrix
#### to reduce the computer workload in situations where a solution might otherwise call for 
#### re-computing the inverse on the matrix when its state hasn't changed since the last computation.
#### 
#### A makeCacheMatrix object is responsible for storing the original matrix and the value of its inverse (the cache).
#### A cacheSolve object operates on a makeCacheMatrix object by computing the inverse of its matrix, if necessary, and 
#### updating the stored value of the makeCacheMatrix object's inverse.
####
#### If the cacheSolve object sees that the makeCacheMatrix object already has a value for the inverse it doesn't compute it,
#### thus saving the compute resources of performing the calculation.


## Write a short comment describing this function
#### makeCacheMatrix stores a matrix and the value of its inverse.  
#### It provides functions for getting/setting its matrix and getting/setting the value of the inverse of the matrix.
#### The matrix can be passed to the makeCacheMatrix object when the object is being instantiated, or, by calling the 'set'
#### function. If the 'set' function is called, the value of the inverse is set to NULL because any previous value will no longer
#### be valid for the new matrix. Also, the cacheSolve object tests the value of inverse for NULL to know whether or not to compute 
#### the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) inverse <<- solve
        getinverse <- function() inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function
#### The cacheSolve object operates on a makeCacheMatrix object. It computes the inverse of the matrix it finds in
#### the makeCacheMatrix object and updates the value of inverse in makeCacheMatrix. If the makeCacheMatrix already
#### has a value for inverse it returns the message "getting cached data" and does not recompute the inverse.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x' using the R solve() function. If x$getinverse() is not NULL
        ## then the inverse has been previously computed and 'cached' so it is not necessary to recompute. Just
        ## return the previously computed value.
        s <- x$getinverse()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setinverse(s)
        s
}
