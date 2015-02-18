##
## R Programming Assignment 2
##
## To demonstrate Lexical Scoping and the use of the "<<-" assignment operator
##
## This function creates a special "matrix" object that can cache
## its inverse. To obtain the cached inverse of the matrix, use the
## companion cacheSolve() function defined in the file below.
##
## This function is patterned after the makeVector() function given for this
## programming assignment.
##
## For this assignment, we assume the matrix supplied is always invertible.
## If not, the cacheSolve() function will report an error.
##
## Usage: 
## ======
## makeCachedMatrix(x) - Creates a cache matrix object using values of the
##              supplied matrix. 
##              The matrix is assumed to be a square, invertible matrix.
##              No error is given if it is not invertible at object creation 
##              time.
##
## $set(x)    - Use this function to set or change the cached matrix stored
##              by this makeCacheMatrix object. 
##              x represents the new value for the matrix to be stored.
##              Again, the supplied matrix is not checked if it is invertible.
##
## $get()     - Use this function to get the value of the matrix stored.
##
## $getInv()  - This function is intended to be used by the companion 
##              cacheSolve() function to fetched the inverse that was 
##              stored. If this value is NULL, cacheSolve() with proceed
##              to obtain the inverse of the cached matrix by.
##
##              Users of makeCacheMatrix object should not use this function
##              to get the stored inverse unless they know for sure an 
##              inverse has been cahed in the object. It is safer to call
##              cacheSolve to obtain the inverse if one is unsure about this.
##
## $setInv(m) - This function is intended to be used by the companion 
##              cacheSolve() function to store the inverse in the object.
##              m represents the inverse to be stored.
##
##              Users of makeCacheMatrix should NOT use this function to
##              change the value of the cached inverse other than to set
##              it to NULL for the purpose of forcing a recalculation of
##              the inverse next time cacheSolve() is called with the object.
##              
makeCacheMatrix <- function( x = matrix() ) {
    
    x_inv <- NULL
    
    set <- function(y) {
            x <<- y
            x_inv <<- NULL
    }
    
    get <- function() x
    
    setInv <- function( xInv ) x_inv <<- xInv
    
    getInv <- function() x_inv
    
    list( set = set, get = get, setInv = setInv, getInv = getInv )
}

##
## This function computes the inverse of the special "matrix" returned
## by makeCacheMatrix function above. If the inverse has already been 
## calculated (and the matrix has not changed), then the function returns 
## the inverse from the cache.
## 
## The function uses the solve() function in R to calculate the inverse.
## If the matrix stored in the makeCacheMatrix object is not invertible,
## an error will be returned.
##
## Besides the makeCacheObject x, all other arguments passed to cacheSolve
## through the ... argument are passed to the R solve() function unchaged.
## See the documentation on solve() for details.
##
cacheSolve <- function( x, ... ) {
    
    ## Returns a matrix that is the inverse of the matrix stored inside
    ## the object returned by a makeCachMatrix() call.
    ##
    ## x - An object returned by a makeCacheMatrix() call.
    ##
    xInv <- x$getInv()
    if( !is.null( xInv ) ) {
            message("getting cached data")
            return( xInv )          # Return the cached inverse
    }
    
    ## Fall through to here if the inverse of the cache matrix has not
    ## been set. Calls solve() to compute the inverse and returns the
    ## result after storing the computed inverse in the cache matrix.
    ##
    data <- x$get()
    dataInv <- solve( data, ... )   # Call solve() to compute the inverse
    x$setInv( dataInv )             # Store the computed inverse in x
    dataInv                         # Return the computed inverse
    
}
