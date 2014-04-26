x## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## i am not sure whether i get the concept of this functions, but i try to do this ;-)

makeCacheMatrix <- function(x = matrix()) {
    ## initalize
    myInvMatrix <- NULL
    
    ## define set function
    set <- function(y) {
        ## assign the argument of set to the matrix, stored in general environment
        x <<- y
        
        ## set "empty cache" for inverted matrix
        myInvMatrix <<- NULL
    }
    
    ## define the get function, return cached matrix
    get <- function() x
    
    ## set the cache of inverted matrix with given argument
    setInvertedMatrix <- function(inv_Matrix) myInvMatrix <- inv_Matrix
    
    ## return the cached version of inverted matrix
    getInvertedMatrix <- function() x
    
    list(set = set, get = get, 
         setInvertedMatrix = setInvertedMatrix, 
         getInvertedMatrix = getInvertedMatrix)
}



## Write a short comment describing this function
## the inverse matrix is computed with solve, whch only works when input is a squared matrix
## it is not checked whether nrol(x) = ncol(x)
##
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    ## get potentially cached inverse matrix
    inv_matrix <- x$getInvertedMatrix()
    
    # if a matrix was cached already, the result is not NA
    if (!is.na(inv_matrix)) {
        message("getting cached inverted matrix")
        return(inv_matrix)
    }
    
    ## get data of the matrix
    myMatrix <- x$getMatrix()

    ## this only works for a square matrix, but it is a start
    inv_matrix <- solve(myMatrix)

    ## set the cache with inverted matrix
    x$setInvertedMatrix(inv_matrix)
    
    return(inv_matrix)
}
