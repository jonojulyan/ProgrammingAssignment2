## The makeCacheMatrix function creates an object that can store the matrix with it's inverse.
## The cacheSolve functions returns the inverse of the matrix.

## makeCache function defines functions to set and get the matrix and the inverse of that matrix.
## The inverse of the matrix is set to null when the matrix in changed or the object is created.

makeCacheMatrix <- function(x = matrix()) {
    matInv <- NULL
    set <- function(y) {
        mat <<- y
        matInv <<- NULL
    }
    get <- function() mat
    setInv <- function(i) matInv <<- i
    getInv <- function() matInv
    list(set = set, get = get, getInv = getInv, setInv = setInv)
}


## If the x object has the inverse matrix stored in it then it returns that matrix
## otherwise it will recalculate the inverse.
## Note in makeCacheMatrix if the matrix is changed the invese value is set to NULL.

cacheSolve <- function(x,...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getInv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    if(dim(data)[1]==dim(data)[2]){
        m <- solve(data)
    }
    else{ 
        ## If the matrix is not a square we cannot find the inverse
        message("Error: not a square matrix")
    }
    x$setInv(m)
    m
}
