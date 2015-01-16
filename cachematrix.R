## function returns a cached matrix

makeCacheMatrix <- function(x = matrix()) {
    invMtrx <- NULL
    #sets matrix values and reseting invmatrix to null
    set <- function(y){
        x <<- y
        invMtrx <<- NULL
    }
    get <- function()  x
    getInverse <- function() invMtrx
    setInverse <- function(invM){
        invMtrx <<- invM
    }
    #naming methods
    list(set = set, get = get,
         getInverse = getInverse,
         setInverse = setInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
     inv <- x$getInverse()
     if(!is.null(inv)){
         message("cached getting data")
         return(inv)
     }
     data <- x$get()
     #solve function does inversion of matrix
     inv <- solve(data)
     x$setInverse(inv)
     inv
     
        ## Return a matrix that is the inverse of 'x'
}
