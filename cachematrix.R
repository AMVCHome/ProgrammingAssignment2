## Functions to caluculate, cache and return inverse of a inversible (square) matrix 
## using built-in R function solve()

## Creates an object that provide methods to cache a given matrix's inverse
makeCacheMatrix <- function(inputMatrix = matrix()) {
    
    ## Initialize variable to hold inverseMatrix 
    inverseMatrix <- NULL
    
    ## Method to set the input matix
    set <- function(y) {
            inputMatrix <<- y
            inverseMatrix <<- NULL
    }

    ## Method to get the input matrix
    get <- function() {
        inputMatrix
    }
    
    ## Method to set inverse of the Matrix
    setInverseMatrix <- function(inversedMatrix) {
        inverseMatrix <<- inversedMatrix
    }
 
    ## Method to get inverseMatrix
    getInverseMatrix <- function(){
        inverseMatrix
    } 
    
    ## Return a list of the methods
    list(set = set, get = get,
         setInverseMatrix = setInverseMatrix,
         getInverseMatrix = getInverseMatrix)
}

## Compute the inverse of the matrix returned by makeCacheMatrix method.
## If the inverse was already calculated, cacheSolve method should 
## retrieve from the cache. If inverse is not present in cache, calculate
## and add to cache.

## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
    ## Get inverse matrix from cache
    inverseMatrix <- x$getInverseMatrix()
    
    ##Check if inverse is retrieved from cache
    if(!is.null(inverseMatrix)) {
        message("getting cached data")
        return(inverseMatrix)
    }
    
    ##inverse is not in cache, so need to be calculated
    givenMatrix <- x$get()
    
    ##using library function solve() to calculate inverse 
    inverseMatrix <- solve(givenMatrix)
    
    ##set calculated inverse into cache
    x$setInverseMatrix(inverseMatrix)
    
    ##return inverseMatrix
    inverseMatrix
}
