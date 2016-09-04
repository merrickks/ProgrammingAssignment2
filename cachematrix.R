## Creates a matrix and defines helper functions
makeCacheMatrix <- function(x = matrix()) {
     invMatrix = NULL
     set = function(y) {
          x <<- y
          invMatrix <<- NULL
     }
     get = function() x
     setInverse = function(inverse) invMatrix <<- inverse 
     getInverse = function() invMatrix
     list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}

## Calls makeCacheMatrix and checks if inverse has already been solved
cacheSolve <- function(x, ...) {
     ## Calls the getter helper function of makeCacheMatrix 
     invMatrix = x$getInverse()
     ## Check if the cached matrix exists
     if (!is.null(invMatrix)){
          ## If found, retrieve cached matrix 
          message("Cached matrix found!")
          return(invMatrix)
     }
     ## If the cached matrix is null, solve the inverse
     solveMatrix = x$get()
     invMatrix = solve(solveMatrix, ...)
     ## Call the setter function to store the inverse matrix in cache
     x$setInverse(invMatrix)
     return(invMatrix)
}