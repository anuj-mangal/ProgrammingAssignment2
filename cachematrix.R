## function makeCacheMatrix sets up a list which contains 4 elements. The first one makes a call to set function to set a matrix, 
## the second one get,gets the matrix, the third and fourth one set and get the inverse of the matrix
## the second function cachesolve creates the invserse of the matrix

## This function creates a list of elements which sets and gets a matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
            i <- NULL
            set <- function(y){
              x <<- y
              i <<- NULL
            }
            get <- function() x
            setinverse <- function(inverse)  i<-inverse
            getinverse <- function() i
            list(set = set, get = get , setinverse = setinverse , getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if (!is.null(i)){
          message("Returning the cached Inverse")
          return(i)
        }
        data_matrix <- x$get()
          i <- solve(data_matrix,...)
          x$setinverse(i)
          i
}
