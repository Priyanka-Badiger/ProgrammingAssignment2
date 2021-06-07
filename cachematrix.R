##These two functions help get inverse of a sqare matrix in cache environment

##This function gives a list, that is later used by cacheSolve 
makeCacheMatrix <- function(x = matrix()) {
     cache <- NULL
     # create the matrix in the working environment
     set <- function(y) {
                x <<- y
                cache <<- NULL
        }
     get <- function() x
     setMatrix <- function(inverse) cache <<- inverse
     getInverse <- function() cache
     # return the created functions to the working environment
     list(set = set, get = get,
             setMatrix = setMatrix,
             getInverse = getInverse)
}


 ## Returns a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
      ## attempt to get the inverse of the matrix stored in cache
       cache <- x$getInverse()
     # return inverted matrix from cache if it exists
        # else create the matrix in working environment
       if (!is.null(cache)) {
                message("getting cached data")
               return(cache)
        }
      # create matrix since it does not exist
       matrix <- x$get()
     # make sure matrix is square and invertible
        # if not, handle exception cleanly
       tryCatch( {
                cache <- solve(matrix, ...)
        },
        error = function(e) {
                message("Error:")
                message(e)

                return(NA)
        },
        warning = function(e) {
                message("Warning:")
                message(e)

                return(NA)
        },
        finally = {
                x$setMatrix(cache)
        } )
        return (cache)
}
