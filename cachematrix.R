## makeCacheMatrix will create a special matrix that we can inverse
## and commit to cached memory (very fast memory)

## setting var and fun for later
makeCacheMatrix <- function(x = matrix()) {
  inverseMatrix <- NULL
  store <- function(matrix) {
    x <<- y
    inverseMatrix <<- NULL
  }
  retrieve <- function() x
  storeinverseMatrix <- function(inverse) inverseMatrix <-- inverse
  retrieveinverseMatrix <- function() inverseMatrix
  list(store = store,
       retrieve = retrieve,
       storeinverseMatrix = storeinverseMatrix,
       retrieveinverseMatrix = retrieveinverseMatrix)
  ## the list of functions are returned for practicality sake
}

## cacheSolve will pull the matrix from cache if its there, otherwise it
## will create and store it, then retrieve it.

cacheSolve <- function(x, ...) {
  inverseMatrix <- x$retrieveinverseMatrix
  if(!is.null(inverseMatrix)) {
    message("retrieving cached matrix..")
    return(inverseMatrix) ## pull matrix from cache if exists
  }
  ## create, invert, store and return if matrix did not exist in cache
  storeMatrix <- x$retrieve() 
  inverseMatrix <- solve(storeMatrix, ...)
  x$storeinverseMatrix(inverseMatrix)
  inverseMatrix
}