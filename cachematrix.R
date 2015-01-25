## makeCacheMatrix() Creates inverse matrix and cache it

makeCacheMatrix <- function(original.matrix = matrix()) {
  
  inverted.matrix <- NULL
  
  set <- function(y) {
    original.matrix <<- y
    inverted.matrix <<- NULL
  }
  
  # Functions for getting and setting cached inv. matrix value
  get <- function() original.matrix

  # Inversing the matrix using solve()
  set.inverse <- function(solve) inverted.matrix <<- solve
  get.inverse <- function() inverted.matrix
  
  list(
    set = set, 
    get = get,
    set.inverse = set.inverse,
    get.inverse = get.inverse)
  
}


## fetch inverse matrix from cache if available , othewise inverse and cache the matrix

cacheSolve <- function(cacheable.matrix, ...) {
  inverted.matrix <- cacheable.matrix$get.inverse()

  # Check cached matrix available in cache
  if(!is.null(inverted.matrix)) {
    message("Getting from cache...")
    return(inverted.matrix)
  }

  # No cached Matrix is available just cache it... 
 
  matrix.to.inverse <- cacheable.matrix$get()
  inverted.matrix <- solve(matrix.to.inverse)
  cacheable.matrix$set.inverse(inverted.matrix)
  inverted.matrix
  
}