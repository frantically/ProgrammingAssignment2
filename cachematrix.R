# Calculating the inverse of a large matrix can be computationally expensive. The functions cacheSolve and makeCacheMatrix 
# allow the inverse of a matrix to be calculated once and then accessed multiple times without re-calculation e.g.
# in the code below, only the first call to cacheSolve(x) will calculate the inverse of the matrix, the second call
# will use the result cached from the first call:
# x <- makeCacheMatrix(matrix(1:4, nrow = 2, ncol = 2))
# cacheSolve(x)
# cacheSolve(x)

#makeCacheMatrix returns a list of functions that allow the getting/setting of:
# - some raw data (get, set)
# - the result of an operation on this data, allowing the result to be cached (getCachedResult, setCachedResult)
makeCacheMatrix <- function(x = matrix()) {
  cachedResult <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setCachedResult <- function(cachedResult) cachedResult <<- cachedResult
  getCachedResult <- function() cachedResult
  list(set = set, get = get, getCachedResult = getCachedResult, setCachedResult = setCachedResult)
}

# cacheSolve will return the inverse of the supplied matrix. If the inverse has already been calculated, the cached result
# will be returned instead of re-calculating the result
cacheSolve <- function(x, ...) {
  inverse = x$getCachedResult()
  if(is.null(inverse)) {
    data = x$get()
    inverse <- solve(data, ...)
    x$setCachedResult(inverse)
  } else {
    message("getting cached data")
  }
  inverse
}