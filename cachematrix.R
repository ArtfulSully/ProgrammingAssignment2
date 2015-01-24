##Overall description: These functions compute the inverse of a matrix and cache it 
##to the object M. The second function checks if the value of M has already been cached,
##and returns the cached value if it has, otherwise it computes the inverse of the Matrix
##and caches it to the object m.

##This function creates a 'special' matrix that really sets the value
##of the matrix, gets the value of the matrix, sets the value of the inverse matrix,
##then gets the value of the inverse of the matrix. 
##The value of the inverse of the matrix is cached in the object m.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y)
    x <<- y
  m <<- NULL
  get <- function() x
  setMatrix <- function(solve) m <<- solve
  getMatrix <- function() m 
  list(set = set, get = get, setMatrix = setMatrix,
       getMatrix  = getMatrix)       
}

##This function checks if the inverse of the matrix X has already been cacluculated.
##If so, it prints 'getting cached data' and returns the cached value of m.
##Otherwise, it computes the inverse of X returned from makeCacheMatrix and
##caches the value to m.

cacheSolve <- function(x, ...) {
  m <- x$getMatrix()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data,...)
  x$setMatrix(m)
}