## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##
## This function creates a matrix object with setter and getter member functions
##
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## Write a short comment describing this function
##
## This function can compute the inverse of the matrix object passed as argument
## If the inverse was previously calculated, it retrieves the cached result and returns it
## If not, it computes the inverse using solve() and caches the result using $setInverse()
##
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}

## Sample output
##
## > my_matrix$set(matrix(c(1,2,3,4),2,2))
## > my_matrix$get()
## [,1] [,2]
## [1,]    1    3
## [2,]    2    4
## > my_matrix$get()
## [,1] [,2]
## [1,]    1    3
## [2,]    2    4
## > my_matrix$getInverse()
## NULL
## > cacheSolve(my_matrix)
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## > my_matrix$getInverse()
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## > my_matrix$get()
## [,1] [,2]
## [1,]    1    3
## [2,]    2    4

