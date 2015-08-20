#August 2015
#Assignment 2: Cachematrix.R
#These two function are provide an object to store the inverse of matrix


#First function is makeCachematrix
#It is to creates an object that stores a matrix
#its inverse
#provides a set of function to 'set' and 'get' them.
#which is really a list containing a function to
## set the value of the matrix
## get the value of the matrix
## set the inverse of the matrix
## get the inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve) 
  
  
}

# Second function is cacheSolve
#Purpose is to test whether the inverse of a matrix stored in a cacheMatrix
#the matrix inverse in computed and stored within the 
#cacheMatrix object

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
  
}