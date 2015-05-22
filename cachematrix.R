## These functions allow you to cache a function's output so you dont
## need to recalculate each time. In this example, the function 
## creates the inverse of a matrix, and determines if it has already
## been cached, or if it needs to calculate. It returns the inverted matrix

## This function creates a list of functions to store and subsequently 
## interact with stored data
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL #initializes m to null
    set <- function(y) {
      x <<- y #stores a new matrix
      m <<- NULL #resets the inverse, because it has not been calculated
    }
    get <- function() {
      x #returns the matrix stored in X
    }
    setInverse <- function(solve) {
      m <<- solve #caches the calculated inverse matrix
    }
    getInverse <- function() {
      m #returns the matrix - can be null or a cached inverse matrix
    }
    ## Returns a list of functions
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
  
}

## This function checks if the inverse has already been calculated, and if so, retrieves it
## Otherwise, the function calculates the inverse, stores it for future use, and returns it

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse() #Can either be Null or a previously calculated inverse
  if(!is.null(m)) { #if the inverse was cached (i.e. Not Null)
    message("getting cached data")
    return(m) #exit the function and return the cached inverse matrix  
  }
  data <- x$get() #if is null, get data and store in data
  m <- solve(data, ...) #create inverse of data, store in m
  x$setInverse(m) #store value of m for future use
  m #return the newly calculated inverse matrix
}
