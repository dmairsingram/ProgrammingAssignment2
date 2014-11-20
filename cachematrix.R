## This function takes a matrix as parameter and 1) sets the value of the matrix 
## 2) gets the value of the matrix 3)set the value of the inverted matrix and 4)  
## gets the value of the inverted matrix. 
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y  ##assign the value to an external environment variable for caching purposes
    m <<- NULL
  }
  get <- function() x
  setinv <- function(ginv) m <<- ginv  ##assign the inverted matrix value to an external environment variable m for caching purposes
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function takes makeCacheMatrix function as parameter. A check is made to
## see whether the value of the inverted matrix is already cached. If cached the 
## value is returned. If not then the inverted value will need to be calculated 
## and the value returned
## 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  ## Check to see if the inverted matrix value is already cached
  if(!is.null(m)){
    message("getting cache data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m  
}
