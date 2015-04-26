## Put comments here that give an overall description of what your
## functions do

## The makeCacheMatrix function is used to create, update and return the matrix 
## we are interested in taking the inverse of. It also contains functions to create, 
## update and return its inverse.



## Write a short comment describing this function

## makeCacheMatrix
## This function returns a list of 4 functions:
## get: to return the matrix we are interested in
## set: to update the matrix we are interested in
## getinv: to return the inverse of the matrix we are interested in
## setinv: to update the inverse of the matrix we are interested in
## One caveat:
## *) It is possible to set the inverse as any matrix i.e. this routine does
## not check that the matrix passed to setinv() is indeed the inverse of the
## matrix we are interested in


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function

## cacheSolve
## This function takes as an argument a list created using the makeCachematrix 
## function. If this object has an inverse matrix stored in it, it will return
## the stored inverse matrix and display the message "getting cached data".
## Otherwise, it will calculate the inverse of the matrix stored in its argument
## using the solve function, store it into the argument list and return this
## inverse.
## Several caveats:
## *) if the matrix is non-invertible, this will return an error
## *) if there is an inverse matrix stored we wil not check whether this is indeed
## the inverse of the matrix we are interested in

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
