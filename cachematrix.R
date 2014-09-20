## Put comments here that give an overall description of what your
## functions do

## This function caches a matrix as a special 'object' such that it becomes easier on the machine
## to invert the matrix.

makeCacheMatrix <- function(x = matrix()){
# first coerces the x passed as a matrix
  m<-NULL
# the set function assigns the value to x
# in this special environment
  set<- function(y){
    x<<-y
    m<<-NULL
  }
  get <- function()x
  setmatrix <- function(solve) m<<- solve
  getmatrix <- function ()m
# creates the special object that can
# cache its inverse
  list( set = set, get = get,
        setmatrix=setmatrix,
        getmatrix=getmatrix)
}


cacheSolve <- function(x=matrix(), ...){
# gets the function from previous
# and returns a message if calculated already
  m <- x$getmatrix()
    if(!is.null(m)){
      message("getting cached data")
      return(m)
    }  
# using the get function as defined in makeMatrix()
# gets the cached matrix and solves for inverse
    matrix <- x$get()
    m <- solve(matrix, ...)
    x$setmatrix(m)
# returns our data
    m
}
