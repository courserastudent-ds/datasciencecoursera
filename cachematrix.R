## Both functions work together to obtain the cache memory of an inverse matrix
## if it has been calculated in the recent past.

##The first function works to return the inputted matrix(get()),establish a new
##matrix(set(y)-depending on what is input),getting the inverse if already 
##calculated(getinv()) and lastly, save the value calculated from cacheSolve 
##function(setinv()).

##The function has the following 3 variable-
##1)x which is a formal argument for the function makeCacheMatrix() 
##which has a default value of a matrix.
##2)inversemat which is NULL till calculated.
##3)y which is a formal argument for the function(set()).

##The function also makes a list of the functions and names each.

makeCacheMatrix <- function(x = matrix()) {
  inversemat <- NULL
  set <- function(y) {
    x <<- y
    inversemat <<- NULL
  }
  get <- function() x
  setinv <- function(inversecal) inversemat <<- inversecal
  getinv <- function() inversemat
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
}


## The cacheSolve() function is used for calculating the inverse of the matrix.
##If it has already been calculated, it will be called from the previous function
##using $getinv()
## If it has not been calculated, it will be obtained using solve() function.
##The data will be called from the previous function using $get()

##Important note- The formal argument of cacheSolve 'x' is NOT the same as the 
##formal argument passed to makeCacheMatrix(). Instead the makeCacheMatrix.obj 
##an argument containing all functions of makeCacheMatrix() will be passed to
##cacheSolve.

cacheSolve <- function(x, ...) 
  {
  inv.local <- makeCacheMatrix.object$getinv()
  if(!is.null(inv.local)) 
    {
    message("getting cached data")
    return(inv.local)
    }
  data <- makeCacheMatrix.object$get()
  inv.local.calculated <- solve(data, ...) 
  makeCacheMatrix.object$setinv(inv.local.calculated)
  inv.local.calculated #return the inverse value
}
