## makeCacheMatrix allows the user to store a matrix and retrieve its inversion which is calculated only once. 
## The argument for the function is a matrix
## The returned object list contains a series of functions and holds the original matrix and the 
## cached inverted matrix of it.
##    set (newMatrix) - use this to set a new matrix into this cached matrix object. 
##                     if the matrix is the same in terms of values 
##                     and dimensions then the inversion is not recalculated. 
##    get - gets the stored matrix. 
##    setinv - accepts the solve(matrix) matrix and stores it as a cached inverted matrix. 
##    getinv - retrieves the cached inversion matrix

makeCacheMatrix <- function(x = matrix()) {
  cachedInv<-NULL
  
  set <- function(y) {
    # check if the sames matrix is being passed
    if(!identical(x,y)) {cachedInv<<-NULL}
    x<<-y
  }
  
  get<- function() x
  
  setinv <- function(inv) {
    cachedInv <<- inv
    
  }
  getinv <- function() cachedInv
  
  list( set=set, get=get, setinv=setinv, getinv=getinv)
  
}

## cacheSolve returns the cached inverse of the matrix 

cacheSolve <- function(x, ...) {
        
  cachedInv <- x$getinv()
  if(!is.null(cachedInv)) {
    message("Caching inverse...")
    return(cachedInv)
  }
  data <- x$get()
  cachedInv <- solve(data)
  x$setinv(cachedInv)
  cachedInv
}
