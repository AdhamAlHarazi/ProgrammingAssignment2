#makeCacheMatrix: This function creates a special "matrix" object that
#can cache its inverse.
#cacheSolve: This function computes the inverse of the special "matrix" 
#returned by makeCacheMatrix above. If the inverse has already been calculated 
#(and the matrix has not changed), 
#then cacheSolve should retrieve the inverse from the cache.


# This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
 
  set <- function(y ){
    x <<- y
    # reset the value of the inverse to NUll if matrix is different
    m <<- Null
  }
  # get the value of the inverse
  get <- function()x
  
  setinverse <- function(solve) m <<- solve
  # get the inverse
  getinverse <- function() m
  
  # passes the value of the function 
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


#This function return the inverse of the matrix whether it cached or not
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  
  # if the inverse is already availabe then return it
  if(!is.null(m)){
    message("getting cached data(Inverse of the matrix)")
    return(m)
  }
  
  # if the inverse is not available, then calculate it and return it
  data <- x$get()
  m <- solve(data)
  x$setinverse(m)
  m
}
