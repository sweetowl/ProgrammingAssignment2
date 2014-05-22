### Function creates a special matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  # m will store in the cache the inversed matrix
  m <- NULL
  
  # details for set
  set <- function (y) {
    x <<- y
    m <<- NULL
  }
  
  # details for get
  get <- function() x
  
  # setting the inverse of the matrix
  setSolve <- function(solve) m <<- solve
  
  # getting the inverse of the matrix
  getSolve <- function() m
  
  # print the matrix with the functions above
  list(set = set, get = get,
       setSolve = setSolve,
       getSolve = getSolve)
}


### Function computes the inverse of the matrix created by makeCacheMatrix function. 
### If the inverse has already been calculated this function will get the inverse from the cache.

cacheSolve <- function(x, ...) {
  
        m <- x$getSolve ()
        
        # If there is the inverse already calculated - return it
        if(!is.null(m)) {
          message("getting cached data")
          return(m)
        }
        
        # If there is NOT the inverse calculated - calculate it!
        data <- x$get()
        m <- solve(data,...)
        
        # Cache this inverse
        x$getSolve(m)
        
        # Return the inverse
        m
}
