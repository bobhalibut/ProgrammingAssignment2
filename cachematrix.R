## These functions enable the storing of the results of inverting
## matrices.  This enables these results to be called quickly rather than 
## having to calculate them each time (which could be time-consuming)


## The makeCacheMatrix() function creates a special 'matrix' object that can
## cache its inverse.


makeCacheMatrix <- function(x = matrix()) { ## name the function and declare 
                                            ## the variable x as a matrix
  
   m <- NULL     ## initialize m locally as NULL (m will be matrix inverse)
     
   ## This declares the four functions: set(), get(), setsolve(), getsolve()
   
   ## set() enables the data values in the matrix to be changed.
   set <- function(y) {  ## declares function in case changes are made 
                        ## to any of the data within the matrix
    x <<- y   ## x is assigned within this function to be y
    m <<- NULL ## m is assigned within this function to be NULL
  }
  get <- function() x  ## the function 'get' that returns the matrix
  
  setsolve <- function(solve) m <<- solve ## this enables storing of the calcualted inverse
                                        
  getsolve <- function() m ## this returns the inverse
  
  list(set = set, get = get,  ## returns the list containing the four functions
       setsolve = setsolve,
       getsolve = getsolve)
}


##  This function returns the inverse of a matrix either from a cached source
##  or by calculating it if no cached source exists.  If no cached source exists
##  then it saves the calcualted inverse matrix to the cache.  If the cached
##  source does exist then it lets the user know it is being retrieved from
##  the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  m <- x$getsolve()  ## pulls in the cached inverse to m (if it exists)
  
  if(!is.null(m)) {  ## checks that m is not null (i.e. inverse has been cached)
    message("getting cached data")  ## if m is not null then returns this tex
    return(m)  ## if m is not null then returns m = matrix inverse
  }     ## if m is null then needs to calculate inverse
  
  data <- x$get() ## temporarily stores matrix
  m <- solve(data, ...) ## calculates the inverse of the stored matrix
  x$setsolve(m) ## stores the inverse in the cache
  m  ## displays the inverse

}
