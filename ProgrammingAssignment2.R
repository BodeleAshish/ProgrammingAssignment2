## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
makeCacheMatrix <- function(x = matrix()) {
  
  inmv <- NULL   #initialize inverse matrix value
  
  #set the value of matrix
  set<-function(y){
    x <<- y
    inmv <<- NULL
  }
  
  get <-function() x  #get the value of matrix
  
  #set value of inverse
  set_inverse <- function(inmv_input) inmv <<- inmv_input
  #get value of inverse
  get_inverse <- function() inmv
  
  #return the list
  list(set = set, get = get,
       set_inverse = set_inverse,
       get_inverse = get_inverse )
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  # check if the inverse is already cached,
  # if so, we get the inverse from the cache directly
  
  inmv <- x$get_inverse()
  
  if(is.null(inmv)){
    message("getting cached Inverse")
    return(inmv)
  }
  # else, we first get the matrix
  data <- x$get()
  #calculate the inverse
  inmv <- solve(data, ...)
  
  #cache the inverse of the matrix
  x$setinverse(inmv)
  #rerurn the matrixvalue
  inmv
}
