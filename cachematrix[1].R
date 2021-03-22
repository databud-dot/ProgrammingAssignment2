## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  
  #create the variable for the inverse
  invrs<- NULL
  #create another function y 
  set <- function(y){
    #assign the value to the variable created in the other function
    x <<-y
    invrs<<-NULL
  }
  get <- function(){x}
  setInverse <- function(inverse){inverse <- NULL}
  getInverse <- function(){invrs}
  list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
}


## Write a short comment describing this function
##This function is used to calculate the inverse of a matrix if its inverse doesnt 
##exist in the cache .This calculates the matrix inverse by calling the solve function
## and the function getInverse is used to call the inverse
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invrs<-x$getInverse()
  if(!is.null(invrs)){
    print("Getting inverse from cache")
    return(invrs)
  }
  mtr <- x$get()
  invrs <- solve(mtr,...)
  x$setInverse(invrs)
  invrs
  
}

