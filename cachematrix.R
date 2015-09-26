#In a manner similar to the cachemean example, we write the code to cache the matrix and its' inverse.
#Then we use the CacheSolve Function to calculate the inverse.

#First we make sure that the function takes in a matrix variable x. 
#We sent inverse variable i to NULL. 
#We set the value "set" to make x take the value of y and i to be null. 

makeCacheMatrix <- function(x = matrix()) {

i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  #Next, we write the get function to return the matrix x.
  get <- function() x
  
  #Next, we write a function to set the inverse of x.
  setinverse <- function(inverse) i <<- inverse
  
  #Finally, we write a function to get the value of i.
  getinverse <- function() i
  
  #We display all of these values in a list.
  list(
    set = set,
    get = get,
    setinverse = setinverse,
    getinverse = getinverse)
}

#Similarly, we write a function to return the cached value (if any) of the inverse. 
#Else, we calculate it manually.
cacheSolve <- function(x, ...) {
  
  #We define the inverse variable i as the inverse that is generated in the MakeCacheMatrix function. 
  #If the cache not empty, the function will return the inverse.
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  #If the cache is empty, the function will get the value of i using the get function from the MakeCacheMatrix function.
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  return(i)
}
