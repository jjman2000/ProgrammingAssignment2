## The two functions below speed up the potentially taxing process of inverting a
## matrix by caching the inverse instead of computing it repeatedly. The inverse
## is cached in a special 'matrix' object that can store itself and its inverse.
## These functions expect the matrix to be invertible - so any matrix argument must be 
## both square and numeric. The functions do not check this condition so passing
## a non-invertible matrix argument will cause errors.


## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse. makeCacheMatrix
## is passed one argument x, an invertible matrix (which requires that the matrix be numeric and square).
makeCacheMatrix <- function(x = matrix()) {
  i <- matrix() #i will store the inverted matrix, inialized to an empty matrix
  set <- function(y) { #the matrix can be changed using the set object and a matrix argument y
    x <<- y       
    i <<- matrix() #if the matrix is changed, the inverted matrix is set to be empty
  }
  get <- function() x # the get object will return the matrix x
  setinverse <- function(inverse) i <<- inverse #setting the inverse when called from cacheSolve function
  getinverse <- function() i
  list(set = set, get = get,setinverse = setinverse,getinverse = getinverse) #makeCacheMatrix is really a list of functions
}


## The function cacheSolve  will return the inverse of a provided object x,
## where x is a special "matrix" object that can cache its inverse. If the inverse of x
## has already been calculated (and the matrix has not changed), then cacheSolve will
## return inverted matrix from cache. Otherwise, it will caclulate the inverse and then save
## it to cache

cacheSolve <- function(x, ...) { #the argument x is a special matrix that can store its inverse
  i <- x$getinverse()
  if(!anyNA(i)) { #if the inverse has been calculated already then notify the user and return the inverse
    message("getting cached data")
    i
  }
  else  {    #calculate the inverse and store it in x
  i <- solve(x$get()) #i gets the inverse of X
  x$setinverse(i) #stores the inverse in cache
  i
  }
}
