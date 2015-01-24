#The functions in this script can be used to calculate the inverse of a matrix.
#These functions will cache the inverse of a matrix and load the cached version when the inverse
#of the same matrix is requested by exploiting the scoping rules of R.

#First, the function makeCacheMatrix should be run on your matrix to create a special matrix object.
#Secondly, the function cacheSolve should be run on the special matrix object returned by makeCacheMatrix.

#For example, the inverse of the matrix [[4, 3][3, 2]] is [[-2, 3], [3, -4]]
#This can be calculated by using the code:
#A<-matrix(c(4, 3, 3,2), nrow = 2, ncol=2) ##make the matrix
#B<-makeCacheMatrix(A) ##create special matrix object
#cacheSolve(B) ##Get inverse from cache if possible, or calculate the inverse and store the inverse in the cache. Return the inverse

makeCacheMatrix <- function(x = matrix()) {
#This function creates a special matrix object, which can
#be used to cache the inverse of a matrix. This function is required
#when the function "cacheSolve.R" is used. This function requires a
#matrix as input.

  #Create a null vector m, for caching the inverse in later.
  m <- NULL
  
  #Store a function in set which returns the function value, but
  #overwrites the cached inverse to null (e.g, special_matrix_object$set).
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  #Store a function in the special matrix object which returns the 
  #original matrix (e.g, special_matrix_object$get).
  get <- function() x
  
  #Store a function in the special matrix object, which stores the
  #inverse in the cache (e.g, special_matrix_object$setinv).
  setinv <- function(solve) m <<- solve
  
  #Store a function in the special matrix object, which returns
  #the cached inverse of a matrix (e.g, special_matrix_object$getinv).
  getinv <- function() m
  
  #Return the special matrix object, which is essentially a list of the 
  #functions set, get, setinv and getinv.
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

#cacheSolve can be used to calculate the 
#inverse of a matrix. If the inverse of the matrix
#has been calculated previously using this function, it will
#load a cached version of the matrix. 

#The function can only be ran with a special matrix object a special created by "CacheMatrix.R".

cacheSolve <- function(x, ...) {
  
  #Check type of x, if x is double assume that a numerical matrix
  #has been inserted and create a special matrix object temporarily.
  #(The function CacheMatrix creates a list, hence this if statement
  #can determine whether a special matrix object has been inserted or not)
  if (typeof(x)!="list")
    
    {#No special special matrix object detected.
    #Return with an error.
    print("No special matrix object detected. Please run CacheMatrix on your matrix matrix and enter the object returned by CacheMatrix.R")
    return(0)
  }

  #Try to recover the previously calculated inverse of the matrix from cache.
  m <- x$getinv()
  
  #See if a value has been loaded in m
  if(!is.null(m)) {
    
    #Cached inverse matrix found, return cached data and return to R
    message("getting cached data")
    return(m)
    
  }
  
  #Cached inverse matrix has not been found, retrieve matrix data from
  #special matrix object
  data <- x$get()
  
  #Using the retrieved numerical matrix, calculate the inverse
  m <- solve(data, ...)
  
  #Store the inverse of the matrix in the cache memory.
  x$setinv(m)
  
  #Return inverse
  m
}

