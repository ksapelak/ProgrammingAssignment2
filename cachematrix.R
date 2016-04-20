## This code will cache the inverse of a matrix and return a matrix that is the inverse of X.

## this function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setmatrix <-function(solve) m<<- solve
  getmatrix <-function() m
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)
}


## This function computes the inverse of the special "matrix" returned by makeCasheMatrix above.  
##If the inverse has already been calculated (and the matrix has not changed), 
##then the cashesolve will retrieve the inverse from the cache.

cacheSolve <- function(x=matrix(), ...) {
  m<-x$getmatrix()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  matrix<-x$get()
  m<-solve(matrix, ...)
  x$setmatrix(m)
  m
}
        ## Return a matrix that is the inverse of 'x'
