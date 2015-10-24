## This programme will create a inverse matrix for square matrix and cache/store in different environment to 
## recover it later, rather recalculating it (if the matrix does not change)
## functions do


makeCacheMatrix <- function(x = matrix()) {

                  m<-NULL
                  setmatrix<-function(y){
                  x<<-y
                  m<<-NULL
              }
                  getmatrix<-function() x
                  setinverse<-function(solve) m<<- solve
                  getinverse<-function() m
                  list(setmatrix = setmatrix, getmatrix = getmatrix,setinverse = setinverse,getinverse=getinverse)
                  
}

cacheSolve <- function(x, ...) { ## Return a matrix that is the inverse of 'x'
                m<-x$getinverse()
                if(!is.null(m)){
                  message("getting cached data")
                  return()
                }
                data<-x$getmatrix()                
                m<- solve(data)
                x$setinverse(m)
                m
}
