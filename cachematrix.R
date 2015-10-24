## This programme will create a inverse matrix for square matrix and cache/store in different environment to 
## recover it later, rather than recalculating it (if the matrix does not change)


      ## makeCacheMatrix takes the matrix input and solves the inverse of the marix.
      ## The solution and input matrix is cached in a different environment, which can be recalled.

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
     ## cachesolve calculates matrix inverse for the matix created in makecachematix
     ## Initially, checks the matrix and its inverse solution in other environment and returns the solution.
     ## If the inverse solution does not exist then it calculates matrix inverse and sets in the cache.

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
