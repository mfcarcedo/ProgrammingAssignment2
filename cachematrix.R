## The following sequence of functions calculates the inverse of a matrix and catches this value so that 
## if the same matrix is invoked for inversion it will return the cached value rather than to compute it again.

## This function creates a special "matrix" object that can cache its inverse. 
makeCacheMatrix <- function(x = matrix()) {
        z<-NULL
        y<-NULL
        setm<- diag(y) 
                x<<-y
                z<<-NULL
        
        getm<-function() x
        setInv<-function(solve) z<<-solve
        getInv<-function() z
        list(setm=setm, getm=getm, setInv=setInv, getInv=getInv)
}
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above- 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache
cacheSolve<-function(x=matrix(),...){
        z<-x$getInv()
        if(!is.null(z)){
                message("getting cached data")
                return(z)
        }
        y<- x$getm()
        z<-solve(y,...)
        x$setInv(z)
        z      ## Return a matrix that is the inverse of 'x'
}
