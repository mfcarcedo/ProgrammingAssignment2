## The following sequence of functions calculates the inverse of a matrix and catches this value so that 
## if the same matrix is invoked for inversion it will return the cached value rather than to compute it again.

## makeCacheMatrix function creates a special "matrix" object that can cache its inverse. It will return a list of the string of functions
## used to run the makeCacheMatrix- 
makeCacheMatrix <- function(x = matrix()) {
        z<-NULL
        y<-NULL
        setm<- diag(y) ## I use the diag function to make sure that the imput matrix is square
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
## I tried the sequence in R with m1 <- matrix(c(1,2,3,4), nrow = 2, ncol = 2); m2<-makeCacheMatrix(m1); cacheSolve(m2) 
## and I got the inverse of m2- When I again passed cacheSolve(m2), I got "getting cached data" and then the same inverse matrix
## so it worked.
