## these functions written in partial fulfillment of coursera r programming
## week 3 assignment

## make cache matrix function creates a specil matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        inv<-NULL                               ##initialising inverse as null
        set<-function(y){
                x<<-y                           ## value of matrix in parent environment
                inv<-NULL                       ## reset invverse of new matrix to NULL
                }
        get<-function()x                        ## retrning value of matrix
        
        setinverse<-function(inverse) inv<<-inverse
        getinverse<-function()inv
        list(set=set,get=get,set inverse=set inverse,get inverse=get inverse)
}


## this function computes the inverse of special matrix returned by the above function
## if  the inverse has already been calculated then cachesolve will retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv<-x$get inverse()
        if(!is.null(inv)){
                message ("getting cached data")
                return(inv)
                }
        data<-x$get()
        inv<-solve(data,...)
        x$set inverse(inv)
        inv
}
