## Cache the inverse of a matrix

## makeCacheMatrix creates the object that can cache it's inverse

makeCacheMatrix <- function(x = matrix()) {
    z<-NULL
    set<-function(y){
        x<<-y
        z<<-NULL
    }
    get<-function() x
    setmatrix<-function(solve) z<<- solve
    getmatrix<-function() z
    list(set=set, get=get, setmatrix=setmatrix, getmatrix=getmatrix)
}


## cacheSolve inverses matrix, returns cache'd matrix if it hasn't changed

cacheSolve <- function(x = matrix(), ...) {
    z<-x$getmatrix()
    if(!is.null(z)){
        message("getting cached data")
        return(z)
    }
    matrix<-x$get()
    z<-solve(matrix, ...)
    x$setmatrix(z)
    z
}
