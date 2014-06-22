## The function creates an matrix object (list) that can cache its inverse.

## Setting the matrix. Getting the matrix and calculating / setting the value of the inverse of the matrix. 
## getting the value of the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set<- function(y){
                x<<-y
                s<<-NULL
        }
        get<-function() x
        setmatrix<-function(solve) s<<- solve
        getmatrix<-function() s
        list(set=set, get=get, setmatrix=setmatrix, getmatrix=getmatrix)
}


## The function checks if the inverse of the matrix is already cached. 
## If so, it returns the inverse of the matrix from the cache. If not yet computed
## it will calculate that and set it as the inverse matrix value.

cachesolve <- function(x=matrix(), ...) {
        s<-x$getmatrix()
        
        if(!is.null(s)){
                message("getting cached data")
                return(s)
        }
        
        matrix<-x$get()
        s<-solve(matrix, ...)
        x$setmatrix(s)
        
        s
        ## Return a matrix that is the inverse of 'x'
}
