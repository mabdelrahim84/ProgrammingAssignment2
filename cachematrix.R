## Put comments here that give an overall description of what your
## functions do

## This function creates a special matrix object that is used to cache the inverse of a matrix passed to it

makeCacheMatrix <- function(x = matrix()) {
    invm <- NULL
    set <- function(y){
        x <<- y
        invm <<- NULL
    }
    
    get <- function(){x}
    
    setInv <- function(inv_mat){
        invm <<- inv_mat
    }
    
    getInv <- function(){
        invm
    }
    
    
    list(set = set, get = get , setInv = setInv, getInv = getInv)
    
}


## This function check if the inverse of a matrix is already cached , if so it will return the cached value
## otherwise , the function will calculate the inverse and return it

cacheSolve <- function(mat_in , ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- mat_in$getInv()
    if (!is.null(i)){
        message("get cached data!")
        return(i)
    }
    my_mat <- mat_in$get()
    inv_mat <- solve(my_mat)
    mat_in$setInv(inv_mat)
    inv_mat
}
