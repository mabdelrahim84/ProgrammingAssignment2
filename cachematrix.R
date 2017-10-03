## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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


## Write a short comment describing this function

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
