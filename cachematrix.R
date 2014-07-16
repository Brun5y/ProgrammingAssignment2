## The two functions work together to cache a matrix and its invserse

## This function takes matrix, stores it in a cached varible, then uses a list to 
## store the location of the matrix and the inverse.

makeCacheMatrix <- function(x = matrix()) { ## Declares the makeCacheMatrix function,
                            ## which uses a matrix as its only argument
    inverse <- NULL         ## Intializes the value of inverse as 0
    set <- function(y) {    ## Declares the function set which uses an argument y
        x <<- y             ## Stores y in cached value x
        inverse <<- NULL    ## Reintializes the value of inverse as 0
    }
    get <- function() x     ## Declares the get function which, 
                            ## prints out the value of the orginal matrix
    setinverse <- function(inv) inverse <<- inv ## Declares the set function which, 
                            ## sets the value of the inverse
                            ## Note that the argument inv is not cached as the  
                            ## inverse matrix until the function is called
    getinverse <- function() inverse    ## Declares the getinverse function which, 
                                        ## prints out the value of the inverse
                                        ## matrix
    list(set = set, get = get,          ## Makes a list where each element of the 
        setinverse = setinverse,        ## list is one the functions we defined in 
         getinverse = getinverse)       ## makecacheMatrix()
}


## This function takes a list as an argument. If there is no value for the inverse 
## matrix stored it uses the elements of that list to store and print an inverse 
## matrix. If the function already has a value stored for the inverse matrix then 
## that value is recalled and returned.

cacheSolve <- function(x, ...) { ## Declares cacheSolve function which takes as an  
                                 ## an argument a list that contains the location 
                                 ## of cached varibles            
    inverse <- x$getinverse()    ## Sets the value of inverse to the value retrieved
                                 ## by the getinverse function, which is an element
                                 ## of the list x.
    if(!is.null(inverse)) {      ## If statement that checks to see if inverse is
                                 ## still empty 
                                 ## if not, it prints the value of inverse
        message("getting cached data")  ## prints the message to council
        return(inverse)          ## returns the value of the inverse and exits the 
                                 ## function
    }
    data <- x$get()              ## Sets the value of data to the value retrieved
                                 ## by the get function, which is an element
    inverse <- solve(data, ...)  ## Calculates the inverse of the matrice and stores
                                 ## it in the varible inverse
    x$setinverse(inverse)        ## Sets the stored value of setinverse, which 
                                 ## is an element of the list x, to the value of
                                 ## the calculated inverse                           
    print(inverse)               ## prints the value of the inverse matrix
}
