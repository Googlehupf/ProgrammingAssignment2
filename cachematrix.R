## OVERALL DESCRIPTION
## .............................................................................
## The two functions 'makeCacheMatrix' and 'cacheSolve' work together to cache the inverse of a matrix.
## Usage: The matrix is first passed to 'makeCacheMatrix'. The inverse is calculated by 'cacheSolve'. If the inverse has been calculated before, 'cacheSolve' will return the cached inverted matrix (instead of performing a new computation).
##
## Example:
##
##      my_matrix <- matrix(c(4, 3, 3, 2), nrow = 2, ncol = 2)
##      mt <- makeCacheMatrix(my_matrix)
##      cacheSolve(mt)
##      cacheSolve(mt)  # 2nd call will retrieve cached inverse
##
## .............................................................................
## NOTE: It is assumed that the matrix supplied to 'makeCacheMatrix' is always convertible. Therefore,
##  - no checks are performed to see if the matrix is square (a matrix has to be square to be invertible).
##  - no checks are performed to see if the matrix is invertible (square matrices that have an inverse are called invertible or nonsingular, square matrices that do not have an inverse are called noninvertible or singular).


## EXPLANATION of 'makeCacheMatrix'
## .............................................................................
## makeCacheMatrix is a function to be supplied with a matrix object 'x'.
## The inverse of 'x' will later be assigned to an object 'inverted_matrix'.
## makeCacheMatrix does six things:
## (1) Create 'inverted_matrix' as empty (NULL). Purpose: To bring 'inverted_matrix' into existence in order to later prevent the '<<-' operator from leaving the parent environment when it searches for it.
## (2) Define a function 'set()'. Purpose: Reset the cache in case 'x' has changed.
## (3) Define a function 'get()'. Purpose: Get the body of 'x', i.e. the matrix (without args).
## (4) Define a function 'setinverse()'. Purpose: Look up 'inverted_matrix' in the parent environment(s) and assign it, using 'inv' as an argument in an anonymous function.
## (5) Define a function 'getinverse()'. Purpose: Get the body of 'inverted_matrix', i.e. the inverse (without args).
## (6) Provide a list with names for the functions (format: name = function, in this case identical for convenience).

makeCacheMatrix <- function(x = matrix()) {
        inverted_matrix <- NULL
        set <- function(y) {
                x <<- y
                inverted_matrix <<- NULL
        }
        get <- function() x
        setinverse <- function(inv) inverted_matrix <<- inv
        getinverse <- function() inverted_matrix
        list(set = set,
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## EXPLANATION of 'cacheSolve'
## .............................................................................
## cacheSolve is a function to be supplied with objects from makeCacheMatrix (designated 'z' to avoid confusion with 'x' and 'y' from the latter).
## It does three things:
## (1) Assign 'inverted_matrix' as an element of 'z' using makeCacheMatrix's getinverse() function.
## (2) Check if 'inverted_matrix' is empty (NULL). If it is not empty but has already been cached, a message is displayed and 'inverted_matrix' is returned.
## (3) In case 'inverted_matrix' was empty, the function continues to calculating it.
##   - First, the matrix object to be inverted is assigned to 'data' using makeCacheMatrix's get() function.
##   - Next, solve() calculates the inverse, which is assigned to 'inverted_matrix'.
##   - setinverse() is called to make 'inverted_matrix' persist as an element of 'z', the makeCacheMatrix object. The next time cacheSolve runs its first line (1), it will assign the already inverted matrix from there and the check in (2) will find that it is not empty.
##   - Lastly, 'inverted_matrix' is returned.

cacheSolve <- function(z, ...) {
        inverted_matrix <- z$getinverse()
        if(!is.null(inverted_matrix)) {
                message("retrieved from the cache")
                return(inverted_matrix)
        }
        data <- z$get()
        inverted_matrix <- solve(data)
        z$setinverse(inverted_matrix)
        inverted_matrix
}



## ASSIGNMENT NOTES (optional)
## .............................................................................
## The reason that two functions are needed is because a cache has to be in some other environment. One function alone can not cache a value (it would overwrite it).
##
## A detailed explanation of environments is available from Hadley Wickham at http://adv-r.had.co.nz/Environments.html [retrieved 2015-01-25].
##
## A function() in R is a closure that has three components: its formals (args), its body (expr) and its environment (evaluation frame). Every time a function is called, a new evaluation frame = environment is created in which the function is executed. The parent frame 'hosts' these environments.
## One can see this by putting
##      print(environment())            # print the function environment
##      this_env <- environment()       # assign the function environment to 'this_env'
##      print(parent.env(this_env))     # print the parent environment of the function environment
## inside a function.
## The '<<-' operator causes a search through a function's parent environment(s) for an existing definition of the variable being assigned (see R documentation: Assignment Operators).
