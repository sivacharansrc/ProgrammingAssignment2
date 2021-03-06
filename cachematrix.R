##URL: https://github.com/sivacharansrc/ProgrammingAssignment2

## The functions makeCacheMatrix and cacheSolve are created so as to compute the 
## inverse of a matrix

## The first function "makeCacheMatrix" creates a special "matrix" object which is 
## capable of caching its inverse. The function helps in the following:
## 1. Set the value of the matrix
## 2. Get the value of the matrix
## 3. Set the value of the matrix inverse
## 4. Get the value of the matrix inverse

makeCacheMatrix <- function(x = matrix()) 
{
  i <- NULL
  set <- function(y)
  {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## The second function cacheSolve computes the inverse of the special matrix created
## by the function makeCacheMatrix. This function is written in such a way that, 
## it first checks if the inverse is already calculated and whether the same is 
## available in the cache. If yes, it retrives the inverse value from the cache.
## If not, this function calculates the inverse value of the matrix and returns
## the same. Also, it sets this computed inverse value to the cache so that it can
## be used later, if applicable.

cacheSolve <- function(x, ...) 
{
   i <- x$getinverse()
   if(!is.null(i)) 
   {
     message("getting cached data")
     return(i)
   }
   data <- x$get()
   i <- solve(data, ...)
   x$setinverse(i)
   i
}


## Calculating the inverse of the matrix
## a <- makeCacheMatrix( matrix(c(1,2,2,1), 2, 2) );
## cacheSolve(a)

## Inverse calculated and result generated in Console Window
## cacheSolve(a)
##             [,1]       [,2]
## [1,] -0.3333333  0.6666667
## [2,]  0.6666667 -0.3333333

## Computing the inverse of the same matrix again retrives the inverse from cache as shown below
## cacheSolve(a)
## getting cached data
##             [,1]       [,2]
## [1,] -0.3333333  0.6666667
## [2,]  0.6666667 -0.3333333