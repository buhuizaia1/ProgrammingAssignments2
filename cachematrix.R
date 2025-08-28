## Put comments here that give an overall description of what your

## functions do
 
## Write a short comment describing this function

library(MASS)

makeCachematrix <- function(x=matrix()) 

{

        inv<-NULL       #initializing inverse as null

        set<-function(y) 

        {

                x<<-y

                inv<<-NULL

        }

        get<-function()x            #function to get matrix

        setinv<-function(inverse)inv<<-inverse

        getinv<-function()

        {

                inver<-ginv(x)

                inver%*%x     #function to obtain inverse of matrix

        }

        list(set =set, get=get,

        setinv=setinv,

        getinv=getinv)

}

cachesolve <- function(x,...)  ##gets cache data

{

        inv<-x$getinv() 

        if(!is.null(inv))      ##checking in inverse is null

        {

                message("getting cached data!")

                return(inv)        

        } 

        data<-x$get()

        inv<-solve(data....)      ##calculates inverse value

        x$setinv(inv)

        inv

}
 
