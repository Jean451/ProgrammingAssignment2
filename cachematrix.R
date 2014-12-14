## creating a kind of matrix able to cache their inverse

## creating a matrix able to cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  z <-NULL
  set<-function(y) {
    x <<-y
    z <<-NULL
  }

  get<-function() x
  setinv<-function(inv) z<<-inv
  getinv<-function() z
  list(set=set, get=get, setinv=setinv,getinv=getinv)
}


## getting the cached inverse of a matrix or calculating and caching it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  z<-x$getinv()
  if(!is.null(z)){
    message("getting cached data")
    return(z)
  }
  data<-x$get()
  z<-solve(data,...)
  x$setinv(z)
  z
}
