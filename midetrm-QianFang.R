library(ggplot2)
library(qualityTools)


# create a vector of w exponential waiting times with lambda = lam

wait <- function(w,lam){
  a = NULL
  for(i in 1:w){
    a = c(a,rexp(1,rate = lam))
  }
  return(a)
}

# Comments
# The exponential distribution can be simulated in R with rexp(n, lambda) where lambda is the rate parameter.
# The mean of exponential distribution is 1/lambda and the standard deviation is also also 1/lambda.
# for example, lambda equals 2, we get 20 exponentail waiting times with average waiting time equals to 1/2=0.5
wait(20,2)



# create a vector of exponential waiting times which total t <= Max with lambda = lam

wait.until <- function(Max,lam){
  # set.seed(50)
  time = 0
  a = NULL
  while(time < Max){
    inter = rexp(1,lam)
    a = c(a,inter)
    time = time + inter
  }
  return(a[1:(length(a)-1)])  ##test w seed ## haha use ()
}

# Comments: 
# The random number stream depends on a seed value. 



# now simulate the number of events to show that the number of events divided by
# exponential waiting times are Poisson distributed
# (don't forget to comment out the "set.seed")

poi.test <- function(rep, Max, lam){
  a = NULL
  for(i in 1:rep){
    #set.seed(50)
    q = wait.until(Max,lam)
    a = c(a,length(q))
  }
  return(a)
}
poi.test(100,50,2)
# Comments 
# for example,I took 100 replication and max equals 50, with lambda is 2. 


# now simlate the waiting time for k events to occur with lambda = lam

wait.for <- function(k, lam){
  time = 0
  count = 0
  a = NULL
  while(count < k){
    inter=rexp(1,lam)
    count = count + 1
    time = time+inter
  }
  
  return(time)
} 
# with lambda equals to 2, the k events is 20.
wait.for(20,2)


gam.test <-function(rep, max.e, lam ){
  a=NULL
  for (i in 1:rep){
    t = wait.for(max.e,lam)
    a = c(a,t)
    
  }
  
  return(a)
}
  
  
  
  
  
  
  

