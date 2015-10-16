library(ggplot2)
library(qualityTools)


# create a vector of w exponential waiting times with lambda = lam

wait <- function(w,lam){
  
  ## Su- set up a range for input values
  # set up range for input value
  if(lam<0)stop("lambda must be larger than 0")
  if(w<0)stop("waiting times must be larger than 0") 
  
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

## Su- don't forget to set seed
set.seed(50)

wait(20,2)

## Su- Also we plot to check whether simulation fits density results
max(wait(20, 2))
x <- seq(0,10,.1)
scale <- 60
y <- scale*dexp(x,.5)
qplot(wait(20, 2), binwidth = .2) + geom_line(aes(x,y,color='red'))


# create a vector of exponential waiting times which total t <= Max with lambda = lam

wait.until <- function(Max,lam){
  # set.seed(50)
  
  ## Su-  set up range for input value
  if(lam<0)stop("lambda must be larger than 0")
  if(Max<0)stop("Maximum waiting times must be larger than 0")
  
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

## Su- check by inputting some values
## test by max = 10, lambda =2
## Su- don't forget to set seed
set.seed(50)
wait.until(10,2)
# interpret: in t=10, for every waiting time we have exponentially rate of 2, number of event might be around 20.




# now simulate the number of events to show that the number of events divided by
# exponential waiting times are Poisson distributed
# (don't forget to comment out the "set.seed")

poi.test <- function(rep, Max, lam){
  
  ## Su- set up range for input value
  if(lam<0)stop("lambda must be larger than 0")
  if(Max<0)stop("Maximum waiting times must be larger than 0")  
  if(rep<0)stop("repeat times must be larger than 0")
  
  a = NULL
  for(i in 1:rep){
    #set.seed(50)
    q = wait.until(Max,lam)
    a = c(a,length(q))
  }
  return(a)
}

## Su- don't forget to set seed
set.seed(50)

poi.test(100,50,2)
# Comments 
# for example,I took 100 replication and max equals 50, with lambda is 2. 

## Su- the result of data input above prove our understanding on gamma distribution
## which is with 100 replications we have t=50, each waiting time exponentially distributed
## at rate of 2. As a result, we should get results around 50/(1/2)=100


# now simlate the waiting time for k events to occur with lambda = lam

wait.for <- function(k, lam){
  
  ## Su- set up range for input value
  if(lam<0)stop("lambda must be larger than 0")
  if(k<0)stop("times of events must be larger than 0") 
  
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

## Su- don't forget to set seed
set.seed(50)

wait.for(20,2)

## Su- Interpretation: with exponential rate of 2, time when the 20th event happen


gam.test <-function(rep, max.e, lam ){
  
  ## Su- set up range for input value
  if(lam<0)stop("lambda must be larger than 0")
  if(max.e<0)stop("Maximum waiting times must be larger than 0")  
  if(rep<0)stop("repeat times must be larger than 0") 
  
  a=NULL
  for (i in 1:rep){
    t = wait.for(max.e,lam)
    a = c(a,t)
  }
  return(a)
}
  
  
## Su- test it with an example
## Su- 
set.seed(50)
# repeat for two times to calculate total waiting time for 3 events happen
gam.test(2,3,2)
# simulate by larger number
gam.test(100,3,2)
# mean should be 3/2, to check it by following:
mean(gam.test(100,3,2))


  

