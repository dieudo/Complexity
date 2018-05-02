
############################################

#Dieudonne Ouedraogo douedra@binghamton.edu

##############################################


#I am currently exploring another function which will lead to lower MSE
#Such function could be like the one below
a=2
b=1
c=2
#################################################################
#sigmoid=function(x){a/(b+exp(-c*x))} # A proposed function

#sigmoid=function(x) {1/(1+exp(-x))} # The traditional sigmoid function

#################################################################

sigmoid=function(x){a/(b+exp(-c*x))} # A proposed one!

###Fit logistic regression
fitLogist=function(x,y,intercept=T,tol=10e-5,max_it=100)
{
  ##Type conversion
  if (!is.matrix(x))
  {
    x=as.matrix(x)
  }
  if (!is.matrix(y))
  {
    y=as.matrix(y)
  }
  ##Add intercept is required
  if (intercept)
  {
    x=cbind(x,1)
  }
  ##Algorithm initialization
  iterations=0
  converged=F
  ##Weights are initialized to 1 
  coeffs=matrix(1,dim(x)[2])
  
  ##Updates the weight until the max number of iter
  ##Or the termination criterion is met
  while (iterations<max_it& !converged)
  {
    iterations=iterations+1
    nu<-sigmoid(x %*% coeffs)
    old_pred=sigmoid(x %*% coeffs)
    nu_diag=diag(nu[,1])
    ##Weights update
    coeffs=coeffs + solve(t(x) %*% nu_diag %*% x)%*% t(x) %*% (y-nu)
    ##compute mse to check termination
    mse=mean((y-sigmoid(x%*%coeffs))^2)
    ##Stop computation if tolerance is reached
    if (mse<tol)
    {
      converged=T
    }
    
  }
  ##Creates the logit objects 
  Logit=list(intercept=intercept)
  Logit[['coeffs']]=coeffs
  Logit[['preds']]=sigmoid(x%*%coeffs)
  Logit[['residuals']]=Logit[['preds']]-y
  Logit[['mse']]=mean(Logit[['residuals']]^2)
  Logit[['iteration']]=iterations
  attr(Logit, "class") <- "Logit"
  return(Logit)
  
}

##Predict the outcome on new data
predict.Logit<-function(Logit,x,probs=T,..)
{
  if (!is.matrix(x))
  {
    x=as.matrix(x)
  }
  if (Logit[['intercept']])
  {
    x=cbind(x,1)
  }
  if (probs)
  {
    sigmoid(x %*% Logit[['coeffs']])
  }
  else
  {
    sigmoid(x %*% Logit[['coeffs']])>0.5
  }
}


fitLogist(iris[,1:4],iris[,5]=='virginica')

