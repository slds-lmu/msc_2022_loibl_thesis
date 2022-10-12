# slim 
data_slim1 = create_sim_data(nob = NULL, n = n, type = "slim2")
X_slim1 = data_slim1[which(names(data_slim1)!="y")]
y_slim1 = data_slim1$y 

x = data_slim1[which(names(data_slim1)!="y")]
y =data_slim1$y 
y = unlist(y)
# browser()
x = x %>% select(where(~ n_distinct(.) > 1))
factor.names = names(x)[sapply(x,class) == "factor"]
if (length(factor.names) > 0){
  xfactors = model.matrix(as.formula(paste("y ~", paste(factor.names, collapse = "+"))), data = cbind(y,x))[, -1]
  x = as.matrix(data.frame(x[which(!(names(x) %in% factor.names))], xfactors))
} else {
  x = as.matrix(x)
}  
# browser()
cv.model = cv.glmnet(x, y, alpha = 1)
model = glmnet(x, y, alpha = alpha, lambda = cv.model$lambda.min)
res = list(model = model, predictions = predict(model, s = cv.model$lambda.min, newx = x))
return(res)


#BIC
t1.bic<-proc.time()[3]
fit<-glmnet(x,y,nlambda=100, standardize=F)
RSS<-deviance(fit)
BIC <- n*log(RSS/n) + log(n)*fit$df
lambda.BIC<-fit$lambda[which.min(BIC)]
t2.bic<-proc.time()[3]
t2.bic - t1.bic

#CV
t1.cv<-proc.time()[3]
fit.cv<-cv.glmnet(x,y, type.measure="mse", nfolds=10, standardize=F)
t2.cv<-proc.time()[3]
t2.cv - t1.cv

#perm
t1.perm<-proc.time()[3]
lambda<-rep(NA,100)
for(i in 1:100){lambda[i]<- (1/n)* max( abs( t(x)%*%sample(y) ) ) }
fit.perm=glmnet(x,y,lambda=median(lambda), standardize=F)
t2.perm<-proc.time()[3]
