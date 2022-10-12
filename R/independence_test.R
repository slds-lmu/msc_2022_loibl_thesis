# Welche Transofrmation h() wäre für unseren Anwendungsfall denkbar ?

library(coin)

x1 = runif(1000, 3, 5)
x2 = runif(1000, 3, 5)
eps = rnorm(1000)

# Data generation process enthält einen Interaktionsterm

y = 1 + x1 + 2*x2 + 2*ifelse(x1<4, x2,0) + eps

data = as.data.frame(cbind(y,x1,x2))

# Anpassung des Modells ohne Interaktion
lm = lm(y ~ x1 + x2)
summary(lm)
score = sandwich::estfun(lm)

score_0 = score[,1]
score_x1 = score[,2]
score_x2 = score[,3]

plot(x1, score_x2)
plot(x2, score_x2)

plot(x2, score_x2)

# Test basierend auf allen Scores 
independence_test(score_0 + score_x1 + score_x2 ~ x1 + x2)
independence_test(score_x2 ~ x1)

# Test basierend auf quadrierten Residuen (-> objective)
independence_test(score_0^2 ~ x1 + x2)

# Test basierend auf Residuen 
independence_test(score_0 ~ x1 + x2)
plot(x2, score_0)

pred = predict(lm, data = cbind(x1,x2), type = "terms")


independence_test(pred[,2] ~ x1)
independence_test(pred[,1] ~ x2)


# GUIDE

res_dich = as.factor(ifelse(score_0 < 0, -1, 1))
x1_quartiles = quantile(x1, seq(0, 1, 0.25))
x1_factor = findInterval(x1, x1_quartiles, rightmost.closed = TRUE)

independence_test(res_dich ~ x1_factor)
independence_test(score_0 ~ x1)

