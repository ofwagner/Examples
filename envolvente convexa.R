# X <- matrix(rnorm(2000), ncol = 2)
# plot(X, cex = 0.5)
# hpts <- chull(X)
# hpts <- c(hpts, hpts[1])
# lines(X[hpts, ][X[hpts, 2]>=0,], col="Red", lty=4,  pch=11)
# 




x <- runif(200, 0, 100)
x2 <- runif(200, 0, 100)

y <- 5*x + 2*x2^2 + 5*x2^3 +rnorm(200, 0, 30)
df <- data.frame(x,x2, y)
df
plot(df)

linearMod <- lm(y ~ x+x2, data=df)

summary(linearMod)


beneficio <- integer(20)
riesgo <- integer(20)
prop_X=1
prop_X2=1
df2=df[(nrow(df)-11):nrow(df),c("x","x2")]

for (inx in 1:20) {

prop_X = prop_X + 1
prop_X2 = prop_X2 + .5

df2$x =df$x[(nrow(df)-11):nrow(df)]*(prop_X/20)
df2$x2 =df$x2[(nrow(df)-11):nrow(df)]*(1-prop_X/20)

  prediccion <- predict(linearMod, df2,se.fit = TRUE)
  
  beneficio[inx] <- sum(prediccion$fit)
  riesgo[inx] <- sum(prediccion$se.fit^2)^(1/2) }

ben_ries <- data.frame(riesgo, beneficio)

plot(ben_ries)

hpts <- chull(ben_ries)
hpts <- c(hpts, hpts[1])
lines(ben_ries[hpts, ][ben_ries[hpts, 2]>=0,], col="Red", lty=4,  pch=11)

