x<- seq(0,1,0.01)

y_log_b.25<-log(x, base = 0.25)
`y_log_-3`<-log(x)/(-3)


y_lambda_45 <- 0.45^x
y_lambda_05 <- 0.05^x
y_lambda_025 <- 0.025^x


plot(x,y_log_b.25, type="l", col=1, xlab="t (days)", ylab="y") 
lines(x,`y_log_-3`, type="l", col=2)
lines(x,y_lambda_45, type="l", col=3)
lines(x,y_lambda_05, type="l", col=4)
lines(x,y_lambda_025, type="l", col=5)
legend(x=0.7, y= 2.5, legend=c("Log_B 0.25", "-1/3 *(Log_B e)","Adstock_45", "Adstock_05", "Adstock_025"),
       col=c(1:5), lty=1, cex=0.8)

# max_y <- max(max(y_log_b.25), max(`y_log_-3`), max(y_lambda_45), max(y_lambda_05), max(y_lambda_025)   )
max_y <- 3

plot(x,y_log_b.25/max_y, type="l", col=1, xlab="t (days)", ylab="y") 
lines(x,`y_log_-3`/max_y, type="l", col=2)
lines(x,y_lambda_45/max_y, type="l", col=3)
lines(x,y_lambda_05/max_y, type="l", col=4)
lines(x,y_lambda_025/max_y, type="l", col=5)
legend(x=0.7, y= 2.5, legend=c("Log_B 0.25", "-1/3 *(Log_B e)","Adstock_45", "Adstock_05", "Adstock_025"),
       col=c(1:5), lty=1, cex=0.8)

max_y <- 1

x<- seq(0,100,0.01)
y <- abs(sqrt(x))
y2 <-abs(sqrt(x/10))
yatan <- atan(x/1)



plot(x,x/max_y)
lines(x,y/max_y, type="l", col=2)
lines(x,y2/max_y, type="l", col=3)
lines(x,yatan/max_y, type="l", col=4)


plot(x,y/max_y, type="l", col=2)
lines(x,y2/max_y, type="l", col=3)
lines(x,yatan/max_y, type="l", col=4)




#https://en.wikipedia.org/wiki/Exponential_distribution

x<- seq(0,20,0.01)
alpha <- 0.5
conversor <- 15

negative_exponential <- function(x,alpha,conversor=1) {
  
                      return((1-exp(-alpha*ifelse(x<=0,0,x)))*conversor) }

y <- negative_exponential(x,alpha,conversor)

yatan <- atan(x/2)

plot(x,y)
# lines(x,x, type="l", col=3)
# lines(x,yatan, type="l", col=4)

