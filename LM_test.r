

#Librerias para distintos contrastes del modelo
library(magrittr)
library(lmtest)
library(gvlma)
library(MASS)
library(car)
library(normtest)
library(sfsmisc) # test wald para RLM
library(corrgram)
library(DescTools)





df = data.frame(y = 1:100, x1 = runif(100), x2 = rnorm(100))


#Correlaciones
corrgram(df, order=NULL, lower.panel=NULL, upper.panel=panel.cor, diag.panel=panel.density)
cor(df) ->correlo
corrgram(df, order=NULL, lower.panel=corrgram::panel.ellipse, upper.panel=panel.cor, diag.panel=panel.density)


#GLM 

model <- lm(df, formula= y~.)

summary(model)

###Asunciones LM

par(mfrow=c(2,2))
plot(model)
par(mfrow=c(1,1))


model_tests<-gvlma(model) #kurtosis y Skewness 

summary(model_tests)
plot(model_tests)

mean(model$residuals) # la media de los residos es cero

#MAPE
MAPE(model)
RMSE(model)
#https://www.rdocumentation.org/packages/DescTools/versions/0.99.19/topics/Measures%20of%20Accuracy

#Autocorrelación
dwtest(model) #ausencia de correlación

##Normalidad
sresid <- studres(model) 
hist(sresid, freq=FALSE, 
     main="Distribution of Studentized Residuals",  breaks = 15)
xfit<-seq(min(sresid),max(sresid),length=40) 
yfit<-dnorm(xfit) 
lines(xfit, yfit) #no es normal!!
boxplot(residuals(model), main = "Boxplot of Residuals")
#qqplot
qqPlot(model, main="QQ Plot")
qqnorm(residuals(model),main = "Q-Q Plot")
qqline(residuals(model))
#JB
jb.norm.test(residuals(model)) #no es normal!!
#outliers
leveragePlots(model)
influencePlot(model,	id.method="identify")

##Homoecedasticiadad
# non-constant error variance test Breusch-Pagan-Godfrey
bptest(model)
ncvTest(model) #se rechaza homocedasticidad!!
# White
white.test(model)
# plot studentized residuals vs. fitted values 
spreadLevelPlot(model) 

##Multicolinearedad 
vif(model)# solo una variable indep..
sqrt(vif(model)) #Si el factor de inflación de la varianza de una variable predictora fuera 5.27 (sqrt 5.27 = 2.3) esto significa que el error estándar para el coeficiente de esa variable predictora es 2.3 veces mayor que si esa variable predictora no estuviera correlacionada con las otras variables predictoras


# Evaluate Nonlinearity
# component + residual plot 
crPlots(model)
reset(model, power=2, type=c("fitted"), data=DF) #Ramseys Test
#MAPE
#https://www.rdocumentation.org/packages/DescTools/versions/0.99.19/topics/Measures%20of%20Accuracy