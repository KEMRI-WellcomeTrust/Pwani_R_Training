###inputting sample data
temp <- c(14.2,16.4,11.9,15.2,18.5,22.1,19.4,25.1,23.4,18.1,22.6,17.2) 
icecream <- c(215,325, 185, 332, 406, 522, 412,614, 544, 421, 445, 408) 
df <- data.frame(temp=temp,icecream=icecream) 
table <- list(df)
print(df)

###Graphical representation - scatter plot

plot(df$temp,df$icecream)

### Correlation by computation
df$deviationTemp <- df$temp- mean(df$temp)
df$deviationIce <- df$icecream- mean(df$icecream)
df$SSxy <- (df$deviationTemp*df$deviationIce) 
df$SSxx <- (df$deviationTemp*df$deviationTemp) 
df$SSyy <- (df$deviationIce*df$deviationIce) 
sum.SSxy <- sum(df$SSxy)
sum.SSxx <- sum(df$SSxx)
sum.SSyy <- sum(df$SSyy)

###Printing the output
print(df)
print(sum.SSxy)
print(sum.SSxx)
print(sum.SSyy)

###correlation coefficient
cor(df$temp,df$icecream)
cor.test(df$temp,df$icecream)

## More Example - Not necessary **

library(mfp)
#install.packages("mfp", dependencies=TRUE)
data(bodyfat)
View(bodyfat)

plot(bodyfat$abdomen, bodyfat$siri, xlab = "Abdomen", ylab = "Percent Body Fat")
cor.matrix <- cor(bodyfat[, c("siri", "weight", "height", "abdomen")])
round(cor.matrix, 2)

