# Paper-1
Economic dispatch of electrical power in South Africa: An application to the Northern Cape province

library(MASS)
library(e1071)
library(quantreg)
library(qgam)
library(devtools)
library(gefcom2017)
library(caret)
library(relaimpo)
library(glmnet)

attach(NewData)
head(NewData)
win.graph()

####Summary statistics for hours
summary(N18H00)
summary(N19H00)
summary(N20H00)
summary(N21H00)

####Standard deviation
sd(N18H00)
sd(N19H00)
sd(N20H00)
sd(N21H00)

####skewness
skewness(N18H00)
skewness(N19H00)
skewness(N20H00)
skewness(N21H00)

####kurtosis
kurtosis(N18H00)
kurtosis(N19H00)
kurtosis(N20H00)
kurtosis(N21H00)

#######################################################################################################
#####ts and density plots for H19 n H20
win.graph()
par(mfrow=c(2,2))
y18=ts(N18H00,start = 2000,frequency = 365)
plot(y18,main="Demand at 18:00",xlab = "Year",ylab="Demand (MW)",col="blue")
v18=density(N18H00)
plot(v18,main="Density at 18:00",xlab = "Electricity demand (MW)",col="blue")

y19=ts(N19H00,start = 2000,frequency = 365)
plot(y19,main="Demand at 19:00",xlab = "Year",ylab="Demand (MW)",col="blue")
v19=density(N19H00)
plot(v19,main="Density at 19:00",xlab = "Electricity demand (MW)",col="blue")

y20=ts(N20H00,start = 2000,frequency = 365)
plot(y20,main="Demand at 20:00",xlab = "Year",ylab="Demand (MW)",col="blue")
v20=density(N20H00)
plot(v20,main="Density at 20:00",xlab = "Electricity demand (MW)",col="blue")

y21=ts(N21H00,start = 2000,frequency = 365)
plot(y21,main="Demand at 21:00",xlab = "Year",ylab="Demand (MW)",col="blue")
v21=density(N21H00)
plot(v21,main="Density at 21:00",xlab = "Electricity demand (MW)",col="blue")

#######################################################################################################
#####Q - Q plots for H18,H19, H20 and H21
win.graph()
par(mfrow=c(2,2))
qqnorm(N18H00, pch = 1, frame = FALSE,main="18:00")
qqline(N18H00, col = "blue", lwd = 2)
#H19
qqnorm(N19H00, pch = 1, frame = FALSE,main="19:00")
qqline(N19H00, col = "blue", lwd = 2)
#H20
qqnorm(N20H00, pch = 1, frame = FALSE,main="20:00")
qqline(N20H00, col = "blue", lwd = 2)
#H21
qqnorm(N21H00, pch = 1, frame = FALSE,main="21:00")
qqline(N21H00, col = "blue", lwd = 2)


#######################################################################################################
######Bloxplot
win.graph()
hour <- c("18:00", "19:00", "20:00", "21:00")
boxplot(N18H00, N19H00, N20H00, N21H00, names= hour, horizontal = FALSE, 
        main="",ylab=" Electricity demand (MW)", col = "blue")

######################################################################################################
#####nonlinear trend
###18:00
win.graph()
par(mfrow=c(1,2))
plot(N18H00, type="l", xlab="Observation number", ylab="Electricity demand (MW) at 18:00")

z = (smooth.spline(time(N18H00), N18H00))
z
lines(smooth.spline(time(N18H00), N18H00, spar=0.1604018),col="red",lwd=3)
dfits = fitted((smooth.spline(time(N18H00), N18H00, spar=0.1604018)))
dfits <- round(dfits,3)
write.table(dfits,"~/noltrend18.txt",sep="\t")

###19:00
plot(N19H00, type="l", xlab="Observation number", ylab="Electricity demand (MW) at 19:00")

z = (smooth.spline(time(N19H00), N19H00))
z
lines(smooth.spline(time(N19H00), N19H00, spar=0.129111),col="red",lwd=3)
dfits = fitted((smooth.spline(time(N19H00), N19H00, spar=0.129111)))
dfits <- round(dfits,3)
write.table(dfits,"~/noltrend19.txt",sep="\t")

###20:00
plot(N20H00, type="l", xlab="Observation number", ylab="Electricity demand (MW) at 20:00")

z = (smooth.spline(time(N20H00), N20H00))
z
lines(smooth.spline(time(N20H00), N20H00, spar=0.105942),col="red",lwd=3)
dfits = fitted((smooth.spline(time(N20H00), N20H00, spar=0.105942)))
dfits <- round(dfits,3)
write.table(dfits,"~/noltrend20.txt",sep="\t")

###21:00
plot(N21H00, type="l", xlab="Observation number", ylab="Electricity demand (MW) at 21:00")

z = (smooth.spline(time(N21H00), N21H00))
z
lines(smooth.spline(time(N21H00), N21H00, spar=0.1092967),col="red",lwd=3)
dfits = fitted((smooth.spline(time(N21H00), N21H00, spar=0.1092967)))
dfits <- round(dfits,3)
write.table(dfits,"~/noltrend21.txt",sep="\t")

#######################################################################################################
######### LINEAR QUNATILE REGRESSION (from quantreg )
##########18:00
win.graph()
par(mfrow=c(2,2))
ln18 <- ts(N18H00)
plot(ln18, xlab="Observation number", ylab="Electricity demand (MW) at 18:00",ylim=c(410,1000))

qr.load18lqr = rq(N18H00 ~ AveMinT + AveTem + AveMaxT + DayType + noltrend18, data= NewData, tau=0.9999)
summary.rq(qr.load18lqr,se="boot") # can use se = "nid" or se="ker" 
lines(qr.load18lqr$fit, col="red")
LQR18 <- fitted(qr.load18lqr)

##########19:00
ln19 <- ts(N19H00)
plot(ln19, xlab="Observation number", ylab="Electricity demand (MW) at 19:00", ylim=c(410,1000))

qr.load19lqr = rq(N19H00 ~ AveMinT + AveTem + AveMaxT + DayType + noltrend19, data= NewData, tau=0.9999)
summary.rq(qr.load19lqr,se="boot") # can use se = "nid" or se="ker" 
lines(qr.load19lqr$fit, col="red")
LQR19 <- fitted(qr.load19lqr)

##########20:00
ln20 <- ts(N20H00)
plot(ln20, xlab="Observation number", ylab="Electricity demand (MW) at 20:00", ylim=c(420,1000))

qr.load20lqr = rq(N20H00 ~ AveMinT + AveTem + AveMaxT + DayType + noltrend20, data= NewData, tau=0.9999)
summary.rq(qr.load20lqr,se="boot") # can use se = "nid" or se="ker" 
lines(qr.load20lqr$fit, col="red")
LQR20 <- fitted(qr.load20lqr)

##########21:00
ln21 <- ts(N21H00)
plot(ln21, xlab="Observation number", ylab="Electricity demand (MW) at 21:00", ylim=c(430,950))

qr.load21lqr = rq(N21H00 ~ AveMinT + AveTem + AveMaxT + DayType + noltrend21, data= NewData, tau=0.9999)
summary.rq(qr.load21lqr,se="boot") # can use se = "nid" or se="ker" 
lines(qr.load21lqr$fit, col="red")
LQR21 <- fitted(qr.load21lqr)

#######################################################################################################
#########  NONLINEAR QUNATILE REGRESSION (from quantreg )
######18:00
win.graph()
par(mfrow=c(2,2))
ln18 <- ts(N18H00)
plot(ln18, xlab="Observation number", ylim=c(400,1000), ylab="Electricity demand (MW) at 18:00")

Dat.nlrq18 <- nlrq(N18H00 ~ SSlogis(AveMinT + AveTem + AveMaxT + DayType + noltrend18, Asym, mid, scal), 
                 data= NewData, tau=0.9999, trace=TRUE)
lines(predict(Dat.nlrq18), col="red")
NLQR18 <- fitted(Dat.nlrq18)

######19:00
ln19 <- ts(N19H00)
plot(ln19, xlab="Observation number", ylab="Electricity demand (MW) at 19:00", ylim=c(400,1000))

Dat.nlrq19 <- nlrq(N19H00 ~ SSlogis(AveMinT + AveTem + AveMaxT + DayType + noltrend19, Asym, mid, scal), 
                 data=NewData, tau=0.9999, trace=TRUE)
lines(predict(Dat.nlrq19), col="red")
NLQR19 <- fitted(Dat.nlrq19)

######20:00
ln20 <- ts(N20H00)
plot(ln20, xlab="Observation number", ylab="Electricity demand (MW) at 20:00", ylim=c(420,1000))

Dat.nlrq20 <- nlrq(N20H00 ~ SSlogis(AveMinT + AveTem + AveMaxT + DayType + noltrend20, Asym, mid, scal), 
                 data=NewData, tau=0.9999, trace=TRUE)
lines(predict(Dat.nlrq20), col="red")
NLQR20 <- fitted(Dat.nlrq20)

######21:00
ln21 <- ts(N21H00)
plot(ln21, xlab="Observation number", ylab="Electricity demand (MW) at 21:00", ylim=c(420,980))

Dat.nlrq21 <- nlrq(N21H00 ~ SSlogis(AveMinT + AveTem + AveMaxT + DayType + noltrend21, Asym, mid, scal), 
                 data=NewData, tau=0.9999, trace=TRUE)
lines(predict(Dat.nlrq21), col="red")
NLQR21 <- fitted(Dat.nlrq21)

########################################################################################################
###########ADDITIVE QUANTILE REGRESSION 
# Calibrate learning rate on a grid
######18:00
win.graph()
par(mfrow=c(2,2))
ln18 <- ts(N18H00)
plot(ln18, xlab="Observation number", ylim=c(400,1000), ylab="Electricity demand (MW) at 18:00")

set.seed(5235)
tun <- tuneLearnFast(form=N18H00~s(AveMinT, bs="ad")+s(AveTem, bs="ad")+s(AveMaxT,bs="ad")+
                       s(DayType, bs="ad")+s(noltrend18,bs="ad"), err = 0.05, qu = 0.9999,
                     data = NewData)
tun
fit18 <-qgam(N18H00~s(AveMinT, bs="ad")+s(AveTem, bs="ad") +s(AveMaxT,bs="ad")+s(DayType, bs="ad")
            +s(noltrend18,bs="ad"), err = 0.05, qu = 0.9999, lsig = tun$lsig, data = NewData)
summary(fit18, se="boot") #se =" ker" " nid" "boot"
lines(fit18$fit, col="red")
AQR18 <- fitted(fit18)

######19:00
ln19 <- ts(N19H00)
plot(ln19, xlab="Observation number", ylim=c(400,1000), ylab="Electricity demand (MW) at 19:00")

set.seed(5235)
tun <- tuneLearnFast(form=N19H00~s(AveMinT, bs="ad")+s(AveTem, bs="ad")+s(AveMaxT,bs="ad")+
                       s(DayType, bs="ad")+s(noltrend19,bs="ad"), err = 0.05, qu = 0.9999,
                     data = NewData)
tun
fit19 <-qgam(N19H00~s(AveMinT, bs="ad")+s(AveTem, bs="ad") +s(AveMaxT,bs="ad")+s(DayType, bs="ad")
            +s(noltrend19,bs="ad"), err = 0.05, qu = 0.9999, lsig = tun$lsig, data = NewData)
summary(fit19, se="boot") #se =" ker" " nid" "boot"
lines(fit19$fit, col="red")
AQR19 <- fitted(fit19)

######20:00
ln20 <- ts(N20H00)
plot(ln20, xlab="Observation number", ylim=c(420,1000), ylab="Electricity demand (MW) at 20:00")

set.seed(5235)
tun <- tuneLearnFast(form=N20H00~s(AveMinT, bs="ad")+s(AveTem, bs="ad")+s(AveMaxT,bs="ad")+
                       s(DayType, bs="ad")+s(noltrend20,bs="ad"), err = 0.05, qu = 0.9999,
                     data = NewData)
tun
fit20 <-qgam(N20H00~s(AveMinT, bs="ad")+s(AveTem, bs="ad") +s(AveMaxT,bs="ad")+s(DayType, bs="ad")
            +s(noltrend20,bs="ad"), err = 0.05, qu = 0.9999, lsig = tun$lsig, data = NewData)
summary(fit20, se="boot") #se =" ker" " nid" "boot"
lines(fit20$fit, col="red")
AQR20 <- fitted(fit20)

######21:00
ln21 <- ts(N21H00)
plot(ln21, xlab="Observation number", ylim=c(420,1000), ylab="Electricity demand (MW) at 21:00")

set.seed(5235)
tun <- tuneLearnFast(form=N21H00~s(AveMinT, bs="ad")+s(AveTem, bs="ad")+s(AveMaxT,bs="ad")+
                       s(DayType, bs="ad")+s(noltrend21,bs="ad"), err = 0.05, qu = 0.9999,
                     data = NewData)
tun
fit21 <-qgam(N21H00~s(AveMinT, bs="ad")+s(AveTem, bs="ad") +s(AveMaxT,bs="ad")+s(DayType, bs="ad")
            +s(noltrend21,bs="ad"), err = 0.05, qu = 0.9999, lsig = tun$lsig, data = NewData)
summary(fit21, se="boot") #se =" ker" " nid" "boot"
lines(fit21$fit, col="red")
AQR21 <- fitted(fit21)

########################################################################################################
##### Calculates the pinball loss score for a given quantile.
####Linear quantile regression
####18:00
win.graph()
par(mfrow=c(2,2))
pinball_loss <- function(tau, y, q) {
  pl_df <- data.frame(tau = tau,
                      y = y,
                      q = q)
  pl_df <- pl_df %>%
    mutate(L = ifelse(y>=q,
                      2*tau/100 * (y-q),
                      2*(1-tau/100) * (q-y)))
  return(pl_df)
}
tau= 99.99
y= N18H00
q=  LQR18 #  NLQR9999, AQR9999

zLQR18 = pinball_loss(tau, y, q)
zLQR18
write.table(zLQR18,"~/pinballfplqr18.txt",sep="\t") 

qloss =zLQR18$L
a=ts(qloss)
plot(a, col="blue",ylab="Pinball loss (LQR18_q0.9999)", xlab="Observation number")
mean(qloss)

####19:00
pinball_loss <- function(tau, y, q) {
  pl_df <- data.frame(tau = tau,
                      y = y,
                      q = q)
  pl_df <- pl_df %>%
    mutate(L = ifelse(y>=q,
                      2*tau/100 * (y-q),
                      2*(1-tau/100) * (q-y)))
  return(pl_df)
}
tau= 99.99
y= N19H00
q=  LQR19 #  NLQR9999, AQR9999

zLQR19 = pinball_loss(tau, y, q)
zLQR19
write.table(zLQR19,"~/pinballfplqr19.txt",sep="\t") 

qloss =zLQR19$L
a=ts(qloss)
plot(a, col="blue",ylab="Pinball loss (LQR19_q0.9999)", xlab="Observation number")
mean(qloss)

####20:00
pinball_loss <- function(tau, y, q) {
  pl_df <- data.frame(tau = tau,
                      y = y,
                      q = q)
  pl_df <- pl_df %>%
    mutate(L = ifelse(y>=q,
                      2*tau/100 * (y-q),
                      2*(1-tau/100) * (q-y)))
  return(pl_df)
}
tau= 99.99
y= N20H00
q=  LQR20 #  NLQR9999, AQR9999

zLQR20 = pinball_loss(tau, y, q)
zLQR20
write.table(zLQR20,"~/pinballfplqr20.txt",sep="\t") 

qloss =zLQR20$L
a=ts(qloss)
plot(a, col="blue",ylab="Pinball loss (LQR20_q0.9999)", xlab="Observation number")
mean(qloss)

####21:00
pinball_loss <- function(tau, y, q) {
  pl_df <- data.frame(tau = tau,
                      y = y,
                      q = q)
  pl_df <- pl_df %>%
    mutate(L = ifelse(y>=q,
                      2*tau/100 * (y-q),
                      2*(1-tau/100) * (q-y)))
  return(pl_df)
}
tau= 99.99
y= N21H00
q=  LQR21 #  NLQR9999, AQR9999

zLQR21 = pinball_loss(tau, y, q)
zLQR21
write.table(zLQR21,"~/pinballfplqr21.txt",sep="\t") 

qloss =zLQR21$L
a=ts(qloss)
plot(a, col="blue",ylab="Pinball loss (LQR21_q0.9999)", xlab="Observation number")
mean(qloss)

########################################################################################################
####Nonlinear quantile regression
####18:00
win.graph()
par(mfrow=c(2,2))
pinball_loss <- function(tau, y, q) {
  pl_df <- data.frame(tau = tau,
                      y = y,
                      q = q)
  pl_df <- pl_df %>%
    mutate(L = ifelse(y>=q,
                      2*tau/100 * (y-q),
                      2*(1-tau/100) * (q-y)))
  return(pl_df)
}
tau= 99.99
y= N18H00
q=  NLQR18 #  NLQR9999, AQR9999

zNLQR18 = pinball_loss(tau, y, q)
zNLQR18
write.table(zNLQR18,"~/pinballfpnqr18.txt",sep="\t") 

qloss =zNLQR18$L
a=ts(qloss)
plot(a, col="blue",ylab="Pinball loss (NLQR18_q0.9999)", xlab="Observation number")
mean(qloss)

####19:00
pinball_loss <- function(tau, y, q) {
  pl_df <- data.frame(tau = tau,
                      y = y,
                      q = q)
  pl_df <- pl_df %>%
    mutate(L = ifelse(y>=q,
                      2*tau/100 * (y-q),
                      2*(1-tau/100) * (q-y)))
  return(pl_df)
}
tau= 99.99
y= N19H00
q=  NLQR19 #  NLQR9999, AQR9999

zNLQR19 = pinball_loss(tau, y, q)
zNLQR19
write.table(zNLQR19,"~/pinballfpnqr19.txt",sep="\t") 

qloss =zNLQR19$L
a=ts(qloss)
plot(a, col="blue",ylab="Pinball loss (NLQR19_q0.9999)", xlab="Observation number")
mean(qloss)

####20:00
pinball_loss <- function(tau, y, q) {
  pl_df <- data.frame(tau = tau,
                      y = y,
                      q = q)
  pl_df <- pl_df %>%
    mutate(L = ifelse(y>=q,
                      2*tau/100 * (y-q),
                      2*(1-tau/100) * (q-y)))
  return(pl_df)
}
tau= 99.99
y= N20H00
q=  NLQR20 #  NLQR9999, AQR9999

zNLQR20 = pinball_loss(tau, y, q)
zNLQR20
write.table(zNLQR20,"~/pinballfpnqr20.txt",sep="\t") 

qloss =zNLQR20$L
a=ts(qloss)
plot(a, col="blue",ylab="Pinball loss (NLQR20_q0.9999)", xlab="Observation number")
mean(qloss)

####21:00
pinball_loss <- function(tau, y, q) {
  pl_df <- data.frame(tau = tau,
                      y = y,
                      q = q)
  pl_df <- pl_df %>%
    mutate(L = ifelse(y>=q,
                      2*tau/100 * (y-q),
                      2*(1-tau/100) * (q-y)))
  return(pl_df)
}
tau= 99.99
y= N21H00
q=  NLQR21 #  NLQR9999, AQR9999

zNLQR21 = pinball_loss(tau, y, q)
zNLQR21
write.table(zNLQR21,"~/pinballfpnqr21.txt",sep="\t") 

qloss =zNLQR21$L
a=ts(qloss)
plot(a, col="blue",ylab="Pinball loss (NLQR21_q0.9999)", xlab="Observation number")
mean(qloss)

#######################################################################################################
####Additive quantile regression
####18:00
win.graph()
par(mfrow=c(2,2))
pinball_loss <- function(tau, y, q) {
  pl_df <- data.frame(tau = tau,
                      y = y,
                      q = q)
  pl_df <- pl_df %>%
    mutate(L = ifelse(y>=q,
                      2*tau/100 * (y-q),
                      2*(1-tau/100) * (q-y)))
  return(pl_df)
}
tau= 99.99
y= N18H00
q=  AQR18 #  NLQR9999, AQR9999

zAQR18 = pinball_loss(tau, y, q)
zAQR18
write.table(zAQR18,"~/pinballfplaqr18.txt",sep="\t") 

qloss =zAQR18$L
a=ts(qloss)
plot(a, col="blue",ylab="Pinball loss (AQR18_q0.9999)", xlab="Observation number")
mean(qloss)

#####19:00
pinball_loss <- function(tau, y, q) {
  pl_df <- data.frame(tau = tau,
                      y = y,
                      q = q)
  pl_df <- pl_df %>%
    mutate(L = ifelse(y>=q,
                      2*tau/100 * (y-q),
                      2*(1-tau/100) * (q-y)))
  return(pl_df)
}
tau= 99.99
y= N19H00
q=  AQR19 #  NLQR9999, AQR9999

zAQR19 = pinball_loss(tau, y, q)
zAQR19
write.table(zAQR19,"~/pinballfplaqr19.txt",sep="\t") 

qloss =zAQR19$L
a=ts(qloss)
plot(a, col="blue",ylab="Pinball loss (AQR19_q0.9999)", xlab="Observation number")
mean(qloss)

#####20:00
pinball_loss <- function(tau, y, q) {
  pl_df <- data.frame(tau = tau,
                      y = y,
                      q = q)
  pl_df <- pl_df %>%
    mutate(L = ifelse(y>=q,
                      2*tau/100 * (y-q),
                      2*(1-tau/100) * (q-y)))
  return(pl_df)
}
tau= 99.99
y= N20H00
q=  AQR20 #  NLQR9999, AQR9999

zAQR20 = pinball_loss(tau, y, q)
zAQR20
write.table(zAQR20,"~/pinballfplaqr20.txt",sep="\t") 

qloss =zAQR20$L
a=ts(qloss)
plot(a, col="blue",ylab="Pinball loss (AQR20_q0.9999)", xlab="Observation number")
mean(qloss)

#####21:00
pinball_loss <- function(tau, y, q) {
  pl_df <- data.frame(tau = tau,
                      y = y,
                      q = q)
  pl_df <- pl_df %>%
    mutate(L = ifelse(y>=q,
                      2*tau/100 * (y-q),
                      2*(1-tau/100) * (q-y)))
  return(pl_df)
}
tau= 99.99
y= N21H00
q=  AQR21 #  NLQR9999, AQR9999

zAQR21 = pinball_loss(tau, y, q)
zAQR21
write.table(zAQR21,"~/pinballfplaqr21.txt",sep="\t") 

qloss =zAQR21$L
a=ts(qloss)
plot(a, col="blue",ylab="Pinball loss (AQR21_q0.9999)", xlab="Observation number")
mean(qloss)

#######################################################################################################
#creating data_train and data_test

load_data_test <- 4164:nrow(NewData)
length(load_data_test)
data_train <- NewData[-load_data_test, ]
length(data_train)
nrow(data_train)
data_test <- NewData[load_data_test, ]
length(data_test)
nrow(data_test)

########################################################################################################
#####CARET MODEL
#####18:00
head(data_train)
N18train <- data_train$N18H00

fit.gbm18 <-train(N18H00 ~ AveMinT + AveTem + AveMaxT + DayType + noltrend18, 
                data= data_train, method = "gbm")# insample
fit.gbm18
summary(fit.gbm18)

#####19:00
N19train <- data_train$N19H00

fit.gbm19 <-train(N19H00 ~ AveMinT + AveTem + AveMaxT + DayType + noltrend19, 
                  data= data_train, method = "gbm")# insample
fit.gbm19
summary(fit.gbm19)

#####20:00
N20train <- data_train$N20H00

fit.gbm20 <-train(N20H00 ~ AveMinT + AveTem + AveMaxT + DayType + noltrend20, 
                  data= data_train, method = "gbm")# insample
fit.gbm20
summary(fit.gbm20)

#####21:00
N21train <- data_train$N21H00

fit.gbm21 <-train(N21H00 ~ AveMinT + AveTem + AveMaxT + DayType + noltrend21, 
                  data= data_train, method = "gbm")# insample
fit.gbm21
summary(fit.gbm21)

########################################################################################################
#####Relative importance #
#####18:00
lmMod18 <- lm(N18train~., data=data_train[-1:-5])  # fit lm() model
relImportance18 <- calc.relimp(lmMod18, type = "lmg", rela = TRUE)  # calculate relative importance scaled to 100
relImportance18
sort(relImportance18$lmg, decreasing=TRUE)  # relative importance

#####19:00
lmMod19 <- lm(N19train~., data=data_train[-1:-5])  # fit lm() model
relImportance19 <- calc.relimp(lmMod19, type = "lmg", rela = TRUE)  # calculate relative importance scaled to 100
relImportance19
sort(relImportance19$lmg, decreasing=TRUE)  # relative importance

#####20:00
lmMod20 <- lm(N20train~., data=data_train[-1:-5])  # fit lm() model
relImportance20 <- calc.relimp(lmMod20, type = "lmg", rela = TRUE)  # calculate relative importance scaled to 100
relImportance20
sort(relImportance20$lmg, decreasing=TRUE)  # relative importance

#####21:00
lmMod21 <- lm(N21train~., data=data_train[-1:-5])  # fit lm() model
relImportance21 <- calc.relimp(lmMod21, type = "lmg", rela = TRUE)  # calculate relative importance scaled to 100
relImportance21
sort(relImportance21$lmg, decreasing=TRUE)  # relative importance

#######################################################################################################
#####LASSO and ELASTICNET
#####18:00
y18 <- N18train
x18 <- cbind(data_train$noltrend18,data_train$DayType,
           data_train$AveTem,data_train$AveMaxT,data_train$AveMinT)

set.seed(2468)
cvfitlasso18=cv.glmnet(x18,y18,alpha = 1, intercept = TRUE, type.measure = "mse")
cvfitlasso18
plot(cvfitlasso18)
coef(cvfitlasso18, s = "lambda.min") # s= Lambda

#####19:00
y19 <- N19train
x19 <- cbind(data_train$noltrend19,data_train$DayType,
             data_train$AveTem,data_train$AveMaxT,data_train$AveMinT)

set.seed(2468)
cvfitlasso19=cv.glmnet(x19,y19,alpha = 1, intercept = TRUE, type.measure = "mse")
cvfitlasso19
plot(cvfitlasso19)
coef(cvfitlasso19, s = "lambda.min") # s= Lambda

#####20:00
y20 <- N20train
x20 <- cbind(data_train$noltrend20,data_train$DayType,
             data_train$AveTem,data_train$AveMaxT,data_train$AveMinT)

set.seed(2468)
cvfitlasso20=cv.glmnet(x20,y20,alpha = 1, intercept = TRUE, type.measure = "mse")
cvfitlasso20
plot(cvfitlasso20)
coef(cvfitlasso20, s = "lambda.min") # s= Lambda

#####21:00
y21 <- N21train
x21 <- cbind(data_train$noltrend21,data_train$DayType,
             data_train$AveTem,data_train$AveMaxT,data_train$AveMinT)

set.seed(2468)
cvfitlasso21=cv.glmnet(x21,y21,alpha = 1, intercept = TRUE, type.measure = "mse")
cvfitlasso21
plot(cvfitlasso21)
coef(cvfitlasso21, s = "lambda.min") # s= Lambda

#######################################################################################################
######Building models
#####AQR model for 18H00 
set.seed(5235)
tun18 <- tuneLearnFast(form=N18H00~s(AveTem, bs="ad")+s(AveMaxT,bs="ad")+ s(DayType, bs="ad")+
                         s(noltrend18,bs="ad"), err = 0.05, qu = 0.9999, data = data_train)
tun18
fit18aqr <-qgam(N18H00~s(AveTem, bs="ad")+s(AveMaxT,bs="ad")+s(DayType, bs="ad")+s(noltrend18,bs="ad")
                , err = 0.05, qu = 0.9999, lsig = tun18$lsig, data = data_train)
summary(fit18aqr, se="boot") #se =" ker" " nid" "boot"

#####AQR model for 19H00 
set.seed(5235)
tun19 <- tuneLearnFast(form=N19H00~s(AveTem, bs="ad")+s(AveMinT,bs="ad")+ s(DayType, bs="ad")+
                         s(noltrend19,bs="ad"), err = 0.05, qu = 0.9999, data = data_train)
tun19
fit19aqr <-qgam(N19H00~s(AveTem, bs="ad")+s(AveMinT,bs="ad")+s(DayType, bs="ad")+s(noltrend19,bs="ad")
                , err = 0.05, qu = 0.9999, lsig = tun19$lsig, data = data_train)
summary(fit19aqr, se="boot") #se =" ker" " nid" "boot"

#####LNR model for 20H00 
fit20lqr = rq(N20H00 ~ AveMinT + DayType + noltrend20, data= data_train, tau=0.9999)
summary.rq(fit20lqr,se="boot") # can use se = "nid" or se="ker"

#####LNR model for 21H00 
fit21lqr = rq(N21H00 ~ AveMinT + DayType + noltrend21, data= data_train, tau=0.9999)
summary.rq(fit21lqr,se="boot") # can use se = "nid" or se="ker"

#######################################################################################################
#####Forecasting from developed models
#####18:00
win.graph()
par(mfrow=c(1,2))
fit18.forecast <- predict(fit18aqr, newdata = data_test)
fit18.forecast
#write.table(fit1.forecast,"~/fq0999.txt",sep="\t")
f18 <- ts(fit18.forecast)

plot(data_test$N18H00, ylab="Electricity demand (MW) at 18:00", xlab="Observation number",lwd=1, 
     type="l", ylim=c(500,980))
lines(f18,col="red",lty=2,lwd=1)
legend("topright",col=c("black","red"), lty=1:2,lwd=1, legend=c("Actuals", "Forecasts (AQR18:00)"))

#####19:00
fit19.forecast <- predict(fit19aqr, newdata = data_test)
fit19.forecast
#write.table(fit1.forecast,"~/fq0999.txt",sep="\t")
f19 <- ts(fit19.forecast)

plot(data_test$N19H00, ylab="Electricity demand (MW) at 19:00", xlab="Observation number",lwd=1, 
     type="l", ylim=c(500,1050))
lines(f19,col="red",lty=2,lwd=1)
legend("topright",col=c("black","red"), lty=1:2,lwd=1, legend=c("Actuals", "Forecasts (AQR19:00)"))

#####20:00
fit20.forecast <- predict(fit20lqr, newdata = data_test)
fit20.forecast
#write.table(fit1.forecast,"~/fq0999.txt",sep="\t")
f20 <- ts(fit20.forecast)

plot(data_test$N20H00, ylab="Electricity demand (MW) at 20:00", xlab="Observation number",lwd=1, 
     type="l", ylim=c(500,1000))
lines(f20,col="red",lty=2,lwd=1)
legend("topright",col=c("black","red"), lty=1:2,lwd=1, legend=c("Actuals", "Forecasts (LQR20:00)"))

#####21:00
fit21.forecast <- predict(fit21lqr, newdata = data_test)
fit21.forecast
#write.table(fit1.forecast,"~/fq0999.txt",sep="\t")
f21 <- ts(fit21.forecast)

plot(data_test$N21H00, ylab="Electricity demand (MW) at 21:00", xlab="Observation number",lwd=1, 
     type="l", ylim=c(500,950))
lines(f21,col="red",lty=2,lwd=1)
legend("topright",col=c("black","red"), lty=1:2,lwd=1, legend=c("Actuals", "Forecasts (LQR21:00)"))

##########################################################################################################
#####Operational forecasting

#####18:00
#####AveTem
set.seed(5235)
fit18Ave <-qgam(AveTem~ s(Month,bs="cc",k=12)+s(DayType,bs="cc",k=7)+s(Trend,bs="cc")+
                  t2(Month,DayType, bs=c("cc","cc"),k=c(12,7)),err = 0.05, qu = 0.9999,
                 data = data_train)# insample
summary(fit18Ave, se="boot") #se =" ker" " nid" "boot"

#Forecasting for AveTem 
win.graph()
fit18Ave.forecast<- predict(fit18Ave, newdata= data_test)
fit18Ave.forecast
plot(fit18Ave.forecast,col="blue")
fit18Ave.forecast <- round(fit18Ave.forecast,3)
write.table(fit18Ave.forecast,"~/fit18Ave.forecast.txt",sep="\t")

#####AveMinT
set.seed(5235)
fit18AveMinT <-qgam(AveMinT~ s(Month,bs="cc",k=12)+s(DayType,bs="cc",k=7)+s(Trend,bs="cc")+
                      t2(Month,DayType, bs=c("cc","cc"),k=c(12,7)),err = 0.05, qu = 0.9999,
                    data = data_train)
summary(fit18AveMinT, se="boot") #se =" ker" " nid" "boot"

#Forecasting for AveMinT 
win.graph()
fit18AveMinT.forecast<- predict(fit18AveMinT, newdata= data_test)
fit18AveMinT.forecast
plot(fit18AveMinT.forecast,col="blue")
fit18AveMinT.forecast <- round(fit18AveMinT.forecast,3)
write.table(fit18AveMinT.forecast,"~/fit18AveMinT.forecast.txt",sep="\t")

#####AveMaxT
set.seed(5235)
fit18AveMaxT <-qgam(AveMaxT ~ s(Month,bs="cc",k=12)+s(DayType,bs="cc",k=7)+s(Trend,bs="cc")+
                      t2(Month,DayType, bs=c("cc","cc"),k=c(12,7)),err = 0.05, qu = 0.9999,
                    data = data_train)
summary(fit18AveMaxT, se="boot") #se =" ker" " nid" "boot"

#Forecasting for AveMaxT 
win.graph()
fit18AveMaxT.forecast<- predict(fit18AveMaxT, newdata= data_test)
fit18AveMaxT.forecast
plot(fit18AveMaxT.forecast,col="blue")
fit18AveMaxT.forecast <- round(fit18AveMaxT.forecast,3)
write.table(fit18AveMaxT.forecast,"~/fit18AveMaxT.forecast.txt",sep="\t")

#####noltrend18
set.seed(5235)
fitnoltrend18 <-qgam(noltrend18 ~ s(Month,bs="cc",k=12)+s(DayType,bs="cc",k=7)+s(Trend,bs="cc")+
                       t2(Month,DayType, bs=c("cc","cc"),k=c(12,7)),err = 0.05, qu = 0.9999,
                     data = data_train)
summary(fitnoltrend18, se="boot") #se =" ker" " nid" "boot"

#Forecasting for noltrend18 
win.graph()
fitnoltrend18.forecast<- predict(fitnoltrend18, newdata= data_test)
fitnoltrend18.forecast
plot(fitnoltrend18.forecast,col="blue")
fitnoltrend18.forecast <- round(fitnoltrend18.forecast,3)
write.table(fitnoltrend18.forecast,"~/fitnoltrend18.forecast.txt",sep="\t")

##########################################################################################################
#####19:00
#####noltrend18
set.seed(5235)
fitnoltrend19 <-qgam(noltrend19 ~ s(Month,bs="cc",k=12)+s(DayType,bs="cc",k=7)+s(Trend,bs="cc")+
                       t2(Month,DayType, bs=c("cc","cc"),k=c(12,7)),err = 0.05, qu = 0.9999,
                     data = data_train)
summary(fitnoltrend19, se="boot") #se =" ker" " nid" "boot"

#Forecasting for noltrend18 
win.graph()
fitnoltrend19.forecast<- predict(fitnoltrend19, newdata= data_test)
fitnoltrend19.forecast
plot(fitnoltrend19.forecast,col="blue")
fitnoltrend19.forecast <- round(fitnoltrend19.forecast,3)
write.table(fitnoltrend19.forecast,"~/fitnoltrend19.forecast.txt",sep="\t")

##########################################################################################################
#####20:00
#####Ave
fit20Ave <- qgam(AveTem~ s(Month,bs="cc",k=12)+s(DayType,bs="cc",k=7)+s(Trend,bs="cc")+
                   t2(Month,DayType, bs=c("cc","cc"),k=c(12,7)),err = 0.05, qu = 0.9999,
                 data = data_train)
summary.rq(fit20Ave,se="boot") # can use se = "nid" or se="ker"

#Forecasting for Ave20 
win.graph()
fit20Ave.forecast<- predict(fit20Ave, newdata= data_test)
fit20Ave.forecast
plot(fit20Ave.forecast,col="blue")
fit20Ave.forecast <- round(fit20Ave.forecast,3)
write.table(fit20Ave.forecast,"~/fit20Ave.forecast.txt",sep="\t")

#####AveMinT
fit20AveMinT = rq(AveMinT ~ DayType + Trend + Month + DayType*Month, data= data_train, tau=0.9999)
summary.rq(fit20AveMinT,se="boot") # can use se = "nid" or se="ker"

#Forecasting for AveMinT 
win.graph()
fit20AveMinT.forecast<- predict(fit20AveMinT, newdata= data_test)
fit20AveMinT.forecast
plot(fit20AveMinT.forecast,col="blue")
fit20AveMinT.forecast <- round(fit20AveMinT.forecast,3)
write.table(fit20AveMinT.forecast,"~/fit20AveMinT.forecast.txt",sep="\t")

#####AveMaxT
fit20AveMaxT = rq(AveMaxT ~ DayType + Trend + Month + DayType*Month, data= data_train, tau=0.9999)
summary.rq(fit20AveMaxT,se="boot") # can use se = "nid" or se="ker"

#Forecasting for AveMaxT
win.graph()
fit20AveMaxT.forecast<- predict(fit20AveMaxT, newdata= data_test)
fit20AveMaxT.forecast
plot(fit20AveMaxT.forecast,col="blue")
fit20AveMaxT.forecast <- round(fit20AveMaxT.forecast,3)
write.table(fit20AveMaxT.forecast,"~/fit20AveMaxT.forecast.txt",sep="\t")

#####noltrend20
fitnoltrend20 <-qgam(noltrend20 ~ s(Month,bs="cc",k=12)+s(DayType,bs="cc",k=7)+s(Trend,bs="cc")+
                         t2(Month,DayType, bs=c("cc","cc"),k=c(12,7)),err = 0.05, qu = 0.9999,
                       data = data_train)
summary.rq(fitnoltrend20,se="boot") # can use se = "nid" or se="ker"

#Forecasting for noltrend20
win.graph()
fitnoltrend20.forecast<- predict(fitnoltrend20, newdata= data_test)
fitnoltrend20.forecast
plot(fitnoltrend20.forecast,col="blue")
fitnoltrend20.forecast <- round(fitnoltrend20.forecast,3)
write.table(fitnoltrend20.forecast,"~/fitnoltrend20.forecast.txt",sep="\t")

#####noltrend21
fitnoltrend21 <-qgam(noltrend21 ~ s(Month,bs="cc",k=12)+s(DayType,bs="cc",k=7)+s(Trend,bs="cc")+
                       t2(Month,DayType, bs=c("cc","cc"),k=c(12,7)),err = 0.05, qu = 0.9999,
                     data = data_train)
summary.rq(fitnoltrend21,se="boot") # can use se = "nid" or se="ker"

#Forecasting for noltrend21
win.graph()
fitnoltrend21.forecast<- predict(fitnoltrend21, newdata= data_test)
fitnoltrend21.forecast
plot(fitnoltrend21.forecast,col="blue")
fitnoltrend21.forecast <- round(fitnoltrend21.forecast,3)
write.table(fitnoltrend21.forecast,"~/fitnoltrend21.forecast.txt",sep="\t")

##########################################################################################################
#####Attach data
attach(Foper)
head(Foper)

######Building models for operational forecasting
#####AQR model for 18H00 
set.seed(5235)
tun18ope <- tuneLearnFast(form=N18H00~s(Ave18, bs="ad")+s(AveMaxT18,bs="ad")+ s(DayType, bs="ad")+
                         s(noltrend18,bs="ad"), err = 0.05, qu = 0.9999, data = Foper)
tun18ope
fit18ope <-qgam(N18H00~s(Ave18, bs="ad")+s(AveMaxT18,bs="ad")+s(DayType, bs="ad")+s(noltrend18,bs="ad")
                , err = 0.05, qu = 0.9999, lsig = tun18ope$lsig, data = Foper)
summary(fit18ope, se="boot") #se =" ker" " nid" "boot"

#####AQR model for 19H00 
set.seed(5235)
tun19ope <- tuneLearnFast(form=N19H00~s(Ave18, bs="ad")+s(AveMinT18,bs="ad")+ s(DayType, bs="ad")+
                         s(noltrend19,bs="ad"), err = 0.05, qu = 0.9999, data = Foper)
tun19ope
fit19ope <-qgam(N19H00~s(Ave18, bs="ad")+s(AveMinT18,bs="ad")+s(DayType, bs="ad")+s(noltrend19,bs="ad")
                , err = 0.05, qu = 0.9999, lsig = tun19ope$lsig, data = Foper)
summary(fit19ope, se="boot") #se =" ker" " nid" "boot"

#####LNR model for 20H00 
fit20ope = rq(N20H00 ~  AveMaxT18 +  Ave18 +  AveMinT18 + DayType + noltrend20, data= Foper, tau=0.9999)
summary.rq(fit20ope,se="boot") # can use se = "nid" or se="ker"

#####LNR model for 21H00 
fit21ope = rq(N21H00 ~ AveMinT18 + DayType + noltrend21, data= Foper, tau=0.9999)
summary.rq(fit21ope,se="boot") # can use se = "nid" or se="ker"

#####Forecasting from developed models operational
#####18:00
win.graph()
par(mfrow=c(1,2))
fit18ope.forecast <- predict(fit18ope, newdata = Foper)
fit18ope.forecast
#write.table(fit1.forecast,"~/fq0999.txt",sep="\t")
f18ope <- ts(fit18ope.forecast)

plot(Foper$N18H00, ylab="Electricity demand (MW) at 18:00", xlab="Observation number",lwd=1, 
     type="l", ylim=c(500,980))
lines(f18ope,col="red",lty=2,lwd=1)
legend("topright",col=c("black","red"), lty=1:2,lwd=1, legend=c("Actuals", "Forecasts (AQR18:00)"))

#####19:00
fit19ope.forecast <- predict(fit19ope)
fit19ope.forecast
#write.table(fit1.forecast,"~/fq0999.txt",sep="\t")
f19ope <- ts(fit19ope.forecast)

plot(Foper$N19H00, ylab="Electricity demand (MW) at 19:00", xlab="Observation number",lwd=1, 
     type="l", ylim=c(500,1050))
lines(f19ope,col="red",lty=2,lwd=1)
legend("topright",col=c("black","red"), lty=1:2,lwd=1, legend=c("Actuals", "Forecasts (AQR19:00)"))

#####20:00
fit20ope.forecast <- predict(fit20ope)
fit20ope.forecast
#write.table(fit1.forecast,"~/fq0999.txt",sep="\t")
f20ope <- ts(fit20ope.forecast)

plot(Foper$N20H00, ylab="Electricity demand (MW) at 20:00", xlab="Observation number",lwd=1, 
     type="l", ylim=c(500,1000))
lines(f20ope,col="red",lty=2,lwd=1)
legend("topright",col=c("black","red"), lty=1:2,lwd=1, legend=c("Actuals", "Forecasts (LQR20:00)"))

#####21:00
fit21ope.forecast <- predict(fit21ope)
fit21ope.forecast
#write.table(fit1.forecast,"~/fq0999.txt",sep="\t")
f21ope <- ts(fit21ope.forecast)

plot(Foper$N21H00, ylab="Electricity demand (MW) at 21:00", xlab="Observation number",lwd=1, 
     type="l", ylim=c(500,950))
lines(f21ope,col="red",lty=2,lwd=1)
legend("topright",col=c("black","red"), lty=1:2,lwd=1, legend=c("Actuals", "Forecasts (LQR21:00)"))

#############################################################################################################
#####Out of sample forecasts
#####Attach data
attach(Foper)
head(Foper)

#####creating data_train and data_test

load_data_test1 <- 1042:nrow(Foper)
length(load_data_test1)
data_train1 <- Foper[-load_data_test1, ]
length(data_train1)
nrow(data_train1)
data_test1 <- Foper[load_data_test1, ]
length(data_test1)
nrow(data_test1)

##########Operationa forecasting for out of sample
#####18:00
#####AveTem out
set.seed(5235)
fit18Aveout <-qgam(Ave18~ s(Month,bs="cc",k=12)+s(DayType,bs="cc",k=7)+s(Trend,bs="cc")+
                  t2(Month,DayType, bs=c("cc","cc"),k=c(12,7)),err = 0.05, qu = 0.9999,
                data = data_train1)# insample
summary(fit18Ave18out, se="boot") #se =" ker" " nid" "boot"

#Forecasting for AveTem 
win.graph()
fit18Aveout.forecast<- predict(fit18Aveout, newdata= data_test1)
fit18Aveout.forecast
plot(fit18Aveout.forecast,col="blue")
fit18Aveout.forecast <- round(fit18Aveout.forecast,3)
write.table(fit18Aveout.forecast,"~/fit18Aveout.forecast.txt",sep="\t")

#####AveMinT
set.seed(5235)
fit18AveMinTout <-qgam(AveMinT18~ s(Month,bs="cc",k=12)+s(DayType,bs="cc",k=7)+s(Trend,bs="cc")+
                      t2(Month,DayType, bs=c("cc","cc"),k=c(12,7)),err = 0.05, qu = 0.9999,
                    data = data_train1)
summary(fit18AveMinTout, se="boot") #se =" ker" " nid" "boot"

#Forecasting for AveMinT 
win.graph()
fit18AveMinTout.forecast<- predict(fit18AveMinTout, newdata= data_test1)
fit18AveMinTout.forecast
plot(fit18AveMinTout.forecast,col="blue")
fit18AveMinTout.forecast <- round(fit18AveMinTout.forecast,3)
write.table(fit18AveMinTout.forecast,"~/fit18AveMinTout.forecast.txt",sep="\t")

#####AveMaxT
set.seed(5235)
fit18AveMaxTout <-qgam(AveMaxT18 ~ s(Month,bs="cc",k=12)+s(DayType,bs="cc",k=7)+s(Trend,bs="cc")+
                      t2(Month,DayType, bs=c("cc","cc"),k=c(12,7)),err = 0.05, qu = 0.9999,
                    data = data_train1)
summary(fit18AveMaxTout, se="boot") #se =" ker" " nid" "boot"

#Forecasting for AveMaxT 
win.graph()
fit18AveMaxTout.forecast<- predict(fit18AveMaxTout, newdata= data_test1)
fit18AveMaxTout.forecast
plot(fit18AveMaxTout.forecast,col="blue")
fit18AveMaxTout.forecast <- round(fit18AveMaxTout.forecast,3)
write.table(fit18AveMaxTout.forecast,"~/fit18AveMaxTout.forecast.txt",sep="\t")

#####noltrend18
set.seed(5235)
fitnoltrend18out <-qgam(noltrend18 ~ s(Month,bs="cc",k=12)+s(DayType,bs="cc",k=7)+s(Trend,bs="cc")+
                       t2(Month,DayType, bs=c("cc","cc"),k=c(12,7)),err = 0.05, qu = 0.9999,
                     data = data_train1)
summary(fitnoltrend18out, se="boot") #se =" ker" " nid" "boot"

#Forecasting for noltrend18 
win.graph()
fitnoltrend18out.forecast<- predict(fitnoltrend18out, newdata= data_test1)
fitnoltrend18out.forecast
plot(fitnoltrend18out.forecast,col="blue")
fitnoltrend18out.forecast <- round(fitnoltrend18out.forecast,3)
write.table(fitnoltrend18out.forecast,"~/fitnoltrend18out.forecast.txt",sep="\t")

##########################################################################################################
#####19:00
#####noltrend19
set.seed(5235)
fitnoltrend19out <-qgam(noltrend19 ~ s(Month,bs="cc",k=12)+s(DayType,bs="cc",k=7)+s(Trend,bs="cc")+
                       t2(Month,DayType, bs=c("cc","cc"),k=c(12,7)),err = 0.05, qu = 0.9999,
                     data = data_train1)
summary(fitnoltrend19out, se="boot") #se =" ker" " nid" "boot"

#Forecasting for noltrend19 
win.graph()
fitnoltrend19out.forecast<- predict(fitnoltrend19out, newdata= data_test1)
fitnoltrend19out.forecast
plot(fitnoltrend19out.forecast,col="blue")
fitnoltrend19out.forecast <- round(fitnoltrend19out.forecast,3)
write.table(fitnoltrend19out.forecast,"~/fitnoltrend19out.forecast.txt",sep="\t")

#####noltrend20
fitnoltrend20out <-qgam(noltrend20 ~ s(Month,bs="cc",k=12)+s(DayType,bs="cc",k=7)+s(Trend,bs="cc")+
                       t2(Month,DayType, bs=c("cc","cc"),k=c(12,7)),err = 0.05, qu = 0.9999,
                     data = data_train1)
summary.rq(fitnoltrend20out,se="boot") # can use se = "nid" or se="ker"

#Forecasting for noltrend20
win.graph()
fitnoltrend20out.forecast<- predict(fitnoltrend20out, newdata= data_test1)
fitnoltrend20out.forecast
plot(fitnoltrend20out.forecast,col="blue")
fitnoltrend20out.forecast <- round(fitnoltrend20out.forecast,3)
write.table(fitnoltrend20out.forecast,"~/fitnoltrend20out.forecast.txt",sep="\t")

#####noltrend21
fitnoltrend21out <-qgam(noltrend21 ~ s(Month,bs="cc",k=12)+s(DayType,bs="cc",k=7)+s(Trend,bs="cc")+
                       t2(Month,DayType, bs=c("cc","cc"),k=c(12,7)),err = 0.05, qu = 0.9999,
                     data = data_train1)
summary.rq(fitnoltrend21out,se="boot") # can use se = "nid" or se="ker"

#Forecasting for noltrend21
win.graph()
fitnoltrend21out.forecast<- predict(fitnoltrend21out, newdata= data_test1)
fitnoltrend21out.forecast
plot(fitnoltrend21out.forecast,col="blue")
fitnoltrend21out.forecast <- round(fitnoltrend21out.forecast,3)
write.table(fitnoltrend21out.forecast,"~/fitnoltrend21out.forecast.txt",sep="\t")

############################################################################################################
#####Attach data
attach(Fout)
head(Fout)

#####creating data_train and data_test

load_data_test2 <- 1042:nrow(Fout)
length(load_data_test2)
data_train2 <- Foper[-load_data_test2, ]
length(data_train2)
nrow(data_train2)
data_test2 <- Fout[load_data_test2, ]
length(data_test2)
nrow(data_test2)

################Out of sample forecasts for hour 18:00
set.seed(5235)
tun18out <- tuneLearnFast(form=N18H00~s(Ave18, bs="ad")+s(AveMaxT18,bs="ad")+ s(DayType, bs="ad")+
                            s(noltrend18,bs="ad"), err = 0.05, qu = 0.9999, data = data_train2)
tun18out
fit18out <-qgam(N18H00~s(Ave18, bs="ad")+s(AveMaxT18,bs="ad")+s(DayType, bs="ad")+s(noltrend18,bs="ad")
                , err = 0.05, qu = 0.9999, lsig = tun18out$lsig, data = data_train2)
summary(fit18out, se="boot") #se =" ker" " nid" "boot"


fit18out.forecast <- predict(fit18out, newdata = data_test2)
fit18out.forecast
fit18out.forecast <- round(fit18out.forecast,3)
write.table(fit18out.forecast,"~/fit18out.forecast.txt",sep="\t")

################Out of sample forecasts for hour 19:00
set.seed(5235)
tun19out <- tuneLearnFast(form=N19H00~s(Ave18, bs="ad")+s(AveMinT18,bs="ad")+ s(DayType, bs="ad")+
                            s(noltrend19,bs="ad"), err = 0.05, qu = 0.9999, data = data_train2)
tun19out
fit19out <-qgam(N19H00~s(Ave18, bs="ad")+s(AveMinT18,bs="ad")+s(DayType, bs="ad")+s(noltrend19,bs="ad")
                , err = 0.05, qu = 0.9999, lsig = tun19out$lsig, data = data_train2)
summary(fit19out, se="boot") #se =" ker" " nid" "boot"

fit19out.forecast <- predict(fit19out, newdata = data_test2)
fit19out.forecast
fit19out.forecast <- round(fit19out.forecast,3)
write.table(fit19out.forecast,"~/fit19out.forecast.txt",sep="\t")

################Out of sample forecasts for hour 20:00
fit20out = rq(N20H00 ~  AveMaxT18 +  Ave18 +  AveMinT18 + DayType + noltrend20, data= data_train2, tau=0.9999)
summary.rq(fit20out,se="boot") # can use se = "nid" or se="ker"

fit20out.forecast <- predict(fit20out, newdata = data_test2)
fit20out.forecast
fit20out.forecast <- round(fit20out.forecast,3)
write.table(fit20out.forecast,"~/fit20out.forecast.txt",sep="\t")

################Out of sample forecasts for hour 21:00
fit21out = rq(N21H00 ~ AveMinT18 + DayType + noltrend21, data= data_train2, tau=0.9999)
summary.rq(fit21out,se="boot") # can use se = "nid" or se="ker"

fit21out.forecast <- predict(fit21out, newdata = data_test2)
fit21out.forecast
fit21out.forecast <- round(fit21out.forecast,3)
write.table(fit21out.forecast,"~/fit21out.forecast.txt",sep="\t")
