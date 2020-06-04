mlb <- read.csv("MLB Stats 11_6_19.csv")
mlb <- mlb[,-c(17:18)]
mlb <- na.omit(mlb)
mlb$All.Star <- as.factor(mlb$All.Star)
mlb$Player.Name <- as.character(mlb$Player.Name)
mlb$teamID <- as.character(mlb$teamID)
mlb$Team <- as.character(mlb$Team)
mlb$Franchise <- as.character(mlb$Franchise)
set.seed(10)

#################
#################### LEARNING ABOUT THE DATASET
summary(mlb)
str(mlb)
dim(mlb)
#MLB summary
options(scipen = 999)
m.mean7 <- round(sapply(mlb[,c(12:32)], mean), 2)
m.sum7 <- round(sapply(mlb[,c(12:32)], sum), 2)
m.min7 <- round(sapply(mlb[,c(12:32)], min), 2)
m.max7 <- round(sapply(mlb[,c(12:32)], max), 2)
m.median7 <-round(sapply(mlb[,c(12:32)], median), 2)
m.length7 <- round(sapply(mlb[,c(12:32)], length), 2)
m.sd7 <- round(sapply(mlb[,c(12:32)], sd), 2)
summary_df <- data.frame(m.mean7, m.sum7, m.min7, m.max7, m.median7, m.length7, m.sd7)
names(summary_df) <- c("Mean", "Sum", "Min", "Max", "Median", "Length", "Standard Dev.")
summary_df

cor.mat.batting <- round(cor(mlb[,c(3:4, 12:32)]), 2); cor.mat.batting

######################################################################################################
#######                           PRINCIPAL COMPENENT ANALYSIS                                 #######
######################################################################################################
pr.mlb <- prcomp(mlb[,c( 12:31)], scale. = T); summary(pr.mlb)
pr.mlb$rot[,1:7]
pca.mlb <- pr.mlb$x
new_predictors <- pr.mlb$x[,1:7]
new_predictors <- as.data.frame(new_predictors); new_predictors
new.mlb <- cbind(new_predictors, mlb)

plot(pca.mlb[,1] ~ pca.mlb[,2], col = mlb$All.Star,xlim= c(-5,5))
identify(pca.mlb[,1] ~ pca.mlb[,2], pos = T)
the_stand_outs <- t(t(mlb[c(2520, 2517, 20571, 24016),])); the_stand_outs

#PCA TESTING
pca.testing <- prcomp(mlb[,c(12:32)], scale. = T); pca.testing$rot[,1:7]
pca.plot.testing <- pca.testing$x
plot(jitter(pca.plot.testing[,1]) ~ pca.plot.testing[,2], xlim = c(0,5), ylim = c(5,20))
identify(pca.plot.testing[,1] ~ pca.plot.testing[,2], pos = t)
t(t(mlb[c(2520, 24016, 2507, 2515, 2517, 1040, 5002),]))

#SB,H,height,weight***
pca.sb<-prcomp(mlb[,c(3:4,15,23)])
summary(pca.sb)
pca.sb$rot[,1:4] #"Sb.Hits", "Sb.Weight", "Sb.SB", "Sb.H"
sb<- pca.sb$x

#G,H,AVG,SLG,OPS,2B,3B,HR, RBI,SO***
  #pca.ab<-prcomp(mlb[c(12,15,16,18,19,21,22,23,24,27)])
  #summary(pca.ab)
  #pca.ab$rot[,1:2]
  #ab<-pca.ab$x
  #plot(ab, col=mlb$All.Star)

######################################################################################################
#######                                        SAMPLING                                        #######
######################################################################################################
# RANDOM SAMPLING
train <- sample(rownames(mlb), dim(mlb)[1]*.6)
train_set <- mlb[train,]
test <- sample(setdiff(rownames(mlb),train_set), dim(mlb)[1]*.4)
test_set <- mlb[test,]


#BALANCING THE DATA
set.seed(12)
train.bal <- mlb$All.Star==1
train.bal <- mlb[train.bal,]
random.bal <- sample(setdiff(rownames(mlb), train.bal), dim(mlb)[1]*.7)
random.bal <- mlb[random.bal,]
bal.set <- rbind(train.bal, random.bal)
bal.train <- sample(rownames(bal.set), dim(bal.set)[1]*.6)
bal.train <- bal.set[bal.train,]
bal.test <- sample(setdiff(rownames(mlb),bal.set), dim(bal.set)[1]*.4)
bal.test <- bal.set[bal.test,]


######################################################################################################
#######                                      AGGREGATION                                       #######
######################################################################################################
options(scipen = 999)
total.HR <- aggregate(mlb$HR, by = list(Team = mlb$Team, TeamID = mlb$teamID), FUN = sum)
total.RBI <- aggregate(mlb$RBI, by = list(Team = mlb$Team, TeamID = mlb$teamID), FUN = sum)
total.H <- aggregate(mlb$H, by = list(Team = mlb$Team, TeamID = mlb$teamID), FUN = sum)
total.X2B <- aggregate(mlb$X2B, by = list(Team = mlb$Team, TeamID = mlb$teamID), FUN = sum)
total.X3B <- aggregate(mlb$X3B, by = list(Team = mlb$Team, TeamID = mlb$teamID), FUN = sum)
total.R <- aggregate(mlb$R, by = list(Team = mlb$Team, TeamID = mlb$teamID), FUN = sum)
total.sal <- aggregate(mlb$salary, by = list(Team = mlb$Team, TeamID = mlb$teamID), FUN = sum)
total.avg <- aggregate(mlb$AVG, by = list(Team = mlb$Team, TeamID = mlb$teamID), FUN = mean)
team.stats.df <- data.frame(total.HR, total.RBI[,-c(1:2)], total.H[,-c(1:2)], total.R[,-c(1:2)], total.X2B[,-c(1:2)], total.X3B[,-c(1:2)], total.sal[,-c(1:2)], total.avg[,-c(1:2)])
names(team.stats.df) <- c("TEAM", "TEAM ID", "HR", "RBI", "H", "R", "X2B", "X3B", "Salary", "AVG"); team.stats.df


######################################################################################################
#######                                       GRAPHS                                           #######
######################################################################################################
pairs(mlb[,c(3:4, 12:32)])
pairs(mlb[,c(3:4, 12:20)])
options(scipen = 99)
boxplot(mlb$salary ~ mlb$Season, outline = F, xlab = "Season", ylab = "Salary")
par(mfrow = c(3,1))
boxplot(mlb$HR, ylab = "Home Runs", col = 2, horizontal = T, staplewex = 1, boxwex = 1.5, cex = 1)
boxplot(mlb$RBI, ylab = "Runs Batted In", col = 3, horizontal = T, staplewex = 1, boxwex = 1.5, cex = 1)
boxplot(mlb$G, ylab = "Games Played", col = 4, horizontal = T, staplewex = 1, boxwex = 1.5, cex = 1)
par(mfrow = c(1,1))
boxplot(mlb$salary ~ mlb$League, outline = F, horizontal = T, xlab = "Salary", ylab = "League")
boxplot(mlb$salary ~ mlb$bats, outline = F, horizontal = F, xlab = "Batting Position", ylab = "Salary")
boxplot(mlb$HR ~ mlb$bats, ylab = "Total Home Runs", xlab = "Batting Position", varwidth = T)
boxplot(mlb$RBI ~ mlb$bats, ylab = "Total RBIs", xlab = "Batting Position", varwidth = T)
heatmap(cor.mat.batting, Colv = NA, Rowv = NA, symm = T, main = "Heatmap of Predictors")
plot(team.stats.df$HR ~ team.stats.df$RBI)
pairs(~., data = team.stats.df[,c(3:9)])
barplot(team.stats.df$HR, names.arg = team.stats.df$`TEAM ID`)
boxplot(mlb$salary ~ mlb$All.Star, outline = F, ylab = "Salary", xlab = "All Star")
boxplot(mlb$salary ~ mlb$Team, outline = F, varwidth = T, ylab = "Salary", xlab = "Team")
boxplot(mlb$OPS ~ mlb$All.Star)


######################################################################################################

#######                                 BEST SUBSET SELECTION                                  #######

######################################################################################################
#################
install.packages("forecast")
install.packages("leaps")
install.packages("boots")
library(forecast)
library(leaps)
library(boot)

#################
#LINEAR MODELS
#Data Partitioning For Subset Selection
vars <- 14:33
mlb.sub <- mlb[,vars]
train.sub <- train_set[,vars]
test.sub <- test_set[,vars]

#################### HR
# Forward
lm.hr.f = regsubsets(HR ~., data=train.sub, nvmax = 19, method = "forward")
sum.hr <- summary(lm.hr.f)
plot(sum.hr$rss, type = "b")
points(which.min(sum.hr$rss), sum.hr$rss[which.min(sum.hr$rss)], cex = 3, pch = 20, col = 2)
plot(sum.hr$adjr2, type = "b")
points(which.max(sum.hr$adjr2), sum.hr$adjr2[which.max(sum.hr$adjr2)], cex = 3, pch = 20, col = 4)
coef(lm.hr.f, 19)

# Backward
lm.hr.b = regsubsets(HR ~., data=train.sub, nvmax = 19, method = "backward")
sum.hr.bac <- summary(lm.hr.b)
plot(sum.hr.bac$rss, type = "b")
points(which.min(sum.hr.bac$rss), sum.hr.bac$rss[which.min(sum.hr.bac$rss)], cex = 3, pch = 20, col = 2)
plot(sum.hr.bac$adjr2, type = "b")
points(which.max(sum.hr.bac$adjr2), sum.hr.bac$adjr2[which.max(sum.hr.bac$adjr2)], cex = 3, pch = 20, col = 4)
coef(lm.hr.b, 19)

# Best
lm.hr.bes = regsubsets(HR ~., data=train.sub, nvmax = 19)
sum.hr.bes <- summary(lm.hr.bes)
plot(sum.hr.bes$rss, type = "b")
points(which.min(sum.hr.bes$rss), sum.hr.bes$rss[which.min(sum.hr.bes$rss)], cex = 3, pch = 20, col = 2)
plot(sum.hr.bes$adjr2, type = "b")
points(which.max(sum.hr.bes$adjr2), sum.hr.bes$adjr2[which.max(sum.hr.bes$adjr2)], cex = 3, pch = 20, col = 4)
coef(lm.hr.bes, 19)

par(mfrow = c(1,2))
plot(lm.hr.bes, scale = "Cp")
plot(lm.hr.bes, scale = "bic")
plot(lm.hr.bes, scale = "r2")
plot(lm.hr.bes, scale = "adjr2")
#Validation errors plotted to the Training RSS: Best subset
#Forward
test.mat <- model.matrix(HR ~., data = test.sub)
val.errors <- rep(NA, 19)

for(e in 1:19){
  coefe <- coef(lm.hr.f, id = e)
  pred <- test.mat[,names(coefe)]%*%coefe
  val.errors[e] <- mean((test.sub$HR-pred)^2)
}
val.errors
plot(sqrt(val.errors), type = "b", ylab = "Root MSE", ylim = c(2,5))
points(sqrt(lm.hr.f$rss[-1]/8833), col = 4, type = "b")
points(which.min(val.errors), sqrt(val.errors)[which.min(val.errors)], col = 2, cex = 3, pch = 4)
coef(lm.hr.f, 19)

#Backward
val.errors.hrbac <- rep(NA, 19)

for(e in 1:19){
  coefe <- coef(lm.hr.b, id = e)
  pred <- test.mat[,names(coefe)]%*%coefe
  val.errors.hrbac[e] <- mean((test.sub$HR-pred)^2)
}
val.errors.hrbac
plot(sqrt(val.errors.hrbac), type = "b", ylab = "Root MSE", ylim = c(2,5))
points(sqrt(lm.hr.b$rss[-1]/8833), col = 4, type = "b")
points(which.min(val.errors.hrbac), sqrt(val.errors.hrbac)[which.min(val.errors.hrbac)], col = 2, cex = 3, pch = 4)
coef(lm.hr.b, 19)

#Best
val.errors.hrbest <- rep(NA, 19)

for(e in 1:19){
  coefe <- coef(lm.hr.bes, id = e)
  pred <- test.mat[,names(coefe)]%*%coefe
  val.errors.hrbest[e] <- mean((test.sub$HR-pred)^2)
}
val.errors.hrbest
plot(sqrt(val.errors.hrbest), type = "b", ylab = "Root MSE", ylim = c(2,7), main = "Best Subset: HR RMSE")
points(sqrt(lm.hr.bes$rss[-1]/8833), col = 4, type = "b")
points(which.min(val.errors.hrbest), sqrt(val.errors.hrbest)[which.min(val.errors.hrbest)], col = 2, cex = 3, pch = 4)
legend("topright", legend = c("Training RSS", "Validation RMSE", "Optimal Model Size"), col = c(4,1,2), lty = c(1,1,-1), pch = c(-1,-1,4))
coef(lm.hr.bes, 19)

par(xpd = T)
plot(val.errors, type = "b", ylab = "Validation Errors", main = "Home Run Subset Selection Test Error")
lines(val.errors.hrbac, col = 2)
lines(val.errors.hrbest, lty = 2, col = 4, lwd = 2)
legend("topright", inset = c(0, -.002), legend = (c("Forward", "Backward", "Both")), col = c(8,2,4), lty = c(1,1,2))

########################## SB
#Run this first
mlb.sub.sb <- cbind(mlb.sub, sb)
mlb.sub.sb <- mlb.sub.sb[,-2]
train.sb <- mlb.sub.sb[train,]
test.sb <- mlb.sub.sb[test,]

#Forward
lm.sb.f = regsubsets(SB ~.,data=train.sb, nvmax = 22, method = "forward")
sum.sb.f <- summary(lm.sb.f)
plot(sum.hr$rss, type = "b")
points(which.min(sum.hr$rss), sum.hr$rss[which.min(sum.hr$rss)], pch = 20, col = 2)
plot(sum.sb.f$adjr2, type = "b")
points(which.max(sum.sb.f$adjr2), sum.sb.f$adjr2[which.max(sum.sb.f$adjr2)], pch = 20, col = 2)

#Backward
lm.sb.b = regsubsets(SB ~.,data=train.sb, nvmax = 22, method = "backward")
sum.sb.b <- summary(lm.sb.b)
plot(sum.sb.b$rss, type = "b")
points(which.min(sum.sb.b$rss), sum.sb.b$rss[which.min(sum.sb.b$rss)], pch = 20, col = 2)
plot(sum.sb.b$adjr2, type = "b")
points(which.max(sum.sb.b$adjr2), sum.sb.b$adjr2[which.max(sum.sb.b$adjr2)], pch = 20, col = 2)
coef(lm.sb.b, 4)

#Best
lm.sb.bes = regsubsets(SB ~.,data=train.sb, nvmax = 22)
sum.sb.bes <- summary(lm.sb.bes)
plot(sum.sb.bes$rss, type = "b")
points(which.min(sum.sb.bes$rss), sum.sb.bes$rss[which.min(sum.sb.bes$rss)], pch = 20, col = 2)
plot(sum.sb.bes$adjr2, type = "b")
points(which.max(sum.sb.bes$adjr2), sum.sb.bes$adjr2[which.max(sum.sb.bes$adjr2)], pch = 20, col = 2)
coef(lm.sb.bes, 4)

par(mfrow = c(1,2))
plot(lm.sb.bes, scale = "Cp")
plot(lm.sb.bes, scale = "bic")
plot(lm.sb.bes, scale = "r2")
plot(lm.sb.bes, scale = "adjr2")

#Validation errors plotted to the Training RSS: Best subset for Stolen Bases
test.mat2 <- model.matrix(SB ~., data = test.sb)

#Forward
val.errors2 <- rep(NA, 22)

for(e in 1:22){
  coefe <- coef(lm.sb.f, id = e)
  pred <- test.mat2[,names(coefe)]%*%coefe
  val.errors2[e] <- mean((test.sb$SB-pred)^2)
}
val.errors2
plot(sqrt(val.errors2), type = "b", ylab = "Root MSE", ylim = c(0,9), xlim = c(1, 24))
points(sqrt(lm.sb.f$rss[-1]/8833), col = 4, type = "b")
points(which.min(val.errors2), sqrt(val.errors2)[which.min(val.errors2)], col = 2, cex = 3, pch = 4)
coef(lm.sb.bes, 4)

#Backward
val.errors2.bac <- rep(NA, 22)

for(e in 1:22){
  coefe <- coef(lm.sb.b, id = e)
  pred <- test.mat2[,names(coefe)]%*%coefe
  val.errors2.bac[e] <- mean((test.sb$SB-pred)^2)
}
val.errors2.bac
plot(sqrt(val.errors2.bac), type = "b", ylab = "Root MSE", ylim = c(0,6), xlim = c(0, 25))
points(sqrt(lm.sb.b$rss[-1]/8833), col = 4, type = "b")
points(which.min(val.errors2.bac), sqrt(val.errors2.bac)[which.min(val.errors2.bac)], col = 2, cex = 3, pch = 4)
coef(lm.sb.b, 4)

#Best
val.errors2.best <- rep(NA, 22)

for(e in 1:22){
  coefe <- coef(lm.sb.bes, id = e)
  pred <- test.mat2[,names(coefe)]%*%coefe
  val.errors2.best[e] <- mean((test.sb$SB-pred)^2)
}
val.errors2.best
plot(sqrt(val.errors2.best), type = "b", ylab = "Root MSE", ylim = c(0,9), xlim = c(1, 24), main = "Best Subset: SB RMSE")
points(sqrt(lm.sb.bes$rss[-1]/8833), col = 4, type = "b")
points(which.min(val.errors2.best), sqrt(val.errors2.best)[which.min(val.errors2.best)], col = 2, cex = 3, pch = 4)
legend("topright", legend = c("Training RSS", "Validation RMSE", "Optimal Model Size"), col = c(4,1,2), lty = c(1,1,-1), pch = c(-1,-1,4))
coef(lm.sb.bes, 4)

par(xpd = T)
plot(val.errors2, type = "b", ylab = "Validation Errors", main = "Stolen Bases Subset Selection Test Error")
lines(val.errors2.bac, col = 2)
lines(val.errors2.best, lty = 2, col = 4, lwd = 2)
legend("topright", inset = c(0, -.002), legend = (c("Forward", "Backward", "Both")), col = c(8,2,4), lty = c(1,1,2))

############### OPS
#Forward
lm.ops.f = regsubsets(OPS ~., data=train.sub, nvmax = 19, method = "forward")
sum.ops.f <- summary(lm.ops.f)
plot(sum.ops.f$rss, type = "b")
points(which.min(sum.ops.f$rss), sum.ops.f$rss[which.min(sum.ops.f$rss)], pch = 20, col = 2)
plot(sum.ops.f$adjr2, type = "b")
points(which.max(sum.ops.f$adjr2), sum.ops.f$adjr2[which.max(sum.ops.f$adjr2)], pch = 20, col = 2)
coef(lm.ops.f, 17)

#Backward
lm.ops.b = regsubsets(OPS ~., data=train.sub, nvmax = 19, method = "backward")
sum.ops.b <- summary(lm.ops.b)
plot(sum.ops.b$rss, type = "b")
points(which.min(sum.ops.b$rss), sum.ops.b$rss[which.min(sum.ops.b$rss)], pch = 20, col = 2)
plot(sum.ops.b$adjr2, type = "b")
points(which.max(sum.ops.b$adjr2), sum.ops.b$adjr2[which.max(sum.ops.b$adjr2)], pch = 20, col = 2)
coef(lm.ops.f, 17)

#Best
lm.ops.bes = regsubsets(OPS ~., data=train.sub, nvmax = 19)
sum.ops.bes <- summary(lm.ops.bes)
plot(sum.ops.bes$rss, type = "b")
points(which.min(sum.ops.bes$rss), sum.ops.bes$rss[which.min(sum.ops.bes$rss)], pch = 20, col = 2)
plot(sum.ops.bes$adjr2, type = "b")
points(which.max(sum.ops.bes$adjr2), sum.ops.bes$adjr2[which.max(sum.ops.bes$adjr2)], pch = 20, col = 2)
coef(lm.ops.f, 17)

par(mfrow = c(1,2))
plot(lm.ops.bes, scale = "Cp")
plot(lm.ops.bes, scale = "bic")
plot(lm.ops.bes, scale = "r2")
plot(lm.ops.bes, scale = "adjr2")

######################################################################################################
#######                                     VALIDATION ERROR                                   #######
######################################################################################################
test.mat3 <- model.matrix(OPS ~., data = test.sub)

#Forward
val.errors3 <- rep(NA, 19)

for(e in 1:19){
  coefe <- coef(lm.ops.f, id = e)
  pred <- test.mat3[,names(coefe)]%*%coefe
  val.errors3[e] <- mean((test.sub$OPS-pred)^2)
}
val.errors3
plot(sqrt(val.errors3), type = "b", ylab = "Root MSE", ylim = c(0.01, .20))
points(sqrt(lm.ops.f$rss[-1]/8833), col = 4, type = "b")
points(which.min(val.errors3), sqrt(val.errors3)[which.min(val.errors3)], col = 2, cex = 3, pch = 4)
coef(lm.ops.f, 17)

#Backward
val.errors3.bac <- rep(NA, 19)

for(e in 1:19){
  coefe <- coef(lm.ops.b, id = e)
  pred <- test.mat3[,names(coefe)]%*%coefe
  val.errors3.bac[e] <- mean((test.sub$OPS-pred)^2)
}
val.errors3.bac
plot(sqrt(val.errors3.bac), type = "b", ylab = "Root MSE", ylim = c(0.01, .20))
points(sqrt(lm.ops.b$rss[-1]/8833), col = 4, type = "b")
points(which.min(val.errors3.bac), sqrt(val.errors3.bac)[which.min(val.errors3.bac)], col = 2, cex = 3, pch = 4)
coef(lm.ops.b, 17)

#Best
val.errors3.best <- rep(NA, 19)

for(e in 1:19){
  coefe <- coef(lm.ops.bes, id = e)
  pred <- test.mat3[,names(coefe)]%*%coefe
  val.errors3.best[e] <- mean((test.sub$OPS-pred)^2)
}
val.errors3.best
plot(sqrt(val.errors3.best), type = "b", ylab = "Root MSE", ylim = c(0.01, .15), xlim = c(1, 22),main = "Best Subset: OPS RMSE")
points(sqrt(lm.ops.bes$rss[-1]/8833), col = 4, type = "b")
points(which.min(val.errors3.best), sqrt(val.errors3.best)[which.min(val.errors3.best)], col = 2, cex = 3, pch = 4)
legend("topright", legend = c("Training RSS", "Validation RMSE", "Optimal Model Size"), col = c(4,1,2), lty = c(1,1,-1), pch = c(-1,-1,4))
coef(lm.ops.bes, 17)

par(xpd = T)
plot(val.errors3, type = "b", ylab = "Validation Errors", main = "OPS Subset Selection Test Error")
lines(val.errors3.bac, col = 2)
lines(val.errors3.best, lty = 2, col = 4, lwd = 2)
legend("topright", inset = c(0, -.002), legend = (c("Forward", "Backward", "Both")), col = c(8,2,4), lty = c(1,1,2))

#THE RSS PLOT IS GENERATING AHOTHER PREDICTOR, FIX

#Create predict function for regsubsets
predict.regsubsets <- function(object, newdata, id,...){
  form=as.formula(object$call[[2]])
  mat=model.matrix(form, newdata)
  coefi=coef(object, id)
  xvars=names(coefi)
  mat[,xvars]%*%coefi
}

#Home Run Validation
predd <- predict.regsubsets(lm.hr.bes, test.sub, 19)
sqrt(mean((predd-test.sub$HR)^2)) #RMSE

val.hr <- rep(NA, 19)
for(e in 1:19){
  pred <- predict.regsubsets(lm.hr.bes, test.sub, e)
  val.hr[e] <- mean((test.sub$HR-pred)^2)
}
plot(sqrt(val.hr), type = "b", ylab = "RMSE", main = "Best Subset Selection Validation RMSE: HR", xlab = "# of Variables")
points(which.min(sqrt(val.hr)), sqrt(val.hr)[which.min(sqrt(val.hr))], col = 2, pch = 5, cex = 2)
coef(lm.hr.f, 19)


#Stolen Bases Validation
preddd <- predict.regsubsets(lm.sb.bes, test.sb, 4)
sqrt(mean((test.sb$SB - preddd)^2)) #RMSE 

val.sb <- rep(NA, 22)
for(e in 1:22){
  pred <- predict.regsubsets(lm.sb.f, test.sb, e)
  val.sb[e] <- mean((test.sb$SB-pred)^2)
}
plot(sqrt(val.sb), type = "b", ylab = "RMSE", main = "Best Subset Selection Validation RMSE: SB", xlab = "# of Variables")
points(which.min(sqrt(val.sb)), sqrt(val.sb)[which.min(sqrt(val.sb))], col = 2, pch = 5, cex = 2)
coef(lm.sb.f, 4)

#OPS Validation
predv <- predict.regsubsets(lm.ops.b, test.sub, 19)
sqrt(mean((predv-test.sub$OPS)^2)) #RMSE

val.ops <- rep(NA, 19)
for(e in 1:19){
  pred <- predict.regsubsets(lm.ops.f, test.sub, e)
  val.ops[e] <- mean((test.sub$OPS-pred)^2)
}
plot(sqrt(val.ops), type = "b",  ylab = "RMSE", main = "Best Subset Selection Validation RMSE: OPS", xlab = "# of Variables")
points(which.min(sqrt(val.ops)), sqrt(val.ops)[which.min(sqrt(val.ops))], col = 2, pch = 5, cex = 2)
which.min(sqrt(val.ops))
coef(lm.ops.f, 17)

######################################################################################################
#######                                 10 FOLD CROSS VALIDATION                               #######
######################################################################################################
##HR
folds <- sample(rep(1:10, length = (nrow(mlb.sub))))
table(folds)

hrcv.error <- matrix(NA, 10, 19)

for(k in 1:10){
  best.fit <- regsubsets(HR ~., data = mlb.sub[folds!=k,], nvmax = 19)
  for(e in 1:19){
    pred <- predict.regsubsets(best.fit, mlb.sub[folds==k,], e)
    hrcv.error[k, e] <- mean((mlb.sub$HR[folds==k]-pred)^2)
  }
}
hrcv.error
#Every row is a test error, every column is the variable corresponding to that test error
rmse.cv <- sqrt(apply(hrcv.error, 2, mean)) #2 means take the mean by column (1 would be row wise)
plot(rmse.cv, type = "b", main = "HR Best Subset: K-fold RMSE", ylab = "RMSE", xlab = "# of Variables")
points(which.min(rmse.cv), rmse.cv[which.min(rmse.cv)], col = 2, pch = 20, cex = 2)
t(t(coef(lm.hr.bes, 19)))

##SB
foldss <- sample(rep(1:10, length = (nrow(mlb.sub.sb))))
table(foldss)

sbcv.error <- matrix(NA, 10, 21)

for(k in 1:10){
  best.fit <- regsubsets(SB ~., data = mlb.sub.sb[foldss!=k,], nvmax = 21)
  for(e in 1:21){
    pred <- predict.regsubsets(best.fit, mlb.sub.sb[foldss==k,], e)
    sbcv.error[k, e] <- mean((mlb.sub.sb$SB[foldss==k]-pred)^2)
  }
}
sbcv.error
#Every row is a test error, every column is the variable corresponding to that test error
rmse.cvv <- sqrt(apply(sbcv.error, 2, mean)) #2 means take the mean by column (1 would be row wise)
plot(rmse.cvv, type = "b", main = "SB Best Subset: K-fold RMSE", ylab = "RMSE", xlab = "# of Variables")
points(which.min(rmse.cvv), rmse.cvv[which.min(rmse.cvv)], col = 2, pch = 20, cex = 2)
t(t(coef(lm.sb.bes, 4)))

##OPS
foldz <- sample(rep(1:10, length = (nrow(mlb.sub))))
table(foldz)

opscv.error <- matrix(NA, 10, 19)

for(k in 1:10){
  best.fit <- regsubsets(OPS ~., data = mlb.sub[foldz!=k,], nvmax = 19)
  for(e in 1:19){
    pred <- predict.regsubsets(best.fit, mlb.sub[foldz==k,], e)
    opscv.error[k, e] <- mean((mlb.sub$OPS[foldz==k]-pred)^2)
  }
}
opscv.error
#Every row is a test error, every column is the variable corresponding to that test error
rmse.cvz <- sqrt(apply(opscv.error, 2, mean)) #2 means take the mean by column (1 would be row wise)
plot(rmse.cvz, type = "b",main = "OPS Best Subset: K-fold RMSE", ylab = "RMSE", xlab = "# of Variables")
points(which.min(rmse.cvz), rmse.cvz[which.min(rmse.cvz)], col = 2, pch = 20, cex = 2)
t(t(coef(lm.ops.bes, 15)))

######################################################################################################
#######                                     RIDGE & LASSO                                      #######
######################################################################################################
# RIDGE
install.packages("glmnet")
library(glmnet)

x = model.matrix(HR~.-1,data=train.sub)
y = train.sub$HR
fit.ridge.hr=glmnet(x,y,alpha=0)
fit.ridge.hr$lambda
plot(fit.ridge.hr,xvar="lambda",label=TRUE, main = "HR Ridge")

cv.ridge.hr = cv.glmnet(x,y,alpha=0)   ### cross validation is crashing R
plot(cv.ridge.hr)
lamb.hr <- cv.ridge.hr$lambda.1se
coef.hr <- coef(fit.ridge.hr, newdata = test.sub, s = lamb.hr)

xx = model.matrix(SB~.-1,data=train.sb)
yy = train.sb$SB
fit.ridge.sb=glmnet(xx,yy,alpha=0)
plot(fit.ridge.sb,xvar="lambda",label=TRUE, main = "SB Ridge")

cv.ridge.sb = cv.glmnet(xx,yy,alpha=0) 
plot(cv.ridge.sb)
lamb.sb <- cv.ridge.sb$lambda.1se
coef.sb <- coef(fit.ridge.sb, newdata = test.sb, s = lamb.sb)


xv = model.matrix(OPS~.-1,data=train.sub)
yv = train.sub$OPS
fit.ridge.ops=glmnet(xv,yv,alpha=0)
plot(fit.ridge.ops,xvar="lambda",label=TRUE, main = "OPS Ridge")

cv.ridge.ops = cv.glmnet(xv,yv,alpha=0) 
plot(cv.ridge.ops)
lamb.ops <- cv.ridge.ops$lambda.1se
coef.ops <- coef(fit.ridge.ops, newdata = test.sub, s = lamb.ops)

#Lasso 
fit.lasso.hr=glmnet(x,y, alpha = 1)
plot(fit.lasso.hr,xvar="lambda",label=TRUE)
cv.lasso.hr = cv.glmnet(x,y,alpha=1)
plot(cv.lasso.hr)
lamb.hr <- cv.lasso.hr$lambda.1se
coef.hr <- coef(fit.lasso.hr, newdata = test.sub, s = lamb.hr)

fit.lasso.sb=glmnet(xx,yy, alpha = 1)
plot(fit.lasso.sb,xvar="lambda",label=TRUE)
cv.lasso.sb = cv.glmnet(xx,yy,alpha=1)
plot(cv.lasso.sb)
lamb.sb <- cv.lasso.sb$lambda.1se
coef.sb <- coef(fit.lasso.sb, newdata = test.sb, s = lamb.sb)

fit.lasso.ops=glmnet(xv,yv, alpha = 1)
plot(fit.lasso.ops,xvar="lambda",label=TRUE)
cv.lasso.ops = cv.glmnet(xv,yv,alpha=1)
plot(cv.lasso.ops)
lamb.ops <- cv.lasso.ops$lambda.1se
coef.ops <- coef(fit.lasso.ops, newdata = test.sub, s = lamb.ops)

##########
#HR
newX <- model.matrix(~.-HR,data=test.sub)
newY <- test.sub$HR
pred=predict(fit.lasso.hr,newx = newX)
dim(pred)
rmse=sqrt(apply((newY-pred)^2,2,mean))
plot(log(fit.lasso.hr$lambda),rmse,type="b",xlab="Log(lambda)", main = "HR Lasso Regression")

lam.best=fit.lasso.hr$lambda[order(rmse)[1]]

lam.best
coef(fit.lasso.hr,s=lam.best)

#SB
newXX <- model.matrix(~.-SB,data=test.sb)
newYY <- test.sb$HR
pred2=predict(fit.lasso.sb,newx = newXX)
rmse2=sqrt(apply((newYY-pred)^2,2,mean))
plot(log(fit.lasso.sb$lambda),rmse2,type="b",xlab="Log(lambda)",  main = "SB Lasso Regression")

lam.best2=fit.lasso.sb$lambda[order(rmse2)[1]]

lam.best2
coef(fit.lasso.sb,s=lam.best2)

#OPS
newXV <- model.matrix(~.-OPS,data=test.sub)
newYV <- test.sub$HR
pred3=predict(fit.lasso.ops,newx = newXV)
rmse3=sqrt(apply((newYV-pred3)^2,2,mean))
plot(log(fit.lasso.ops$lambda),rmse3,type="b",xlab="Log(lambda)",  main = "OPS Lasso Regression")

lam.best3=fit.lasso.ops$lambda[order(rmse)[1]]

lam.best3
coef(fit.lasso.ops,s=lam.best3)

######################################################################################################
#######                           LOGISTIC REGRESSION (All-Star)                               #######
######################################################################################################
install.packages("pROC")
install.packages("caret")
install.packages("e1071")
install.packages("MLmetrics")
install.packages("klaR")
library(pROC)
library(e1701)
library(caret)
library(MLmetrics)
library(klaR)

#Using stepclass() to find best model
bal.sub <- bal.set[,vars]
bal.sub.train <- bal.train[,vars]
bal.sub.test <- bal.test[,vars]
bal.d <- bal.sub[,-20]
bal.c <- bal.sub[,20]
glm.log <- stepclass(bal.d, bal.c, "lda", maxvar = 22, direction = "both")
plot(glm.log)

#Best Subset
log.best <- glm(All.Star ~ salary + SO + RBI*R + IBB + OPS + SB, data = bal.sub.train, family = "binomial"); summary(log.best)
best.prob <- predict(log.best, newdata = bal.sub.test, type = "response")
best.pred <- rep(0, 5087)
best.pred[best.prob>=.2] <- 1
table(best.pred, bal.test$All.Star)
confusionMatrix(as.factor(best.pred), as.factor(bal.test$All.Star))

## CV for Best Subset
best.cv <- cv.glm(bal.sub, log.best, K = 10); best.cv$delta

#Forward has a low misclassification error compared to Best Subset, moreover, Forward selection can accurately predict an All-Star with 89% of the time.

#ROC Curve for the original Logistic Model and the forward selection model
roc.df <- data.frame(best.prob, bal.test$All.Star)
r <- roc(roc.df$bal.test.All.Star, roc.df$best.prob)
plot.roc(r, print.auc = T, main = "Logistic Regression ROC Curve")

######################################################################################################
#######                                       LDA & QDA                                        #######
######################################################################################################
library(MASS)
lda.1 <- lda(All.Star ~ AVG*OPS + log(salary) + RBI*R + IBB + G*AB + SB, data = bal.train); lda.1
lda.pred <- predict(lda.1, newdata = bal.test, type = "class")
lda.class <- lda.pred$class
table(lda.class, bal.test$All.Star)
confusionMatrix(as.factor(lda.class), as.factor(bal.test$All.Star))

#QDA
qda.1 <- qda(All.Star ~ AVG*OPS + log(salary) + RBI*R + IBB + G*AB + SB, data = bal.train); qda.1
qda.pred <- predict(qda.1, newdata = bal.test, type = "response")
qda.class <- qda.pred$class
table(qda.class, bal.test$All.Star)
confusionMatrix(as.factor(qda.class), as.factor(bal.test$All.Star))

#################
#Partition Plots
#Power Index
partimat(Power.Index ~ OPS + ISO, data = bal.train, method = "lda") #10% error rate
partimat(Power.Index ~ OPS + ISO, data = bal.train, method = "qda") #1% error rate
partimat(Power.Index ~ OPS + ISO, data = bal.train, method = "naiveBayes") #5% error rate


#Partition Plots
#All Star
partimat(All.Star ~ RBI + salary, data = bal.train, method = "lda") #21% error rate

######################################################################################################
#######                           CLASSIFICATION TREE (All-Star)                               #######
######################################################################################################

install.packages("tree")
install.packages("randomForest")
install.packages("gbm")
install.packages("ROCR")
library(ROCR)
library(gbm)
library(randomForest)
library(tree)
options(scipen = 99)
star.tree <- tree(All.Star ~., data = bal.sub.train)
plot(star.tree)
text(star.tree, pretty = 0, cex = .65)
summary(star.tree)

cv.star.tree <- cv.tree(star.tree, FUN = prune.misclass)
plot(cv.star.tree)

star.prune <- prune.misclass(star.tree, best = 4)
plot(star.prune)
text(star.prune, pretty = 0, cex = .85)

pred <- predict(star.prune, newdata = bal.sub.test, type = "class")
table(pred, bal.sub.test$All.Star)
(453/(451+453))

rf1 <- randomForest(All.Star ~., data = bal.sub.train, mtry = 1, ntree = 400)
rf1$importance
varImpPlot(rf1, main = "Variables Important in All-Star Classification")
rf1$err.rate[400]

pred.rf <- predict(rf1, newdata = bal.sub.test, type = "class")
table(pred.rf, bal.sub.test$All.Star)
confusionMatrix(as.factor(pred.rf), as.factor(bal.sub.test$All.Star))

oob.err <- double(19)

for(i in 1:19){
  rf1 <- randomForest(All.Star ~., data = bal.sub.train, mtry = i, ntree = 400)
  oob.err[i] <- rf1$err.rate[400]
  cat(i, "")
}

plot(oob.err, type = "b", main = "OOB Misclassification Rate", ylab = "OOB Error", xlab = "Variables per Forest")

pred2.rf <- predict(rf1, newdata = bal.sub.test, type = "prob")[,2]
roc.rf.prob <- prediction(pred2.rf, bal.sub.test$All.Star)
roc.log.prob <- prediction(best.prob, bal.sub.test$All.Star)
perf <- performance(roc.rf.prob, "tpr", "fpr" )
perf2 <- performance(roc.log.prob, "tpr", "fpr")
plot(perf, col = 4, main = "ROC Curves")
plot(perf2, add = TRUE, col = 2)
legend("bottomright", legend = c("Random Forest", "Logistic Rgression"), col = c(4,2), lty = c(1,1), cex = 1.5)
