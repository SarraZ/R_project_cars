library(GGally)
library(ggplot2)
library(leaps)
#read the data and rename variables
auto <- read.table(file = "imports-85.data.txt", sep=",")
names(auto) <- c("symboling","normalized","make","fuelType", "aspiration","numOfDoors","bodyStyle","driveWheels","engineLocation","wheelBase","length","width","height","curbWeight","engineType","numOfCylinders","engineSize","fuelSystem","bore","stroke","compressionRatio","horsepower","peakRpm","cityMpg","highwayMpg","price")

#convert from ? to N.A
auto$normalized[auto$normalized =="?"] <- NA
auto$stroke[auto$stroke =="?"] <- NA
auto$bore[auto$bore=="?"] <- NA
auto$price[auto$price=="?"] <- NA
auto$horsepower[auto$horsepower =="?"] <- NA
auto$peakRpm[auto$peakRpm =="?"] <- NA
auto$numOfDoors[auto$numOfDoors =="?"] <- NA

auto$price<-strtoi(auto$price) #convertir les chaines en  numeriques 
#auto$price<-as.numeric(auto$price)


#convert to factor or to numeric and replace NA value with the mean of the variable
auto$symboling=as.factor(auto$symboling)
auto$normalized=as.numeric(as.character(auto$normalized))

# predict variable stroke
auto$stroke=as.numeric(as.character(auto$stroke))
mean(auto$stroke,na.rm=TRUE)
auto$stroke[is.na(auto$stroke)]<-3.255423

#predict variable bore
auto$bore=as.numeric(as.character(auto$bore))
mean(auto$bore,na.rm=TRUE)
auto$bore[is.na(auto$bore)]<-3.329751

# predict N.A of price
mean(auto$price,na.rm=TRUE)
auto$price[is.na(auto$price)]<-13207.13

#predict horsepower

auto$horsepower=as.numeric(as.character(auto$horsepower))
mean(auto$horsepower,na.rm=TRUE)
auto$horsepower[is.na(auto$horsepower)]<-3.329751

#predict peakRpm
auto$peakRpm=as.numeric(as.character(auto$peakRpm))
mean(auto$peakRpm,na.rm=TRUE)
auto$peakRpm[is.na(auto$peakRpm)]<-3.329751

#predict numofDoors
auto$numOfDoors=as.factor(as.character(auto$numOfDoors))
plot(auto$numOfDoors)
auto$numOfDoors[is.na(auto$numOfDoors)]<-"four"


#std(auto$price)
hist(auto$price)                 

pie(table(auto$aspiration))


pie(table(auto$fuelSystem))
summary(auto$fuelSystem)

pie(table(auto$fuelType))
summary(auto$fuelType)
#mean(auto$price ,which(!(is.na(auto$price))))
#auto$price[which (auto$price == NA)] <- 13207.13
#auto$price <- replace(auto$price,which(is.na(auto$price)),13207.13)
#auto$price<- lapply(auto$price, FUN=as.numeric)
#auto$price <- replace(auto$price,10,mean(auto$price,na.rm=T))

#Partie 1 : Statistique univariée
#On remarque que la variable à prédire qui est Normalized a bcq de valeurs manquantes , donc contruit un nouveau data qui contient que les valeurs valides 
auto$normalized=as.numeric(as.character(auto$normalized))
Train.Normalized <- subset(auto,!(is.na(auto$normalized)))
Test.Normalized <- subset(auto,is.na(auto$normalized),na.rm=TRUE)

hist(Train.Normalized$normalized)
lines(Train.Normalized$normalized, col="darkred")
#D'aprés le graphe , cette distibution ne semble pas qu'elle suit la loi normale, on peut vérifier avec la fonction de répartition
#et en traçant le diagramme Quantile-Quantile ou diagramme Q-Q plot qui  permet d'évaluer la pertinence de l'ajustement de notre variable à expliquer   avec une loi gaussienne réduite
#Le QQ-plot d'une variable permet de tester graphiquement sa normalit´e. On l'obtient
 
#Plus le nuage est proche de la droite, plus la variable peut-^etre consid´er´ee comme normale.

par(mfrow=c(1,3))
plot(ecdf(Train.Normalized$normalized), main="fonction de répartition",xlab="")
title(outer=TRUE, main="\n Distribution des pertes moyennes d'une voiture <Normalized>" )
qqnorm(Train.Normalized$normalized)
qqline(Train.Normalized$normalized)
boxplot(Train.Normalized$normalized, main="La boite à moustache de la variable Normalized")

#On remarque qu'il existe des valeurs abérrantes (mch sure )

#On peut vérifier et étudier encore la normalité de Normalized  avec le test Shapiro 
#L'hypothèse nulle de ce test est H0 : la variable est normale.
#Cette p-valeur repr´esente la probabilité que l'on a de se tromper en rejetant H0.
shapiro.test(Train.Normalized$normalized)
#Habituellement, on effectue des tests à  un seuil de 5%, cela signifie que l'on ne souhaite pas avoir plus de 5% de chance de se tromper en rejetant H0 qui est <Normalized suit la loi normale>.
#On compare donc la p-value à 0.05. Notre p-value < 0.05, alors on peut rejeter H0.

#Statistique bivariée

qualitatives <- sapply(Train.Normalized, is.factor)
#jeu de données pour les var numériques
quantitatives <-Train.Normalized[!qualitatives]
#jeu de données pour les var qualit
qual<-Train.Normalized[qualitatives]
qualitatives
#ggpairs(Train.Normalized, columns = which(!qualitatives))

mcor <- cor(quantitatives)
mcor

#variables qualitatives
par(mfrow=c(1,3))
boxplot(Train.Normalized$normalized~Train.Normalized$fuelType,data=Train.Normalized)
boxplot(Train.Normalized$normalized~Train.Normalized$engineType,data=Train.Normalized)
boxplot(Train.Normalized$normalized~Train.Normalized$driveWheels,data=Train.Normalized)

#ici on a tracé ce boxplot pour déterminer l'influence de la variable "gas/diesel" sur notre variable réponse
#Nous remarquons que les voitures roulantes avec du gaz ont des pertes plus remarquables que celle avec diesel et avec une variance plus grande autour de la médiane
#Nous remarquons aussi que les voitures qui ont les moteurs de type OHCF et DOHC et OHC ont plus de pertes que les autres
#et concernant les roues ,les voitures qui possédent le type de transmission Rear Wheel Drive, c'est à dire que les roues arrière qui sont motrices perdent plus que les voitures qui ont d'autres types de transmission

qualitatives

#nOMMER la variable à expliquer
Nor<-Train.Normalized$normalized

#Recherche des variables significatif 
set.seed(123)
M0 <- lm(Nor~1,Train.Normalized)
M11 <- lm(Nor~Train.Normalized$peakRpm,Train.Normalized)
M12 <- lm(Nor~Train.Normalized$horsepower,Train.Normalized)
M2 <- lm(Nor~Train.Normalized$peakRpm+Train.Normalized$horsepower,Train.Normalized)
anova(M0,M11,M2)
anova(M0,M12,M2)

#On remarque en comparant les p-value des variables significatives que le HorsePower peut nous donner plus d'informations sur les pertes que peakRpm
#pour gagner plus de précision
M0 <- lm(Nor~1,Train.Normalized)
M11 <- lm(Nor~Train.Normalized$wheelBase,Train.Normalized)
M12 <- lm(Nor~Train.Normalized$length,Train.Normalized)
M2 <- lm(Nor~Train.Normalized$wheelBase+Train.Normalized$length,Train.Normalized)
anova(M0,M11,M2)
anova(M0,M12,M2)
#on garde les deux

M0 <- lm(Nor~1,Train.Normalized)
M11 <- lm(Nor~Train.Normalized$wheelBase,Train.Normalized)
M12 <- lm(Nor~Train.Normalized$width,Train.Normalized)
M2 <- lm(Nor~Train.Normalized$wheelBase+Train.Normalized$width,Train.Normalized)
anova(M0,M11,M2)
anova(M0,M12,M2)
#on garde les deux


M0 <- lm(Nor~1,Train.Normalized)
M11 <- lm(Nor~Train.Normalized$width,Train.Normalized)
M12 <- lm(Nor~Train.Normalized$length,Train.Normalized)
M2 <- lm(Nor~Train.Normalized$width+Normalized$length,Train.Normalized)
anova(M0,M11,M2)
anova(M0,M12,M2)

#on garde width et length

M0 <- lm(Nor~1,Train.Normalized)
M11 <- lm(Nor~Train.Normalized$curbWeight,Train.Normalized)
M12 <- lm(Nor~Train.Normalized$wheelBase,Train.Normalized)
M2 <- lm(Nor~Train.Normalized$curbWeight+Train.Normalized$wheelBase,Train.Normalized)
anova(M0,M11,M2)
anova(M0,M12,M2)
#on garde les deux

M0 <- lm(Nor~1,Train.Normalized)
M11 <- lm(Nor~Train.Normalized$curbWeight,Train.Normalized)
M12 <- lm(Nor~Train.Normalized$length,Train.Normalized)
M2 <- lm(Nor~Train.Normalized$curbWeight+Train.Normalized$length,Train.Normalized)
anova(M0,M11,M2)
anova(M0,M12,M2)
#on garde les 2

M0 <- lm(Nor~1,Train.Normalized)
M11 <- lm(Nor~Train.Normalized$curbWeight,Train.Normalized)
M12 <- lm(Nor~Train.Normalized$width,Train.Normalized)
M2 <- lm(Nor~Train.Normalized$curbWeight+Train.Normalized$width,Train.Normalized)
anova(M0,M11,M2)
anova(M0,M12,M2)
#on peut supprimer la variable width

M0 <- lm(Nor~1,Train.Normalized)
M11 <- lm(Nor~Train.Normalized$curbWeight,Train.Normalized)
M12 <- lm(Nor~Train.Normalized$engineSize,Train.Normalized)
M2 <- lm(Nor~Train.Normalized$curbWeight+Train.Normalized$engineSize,Train.Normalized)
M3 <- lm(Nor~Train.Normalized$curbWeight+Train.Normalized$engineSize+Train.Normalized$wheelBase,Train.Normalized)

anova(M0,M11,M2)
anova(M0,M12,M2)
anova(M0,M2,M3)

#On peut dire que les enginesize, wheelBase et curbweigth sont significatives ensembles 


M0 <- lm(Nor~1,Train.Normalized)
M11 <- lm(Nor~Train.Normalized$cityMpg,Train.Normalized)
M12 <- lm(Nor~Train.Normalized$horsepower,Train.Normalized)
M2 <- lm(Nor~Train.Normalized$cityMpg+Train.Normalized$horsepower,Train.Normalized)
anova(M0,M11,M2)
anova(M0,M12,M2)
#cityMpg et horsepower sont toutes les deux significatives ,mais en comparant leur p-value,on peut dire que horsepower est plus significative


M0 <- lm(Nor~1,Train.Normalized)
M11 <- lm(Nor~Train.Normalized$width,Train.Normalized)
M12 <- lm(Nor~Train.Normalized$price,Train.Normalized)
M2 <- lm(Nor~Train.Normalized$width+Train.Normalized$price,Train.Normalized)
anova(M0,M11,M2)
anova(M0,M12,M2)
#on garde les deux

M0 <- lm(Nor~1,Train.Normalized)
M11 <- lm(Nor~Train.Normalized$curbWeight,Train.Normalized)
M12 <- lm(Nor~Train.Normalized$price,Train.Normalized)
M2 <- lm(Nor~Train.Normalized$curbWeight+Train.Normalized$price,Train.Normalized)
anova(M0,M11,M2)
anova(M0,M12,M2)


M0 <- lm(Nor~1,Train.Normalized)
M11 <- lm(Nor~Train.Normalized$highwayMpg,Train.Normalized)
M12 <- lm(Nor~Train.Normalized$horsepower,Train.Normalized)
M2 <- lm(Nor~Train.Normalized$highwayMpg+Train.Normalized$horsepower,Train.Normalized)
anova(M0,M11,M2)
anova(M0,M12,M2)
#on garde les deux



#ici je voulais prédire les valeurs manquantes de stroke donc je vais contruire un jeu de données sans valeurs manquantes
#stroke1 <-subset(Train.Normalized,!(is.na(Train.Normalized$stroke)))

#regarder la corrélation entre les variables quantitatives
image(cor(Train.Normalized[, !qualitatives]))
heatmap(abs(cor(Train.Normalized[, !qualitatives])))

#ggscatmat(Train.Normalized)

#Ms <- lm(formula = stroke1$stroke ~ .,data=stroke1)


library(MASS)

#Modelisation de Nor en fonction de toutes les autres variables

set.seed(12)

#la variable engineLocation ne contient qu'une seule valeur ,alors on l'élimine car elle est plus utile

null <- lm(Nor~1, data=Train.Normalized [, -9])
full <- lm(Nor~symboling+make+fuelType+aspiration+numOfDoors+bodyStyle+driveWheels+wheelBase+length+width+height+curbWeight+engineType+numOfCylinders+engineSize+fuelSystem+bore+stroke+compressionRatio+horsepower+peakRpm+cityMpg+highwayMpg+price , data=Train.Normalized )

anova(null,full)
summary(full)

M<-lm(Nor~ make+driveWheels+height+curbWeight+horsepower,Train.Normalized[,-9])
summary(M) #74% des pertes sont expliqués par make et drivewheels ,height,curbWeight+horsepower
M1<-lm(Nor~ make+driveWheels+height,Train.Normalized[,-9])
summary(M1) # p-value = 6.03e-16 :on rejette l'hypothése que alpha=0

#ici on remarque que 74% des pertes sont aussi expliquées mais avec moins de variables , donc M1 est meilleur que M
#Residual standard error: 19.28 on 143 degrees of freedom : elle nous donne une idée de la variance entre la valeur observée de Normalized et celle à prédire : e= y-y^


abline(M1)
summary(M1)$adj.r.squared #  0.7041954
AIC(M1) # 1457.451
BIC(M1) # 1525.648
#Pour rendre l'équation utile à des fins de prévision, il est souhaitable que le modèle contienne un maximum devariables explicatives afin d'en augmenter le pouvoir explicatif.
#Les méthodes de type pas à pas consistent à considérer d'abord un modèle faisant intervenir un certain nombre de variables explicatives. Puis nous procédons par élimination ou ajout successif de variables.

#la régression stepwise

model.AIC <- step(lm(Nor~.,Train.Normalized[, -9,-2]), k=2)
#le modele retenu est : 
#Nor ~ normalized + make + aspiration + driveWheels + wheelBase + width + height + numOfCylinders + engineSize + compressionRatio + horsepower
#p-value<5% : le modele est globalement significatif
summary(model.AIC)$adj.r.squared # 1
AIC(model.AIC) # -10134.3
BIC(model.AIC) # -10028.9

model.BIC <- step(lm(Nor~.,Train.Normalized[, -c(2,9)]), k=2)
# Nor ~ normalized + make + aspiration + driveWheels + wheelBase + width + height + numOfCylinders + engineSize + compressionRatio + horsepower

summary(model.BIC)$adj.r.squared # 1
AIC(model.BIC) # -10134.3
BIC(model.BIC) # -10028.9

#comparaison suivant AIC: on choisit le plus petit donc model.AIC et model.BIC sont meilleurs que M1
#suivant le critére BIC : meme chose
# SUivant r² ajusté : on choisit celui qui posséde la plus grande valeur : même comparaison

par(mfrow=c(2,3))
plot(model.AIC, which=1:3) # d'aprés le graphe de residuals ou erreurs on peut remarquer la relation entre les pertes et les variables prédicateurs est appromaximativement linéaire, la variation paraît constante
plot(model.BIC, which=1:3)
#kifech na9rahom ???

#Plusieurs types de procédures de sélection de variables telque la recherche exhaustive
#REx <- regsubsets(Nor ~ symboling+make+fuelType+aspiration+driveWheels+wheelBase+length+width+height+curbWeight+engineType+numOfCylinders+engineSize+fuelSystem+compressionRatio+horsepower+peakRpm+cityMpg+highwayMpg , data=Train.Normalized, nvmax=10, nbest=500, really.big=TRUE)



#ridge ,lasso

#L'autre stratégie qui cherche à conserver l'ensemble ou tout du moins la plupart des variables explicatives pose un problème de multicolinéarité. Il est résolu par une procédure de régularisation.
#La représentation du chemin de  la solution peut se faire  en fonction de diverses mesures du niveau de régularisation

#division de la dataset 

n <- nrow(Train.Normalized) 
app <- sample(1:n, n / 4)
Train <- Train.Normalized[ -app, ]
test <- Train.Normalized[app, ]
test<-test[,-c(15)]
plot(Train$engineLocation)
par(mfrow=c(1,2))
plot(test$engineType)
plot(Train$engineType)
#Train$engineLocation<- NULL
library(glmnet)
set.seed(19875)

x <- model.matrix(~.,data=Train[,-2])
y <-Train$normalized
ridge <-glmnet(x,y,alpha=0)
par(mfrow=c(2,3))
plot(ridge, xvar="lambda")
plot(ridge, xvar="norm")
plot(ridge, xvar="dev")
#Nous voyons la régularisation pénalisée de Ridge, qui permet de diminuer les coefficients des variables jusqu'à ce qu'ils deviennent nuls, et notre objectif sera de fixer un certain lambda qui nous permet de prendre les coefficients de chaque variable suivant son importance dans le modèle.
#Nous procédons alors à une validation croisée afin de fixer lambda
#La Figure 3.1 expose la cross validation par les deux methodes (Lasso et Ridge) et dans la Figure 3.2, on peut regarder les valeurs de lambda qui minimise les criteres BIC et nBIC.

ridge.10cv <- cv.glmnet(x,y,nfolds=10, alpha=0, grouped=FALSE)
ridge.loo <- cv.glmnet(x,y,nfolds=n , alpha=0, grouped=FALSE)
par(mfrow=c(2,2))
plot(ridge.10cv)
plot(ridge.loo)
#
x0 <- model.matrix(~., data=Train[1:5, -2]) # une nouvelle observation pour l'exemple
predict(ridge, newx=x0, s=ridge.10cv$lambda.min)

predict(ridge, newx=x0, s=ridge.10cv$lambda.1se)


#lasso
lasso <- glmnet(x,y)
par(mfrow=c(1,3))
plot(lasso, xvar="lambda")
plot(lasso, xvar="norm")
plot(lasso, xvar="dev")

lasso.10cv <- cv.glmnet(x,y,nfolds=10, grouped=FALSE)
lasso.loo <- cv.glmnet(x,y,nfolds=n , grouped=FALSE)
par(mfrow=c(1,2))
plot(lasso.10cv)
plot(lasso.loo)

predict(lasso, x0, s=lasso.10cv$lambda.min)
predict(lasso, x0, s=lasso.10cv$lambda.1se)

#comparaison
null <- lm(normalized~1, Train)
full <- lm(normalized~symboling+fuelType+engineLocation+aspiration+numOfDoors+bodyStyle+driveWheels+wheelBase+length+width+height+curbWeight+engineSize+bore+stroke+compressionRatio+horsepower+peakRpm+cityMpg+highwayMpg+price , data=Train)

step.AIC <- step(lm(Train$normalized~.,Train[,-c(2,15)]), k=2)
step.BIC <- step(lm(Train$normalized~.,Train[, -c(2,15)]), k=log(nrow(Train)))

y.test <- test$normalized
x.test <- model.matrix(~.,data=test[,-c(2)])
#test<-test[,-c(2)]


err.null <- mean((y.test - predict(null, test))^2)
err.full <- mean((y.test - predict(full, test))^2)
plot(test$engineType)
err.sAIC <- mean((y.test - predict(step.AIC, test))^2)
err.sBIC <- mean((y.test - predict(step.BIC, test))^2)
s<-predict(ridge, x.test, s=ridge.10cv$lambda.1se)

n <- nrow(Train)
p <- ncol(Train) - 1 + 1
AIC <- n*log(colMeans((y - predict(lasso, x))^2)) + 2 * lasso$df
BIC <- n*log(colMeans((y - predict(lasso, x))^2)) + log(n) * lasso$df
eBIC <- n*log(colMeans((y - predict(lasso, x))^2)) + log(p) * lasso$df
mBIC <- n*log(colMeans((y - predict(lasso, x))^2)) + (log(n) + 2 *log(p)) * lasso$df
lambda.min.BIC <- lasso$lambda[which.min(BIC)]
lambda.min.mBIC <- lasso$lambda[which.min(mBIC)]

err.ridge.min <- mean((y.test - predict(ridge, newx=x.test, s=ridge.10cv$lambda.min))^2)
err.ridge.1se <- mean((y.test - predict(ridge, newx=x.test, s=ridge.10cv$lambda.1se))^2)
err.lasso.min <- mean((y.test - predict(lasso, newx=x.test, s=lasso.10cv$lambda.min))^2)
err.lasso.1se <- mean((y.test - predict(lasso, newx=x.test, s=lasso.10cv$lambda.1se))^2)
err.lasso.BIC <- mean((y.test - predict(lasso, newx=x.test, s=lambda.min.BIC))^2)
err.lasso.mBIC <- mean((y.test - predict(lasso, newx=x.test, s=lambda.min.mBIC))^2)
res <- data.frame(modele = c("null", "full", "step.AIC", "step.BIC", "ridge.CVmin",
                             "ridge.CV1se", "lasso.CVmin", "lasso.CV1se", "lasso.BIC", "lasso.mBIC"),
                  erreur = c(err.null, err.full, err.sAIC, err.sBIC, err.ridge.min, err.ridge.1se,
                             err.lasso.min, err.lasso.1se, err.lasso.BIC, err.lasso.mBIC))
print(res)
dim(coef(ridge.10cv))
#En comparant les erreurs de chaque modèle, nous avons constaté que le modéle ridge est meilleur car il posséde l'erreur la plus petite


predict(ridge, newx=x.test, s=ridge.10cv$lambda.1se)

#regression polynomiale 
qualitatives <- sapply(Train.Normalized, is.factor)
#jeu de données pour les var numériques
quantitatives <-Train.Normalized[!qualitatives]
quan <-Train.Normalized[!qualitatives]

#jeu de données pour les var qualit
qual<-Train.Normalized[qualitatives]

#Nouvelle DATASET
#quantitatives<-quantitatives[,-c(16,17)]

quantitatives<-cbind(quantitatives,quantitatives$wheelBase^2)
quantitatives<-cbind(quantitatives,quantitatives$wheelBase^3)
quantitatives<-cbind(quantitatives,quantitatives$length^2)
quantitatives<-cbind(quantitatives,quantitatives$length^3)
quantitatives<-cbind(quantitatives,quantitatives$width^2)
quantitatives<-cbind(quantitatives,quantitatives$width^3)
quantitatives<-cbind(quantitatives,quantitatives$height^2)
quantitatives<-cbind(quantitatives,quantitatives$height^3)
quantitatives<-cbind(quantitatives,quantitatives$curbWeight^2)
quantitatives<-cbind(quantitatives,quantitatives$curbWeight^3)
quantitatives<-cbind(quantitatives,quantitatives$engineSize^2)
quantitatives<-cbind(quantitatives,quantitatives$engineSize^3)
quantitatives<-cbind(quantitatives,quantitatives$bore^2)
quantitatives<-cbind(quantitatives,quantitatives$bore^3)

quantitatives<-cbind(quantitatives,quantitatives$stroke^2)
quantitatives<-cbind(quantitatives,quantitatives$stroke^3)
quantitatives<-cbind(quantitatives,quantitatives$compressionRatio^2)
quantitatives<-cbind(quantitatives,quantitatives$compressionRatio^3)
quantitatives<-cbind(quantitatives,quantitatives$horsepower^2)
quantitatives<-cbind(quantitatives,quantitatives$horsepower^3)
quantitatives<-cbind(quantitatives,quantitatives$peakRpm^2)
quantitatives<-cbind(quantitatives,quantitatives$peakRpm^3)

quantitatives<-cbind(quantitatives,quantitatives$cityMpg^2)
quantitatives<-cbind(quantitatives,quantitatives$cityMpg^3)
quantitatives<-cbind(quantitatives,quantitatives$highwayMpg^2)
quantitatives<-cbind(quantitatives,quantitatives$highwayMpg^3)
quantitatives<-cbind(quantitatives,quantitatives$price^2)
quantitatives<-cbind(quantitatives,quantitatives$price^3)



colnames(quantitatives)[colnames(quantitatives)=="quantitatives$engineSize^2"] <- "engineSize2"
colnames(quantitatives)[colnames(quantitatives)=="quantitatives$engineSize^3"] <- "engineSize3"
colnames(quantitatives)[colnames(quantitatives)=="quantitatives$wheelBase^2"] <- "wheelBase2"
colnames(quantitatives)[colnames(quantitatives)=="quantitatives$wheelBase^3"] <- "wheelBase3"
colnames(quantitatives)[colnames(quantitatives)=="quantitatives$length^2"] <- "length2"
colnames(quantitatives)[colnames(quantitatives)=="quantitatives$length^3"] <- "length3"
colnames(quantitatives)[colnames(quantitatives)=="quantitatives$width^2"] <- "width2"
colnames(quantitatives)[colnames(quantitatives)=="quantitatives$width^3"] <- "width3"
colnames(quantitatives)[colnames(quantitatives)=="quantitatives$height^2"] <- "height2"
colnames(quantitatives)[colnames(quantitatives)=="quantitatives$height^3"] <- "height3"
colnames(quantitatives)[colnames(quantitatives)=="quantitatives$curbWeight^2"] <- "curbWeight2"
colnames(quantitatives)[colnames(quantitatives)=="quantitatives$curbWeight^3"] <- "curbWeight3"
colnames(quantitatives)[colnames(quantitatives)=="quantitatives$bore^2"] <- "bore2"
colnames(quantitatives)[colnames(quantitatives)=="quantitatives$bore^3"] <- "bore3"
colnames(quantitatives)[colnames(quantitatives)=="quantitatives$stroke^2"] <- "stroke2"
colnames(quantitatives)[colnames(quantitatives)=="quantitatives$stroke^3"] <- "stroke3"
colnames(quantitatives)[colnames(quantitatives)=="quantitatives$compressionRatio^2"] <- "compressionRatio2"
colnames(quantitatives)[colnames(quantitatives)=="quantitatives$compressionRatio^3"] <- "compressionRatio3"
colnames(quantitatives)[colnames(quantitatives)=="quantitatives$horsepower^2"] <- "horsepower2"
colnames(quantitatives)[colnames(quantitatives)=="quantitatives$horsepower^3"] <- "horsepower3"
colnames(quantitatives)[colnames(quantitatives)=="quantitatives$peakRpm^2"] <- "peakRpm2"
colnames(quantitatives)[colnames(quantitatives)=="quantitatives$peakRpm^3"] <- "peakRpm3"
colnames(quantitatives)[colnames(quantitatives)=="quantitatives$cityMpg^2"] <- "cityMpg2"
colnames(quantitatives)[colnames(quantitatives)=="quantitatives$cityMpg^3"] <- "cityMpg3"
colnames(quantitatives)[colnames(quantitatives)=="quantitatives$highwayMpg^2"] <- "highwayMpg2"
colnames(quantitatives)[colnames(quantitatives)=="quantitatives$highwayMpg^3"] <- "highwayMpg3"
colnames(quantitatives)[colnames(quantitatives)=="quantitatives$price^2"] <- "price2"
colnames(quantitatives)[colnames(quantitatives)=="quantitatives$price^3"] <- "price3"


TrainPoly <- quantitatives[ 1:151,]
testPoly <- quantitatives[152:205, ]

Train3 <- quan[ 1:151,]
Test3<- quan[152:205, ]

null2 <- lm(normalized~1, TrainPoly)
full2 <- lm(normalized~.-1 , data=TrainPoly)
full<-lm(normalized~.,data=Train[!qualitatives])
#full3 <- lm(normalized~wheelBase+length+width+height+curbWeight+engineSize+bore+stroke+compressionRatio+horsepower+peakRpm+cityMpg+highwayMpg+price+wheelBase2+length2+width2+height2+curbWeight2+engineSize2+bore2+stroke2+compressionRatio2+horsepower2+peakRpm2+cityMpg2+highwayMpg2+price2 , data=TrainPoly )
full3<-lm(normalized~.,data=Train3)
summary(full3)
summary(full2)
full4<-lm(normalized~peakRpm+cityMpg+horsepower+horsepower2+horsepower3+price+price2+price3+peakRpm2+cityMpg2+cityMpg3+highwayMpg+highwayMpg2+highwayMpg3,data=TrainPoly)
summary(full4)
#la regression polynomiale a diminué l'erreur et a bien amélioré la r² ajusté

#on remarque que les var significatives selon p-value sont horspower et compressionration ,horspower^2 et compressionration^2 
anova(full3,full2)

plot(TrainPoly$normalized~TrainPoly$cityMpg,main="Représentation des pertes moyennes en fonction du prix",las=1)
lines(lowess(TrainPoly$normalized~TrainPoly$cityMpg),col="blue")
mm<-lm(normalized~cityMpg,data=TrainPoly)
mm1<-lm(normalized~cityMpg2,data=TrainPoly)
mm2<-lm(normalized~cityMpg+cityMpg2,data=TrainPoly)
x<-predict(mm1)
abline(TrainPoly$normalized~x)
lines(smooth.spline(TrainPoly$cityMpg,predict(mm1)),col="green",lwd=3)

mcor <- cor(quan)
mcor



#Nous avons essayé de comparer la variable à prédire qui représente les pertes moyennes en fonction de la hauteur d'une voiture puis les pertes moyennes en fonction de la hauteur au carré, et ça nous a donné la même représentation du nuage de points
par(mfrow=c(1,2))
plot(TrainPoly$normalized~TrainPoly$height)
lines(lowess(TrainPoly$normalized~TrainPoly$height),col="red")
plot(TrainPoly$normalized~TrainPoly$height2)
lines(lowess(TrainPoly$normalized~TrainPoly$height2))

#Model3<- lm(normalized~  compressionRatio+peakRpm+peakRpm2+peakRpm3+price+price2+price3+horsepower+horsepower2+horsepower3+cityMpg2+cityMpg3, data=quantitatives)
#summary(Model3)
#n<-nrow(TrainPoly)
#model.AIC <- step(lm(TrainPoly$normalized~.,TrainPoly[,-c(1)]))
#le modele retenu est : 

# summary(model.AIC)$adj.r.squared # 0.357
# AIC(model.AIC) # -10134.3
# BIC(model.AIC) # -10028.9
# 
# model.BIC <- step(lm(TrainPoly$normalized~.,TrainPoly[, -c(1)]),k=log(n), trace=FALSE)
# Nor ~ normalized + make + aspiration + driveWheels + wheelBase + width + height + numOfCylinders + engineSize + compressionRatio + horsepower

#summary(model.BIC)$adj.r.squared #0.238

x <- model.matrix(~.,data=TrainPoly[,-1])
y <-TrainPoly$normalized
#ridge
ridge <-glmnet(x,y,alpha=0)
par(mfrow=c(2,3))
plot(ridge, xvar="lambda")
plot(ridge, xvar="norm")
plot(ridge, xvar="dev")

#lasso
lasso <- glmnet(x,y)
par(mfrow=c(1,3))
plot(lasso, xvar="lambda")
plot(lasso, xvar="norm")
plot(lasso, xvar="dev")

lasso.10cv <- cv.glmnet(x,y,nfolds=10, grouped=FALSE)
lasso.loo <- cv.glmnet(x,y,nfolds=n , grouped=FALSE)
par(mfrow=c(1,2))
plot(lasso.10cv)
plot(lasso.loo)

step.AIC <- step(lm(TrainPoly$normalized~.,TrainPoly[,-c(1)]), k=2,direction ="backward")
summary(model.AIC)$adj.r.squared 
step.BIC <- step(lm(TrainPoly$normalized~.,TrainPoly[, -c(1)]), k=log(nrow(TrainPoly)))
summary(model.BIC)$adj.r.squared 
AIC(step.AIC)
AIC(step.BIC)
par(mfrow=c(2,2))
plot(step.AIC)
plot(step.BIC)
y.test <- testPoly$normalized
x.test <- model.matrix(~.,data=testPoly[,-c(1)])
#test<-test[,-c(2)]


err.null <- mean((y.test - predict(null2, testPoly))^2)
err.full <- mean((y.test - predict(full2, testPoly))^2)

err.sAIC <- mean((y.test - predict(step.AIC, testPoly))^2)
err.sBIC <- mean((y.test - predict(step.BIC, testPoly))^2)
s<-predict(ridge, x.test, s=ridge.10cv$lambda.1se)

n <- nrow(TrainPoly)
p <- ncol(TrainPoly) - 1 + 1
AIC <- n*log(colMeans((y - predict(lasso, x))^2)) + 2 * lasso$df
BIC <- n*log(colMeans((y - predict(lasso, x))^2)) + log(n) * lasso$df
eBIC <- n*log(colMeans((y - predict(lasso, x))^2)) + log(p) * lasso$df
mBIC <- n*log(colMeans((y - predict(lasso, x))^2)) + (log(n) + 2 *log(p)) * lasso$df
lambda.min.BIC <- lasso$lambda[which.min(BIC)]
lambda.min.mBIC <- lasso$lambda[which.min(mBIC)]
#ici la plus petite lamda utilisé pour le modele BIC et mBIC sont
# lambda.min.BIC
# [1] 2.897922
# > lambda.min.mBIC
# [1] 8.06364
#On va les utiliser pour voir l'ecart de la moyenne de l'erreur
err.ridge.min <- mean((y.test - predict(ridge, newx=x.test, s=ridge.10cv$lambda.min))^2)
err.ridge.1se <- mean((y.test - predict(ridge, newx=x.test, s=ridge.10cv$lambda.1se))^2)
err.lasso.min <- mean((y.test - predict(lasso, newx=x.test, s=lasso.10cv$lambda.min))^2)
err.lasso.1se <- mean((y.test - predict(lasso, newx=x.test, s=lasso.10cv$lambda.1se))^2)
err.lasso.BIC <- mean((y.test - predict(lasso, newx=x.test, s=lambda.min.BIC))^2)
err.lasso.mBIC <- mean((y.test - predict(lasso, newx=x.test, s=lambda.min.mBIC))^2)
res <- data.frame(modele = c("null", "full", "step.AIC", "step.BIC", "ridge.CVmin",
                             "ridge.CV1se", "lasso.CVmin", "lasso.CV1se", "lasso.BIC", "lasso.mBIC"),
                  erreur = c(err.null, err.full, err.sAIC, err.sBIC, err.ridge.min, err.ridge.1se,
                             err.lasso.min, err.lasso.1se, err.lasso.BIC, err.lasso.mBIC))
print(res)
AIC(ridge) # 0.357
predict(ridge, newx=x.test, s=ridge.10cv$lambda.min)

#essayons avec un dataset qui contient que les quantitatives
set.seed(123)
app <- sample(1:205, 205 / 4)
Train3 <- quan[ -app, ]
Test3 <- quan[app, ]

null3 <- lm(normalized~1, Train3)
full3 <- lm(normalized~.-1 , data=Train3)
anova(full3,full2)
m<-lm(normalized~horsepower+compressionRatio+height+price,TrainPoly)
summary(m)
m1<-lm(normalized~horsepower+horsepower2+compressionRatio+compressionRatio2+height+height2+price+price2,TrainPoly)
summary(m1)
anova(m,m1)


#polynomiale (correction)

L=c()
for(i in colnames(Test))
{
  L=c(L,sum((is.na(Test))))
}
L
TrPoly <- Train.Normalized[1:130,]
Test <- Train.Normalized[131:164,]
y.test <- Test$normalized
y.train<-TrainPoly$normalized
x.test <- model.matrix(~.,data=testPoly[,-c(1)])

null <- lm(normalized~1, TrPoly)
full <- lm(normalized~fuelType+aspiration+numOfDoors+bodyStyle+wheelBase+length+width+height+curbWeight+numOfCylinders+engineSize+fuelSystem+bore+stroke+compressionRatio+horsepower+peakRpm+cityMpg+highwayMpg+price , data=TrPoly)


err.null <- mean((y.test - predict(null, Test))^2)
err.full <- mean((y.test - predict(full, Test[,-c(1,3)]))^2)

err.null <- mean((y.train - predict(null, TrPoly))^2)
err.full <- mean((y.train - predict(full, TrPoly[,-c(1,3)]))^2)
set.seed(123)
full <- lm(normalized~fuelType+aspiration+numOfDoors+bodyStyle+wheelBase+length+height+I(height^2)+width+curbWeight+numOfCylinders+engineSize+fuelSystem+bore+stroke+compressionRatio+horsepower+peakRpm+cityMpg+highwayMpg+price , data=TrPoly)


err.null <- mean((y.test - predict(null, Test))^2)
err.full <- mean((y.test - predict(full, Test[,-c(1,3)]))^2)

err.null2 <- mean((y.train - predict(null, TrPoly))^2)
err.full2 <- mean((y.train - predict(full, TrPoly[,-c(1,3)]))^2)



full <- lm(normalized~fuelType+aspiration+numOfDoors+bodyStyle+wheelBase+length+height+I(height^2)+I(height^3)+width+curbWeight+numOfCylinders+engineSize+fuelSystem+bore+stroke+compressionRatio+horsepower+peakRpm+cityMpg+highwayMpg+price , data=TrPoly)

err.full <- mean((y.test - predict(full, Test[,-c(1,3)]))^2)
summary(full)
err.null <- mean((y.test - predict(null, Test))^2)
err.full <- mean((y.test - predict(full, Test[,-c(1,3)]))^2)

err.null2 <- mean((y.train - predict(null, TrPoly))^2)
err.full2 <- mean((y.train - predict(full, TrPoly[,-c(1,3)]))^2)


x.test <- model.matrix(~fuelType+aspiration+numOfDoors+bodyStyle+wheelBase+length+height+width+curbWeight+numOfCylinders+engineSize+fuelSystem+bore+stroke+compressionRatio+horsepower+peakRpm+cityMpg+highwayMpg+price,data=TrPoly[,-2])
y.test<-TrPoly$normalized
#ridge
ridge <-glmnet(x.test,y.test,alpha=0)
par(mfrow=c(2,3))
plot(ridge, xvar="lambda")
plot(ridge, xvar="norm")
plot(ridge, xvar="dev")

ridge.10cv <- cv.glmnet(x.test,y.test,nfolds=10, alpha=0, grouped=FALSE)
ridge.loo <- cv.glmnet(x.test,y.test,nfolds=nrow(TrPoly) , alpha=0, grouped=FALSE)
#lasso
lasso <- glmnet(x.test,y.test)
par(mfrow=c(1,3))
plot(lasso, xvar="lambda")
plot(lasso, xvar="norm")
plot(lasso, xvar="dev")

lasso.10cv <- cv.glmnet(x.test,y.test,nfolds=10, grouped=FALSE)
lasso.loo <- cv.glmnet(x.test,y.test,nfolds=nrow(TrPoly) , grouped=FALSE)
par(mfrow=c(1,2))
plot(lasso.10cv)
plot(lasso.loo)
n<-nrow(TrPoly)
p <- ncol(TrPoly) - 1 + 1
AIC <- n*log(colMeans((y.test - predict(lasso, x.test))^2)) + 2 * lasso$df
BIC <- n*log(colMeans((y.test - predict(lasso, x.test))^2)) + log(n) * lasso$df
eBIC <- n*log(colMeans((y.test - predict(lasso, x.test))^2)) + log(p) * lasso$df
mBIC <- n*log(colMeans((y.test - predict(lasso, x.test))^2)) + (log(n) + 2 *log(p)) * lasso$df
lambda.min.BIC <- lasso$lambda[which.min(BIC)]
lambda.min.mBIC <- lasso$lambda[which.min(mBIC)]

err.ridge.min <- mean((y.test - predict(ridge, newx=x.test, s=ridge.10cv$lambda.min))^2)
err.ridge.1se <- mean((y.test - predict(ridge, newx=x.test, s=ridge.10cv$lambda.1se))^2)
err.lasso.min <- mean((y.test - predict(lasso, newx=x.test, s=lasso.10cv$lambda.min))^2)
err.lasso.1se <- mean((y.test - predict(lasso, newx=x.test, s=lasso.10cv$lambda.1se))^2)
err.lasso.BIC <- mean((y.test - predict(lasso, newx=x.test, s=lambda.min.BIC))^2)
err.lasso.mBIC <- mean((y.test - predict(lasso, newx=x.test, s=lambda.min.mBIC))^2)

par(mfrow=c(2,2))
plot(ridge.10cv)
plot(ridge.loo)
#
x0 <- model.matrix(~., data=Train[1:5, -2]) # une nouvelle observation pour l'exemple
predict(ridge, newx=x0, s=ridge.10cv$lambda.min)

predict(ridge, newx=x0, s=ridge.10cv$lambda.1se)

#saraa dghdk
##test branche for sara
##test branche sara 