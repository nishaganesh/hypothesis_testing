# INST627 Project
# Team: Magnificent

#load the data 
water <- read.csv(file.choose())

#number of variables and observations
dim(water)
str(water)

#descriptive statistics
summary(water)

#exploratory analysis
par(mar = rep(2, 4))
plot(water$What.is.your.preferred.method.of.drinking.water., col="green", main="Preferred method of\n drinking water")
plot(water$Which.age.group.do.you.belong.to., col="green", main="Age group")
plot(water$Did.you.ever.fall.sick.because.of.drinking.tap.water., col="green", main="Falling sick because of\n drinking tap water")
hist(water$How.would.you.rate.your.trust.in.all.brands.of.bottled.water.to.be.contaminate.free...Rate.from.1.to.5..with.5.being.trusting.completely., col="green", main="Distribution of trust in\n bottled water rating", xlab="Trust in bottled water rating")
par(mfrow=c(1,1))
legend("topleft", legend=c("5-High Trust", "1- No Trust"),  cex=0.7, bty = "n")

hist(water$How.would.you.rate.your.trust.in.the.tap.water.at.your.house.to.be.contaminate.free..Rate.from.1.to.5..with.5.being.trusting.completely., col="green", main = "Distribution of trust in\n tap water rating", xlab="Trust in tap water rating")
legend("topleft", legend=c("5-High Trust", "1- No Trust"),  cex=0.7, bty = "n")
hist(water$How.would.you.rate.the.taste.of.bottled.water..Rate.from.1.to.5..with.5.being.the.highest., col="green", main="Distribution of taste of bottled water rating", xlab="Taste of bottled water rating")
legend("topleft", legend=c("5-Very good", "1- Very bad"),  cex=0.7, bty = "n")
hist(water$How.would.you.rate.the.taste.of.your.tap.water...Rate.from.1.to.5..with.5.being.the.highest., col="green", main=" Distribution of taste of tap water rating", xlab="Taste of tap water rating")
legend("topleft", legend=c("5-Very good", "1- Very bad"),  cex=0.7, bty = "n")

par(mar=c(11, 4.1, 4.1, 2.1))
plot(water$In.which.county.of.Maryland.do.you.reside., col="green", las=2, main="County")
mosaicplot(table(water$In.which.county.of.Maryland.do.you.reside., water$What.is.your.preferred.method.of.drinking.water), col = rainbow(4), las=2,  main = "Water preference vs county")
mosaicplot(table(water$Which.age.group.do.you.belong.to., water$What.is.your.preferred.method.of.drinking.water), col = rainbow(4), las=4,  main = "Water preference vs Age group")
mosaicplot(table(water$Did.you.ever.fall.sick.because.of.drinking.tap.water., water$What.is.your.preferred.method.of.drinking.water), col = rainbow(4), las=4,  main = "Water preference vs falling sick")
mosaicplot(table(water$How.many.glasses.bottles.of.water.do.you.drink.per.day.on.average...Consider.a.scale.of.16oz.glass.bottle., water$What.is.your.preferred.method.of.drinking.water.), col = rainbow(4), las=4,  main = "Water preference vs consumption of water in glasses")

#install car package
install.packages("car")
library(car)

levels(water$What.is.your.preferred.method.of.drinking.water.)
preference <- factor(water$What.is.your.preferred.method.of.drinking.water.)
table(preference)

factor_county <- factor(water$In.which.county.of.Maryland.do.you.reside.)
table(factor_county)
levels(factor_county)
county<-recode(as.numeric(factor_county), "1=5;2=1;3=2;4:10=5;11=3;12=4;13:14=5")
county<-factor(county, labels = c("Baltimore City","Baltimore County","MoCo", "PG", "Others"))
table(water$In.which.county.of.Maryland.do.you.reside., county)


# Independent variable county 
model1 <- glm(preference ~ county, family=binomial)
summary(model1)
exp(cbind(OR=coef(model1), confint(model1)))


factor_age <- factor(water$Which.age.group.do.you.belong.to.)
table(factor_age)
levels(factor_age)
age <- recode(as.numeric(water$Which.age.group.do.you.belong.to.), "1=2; 2=1; 3=3; 4=4; 5:7=5")
age <-factor(age, labels = c("25-34","18-24","35-44", "45-54", "Others"))
table(water$Which.age.group.do.you.belong.to., age)

# Independent variable: age group 
model2 <- glm(preference ~ age , family=binomial)
summary(model2)
exp(cbind(OR=coef(model2), confint(model2)))


# Independent variables: county, age group
model3 <- glm(preference ~ age + county, family=binomial)
summary(model3)
exp(cbind(OR=coef(model3), confint(model3)))


factor_device <- factor(water$Do.you.use.a.device.at.home.to.filter.your.tap.water.)
table(factor_device)
device <- recode(as.numeric(water$Do.you.use.a.device.at.home.to.filter.your.tap.water.), "1=2; 2=2; 3=1")
device <-factor(device, labels = c("Yes","No and Don't know"))
table(water$Do.you.use.a.device.at.home.to.filter.your.tap.water., device)

# Independent variable: device to filter tap water
model4 <- glm(preference ~ device , family=binomial)
summary(model4)
exp(cbind(OR=coef(model4), confint(model4)))


# Independent variables: county, age group, device to filter tap water
model5 <- glm(preference ~ age + county + device , family=binomial)
summary(model5)
exp(cbind(OR=coef(model5), confint(model5)))


factor_glasses <- factor(water$How.many.glasses.bottles.of.water.do.you.drink.per.day.on.average...Consider.a.scale.of.16oz.glass.bottle.)
table(factor_glasses)
glasses <- recode(as.numeric(water$How.many.glasses.bottles.of.water.do.you.drink.per.day.on.average...Consider.a.scale.of.16oz.glass.bottle.), "11=11")
table(glasses)

# Independent variables: county, age group, device to filter tap water- Yes, number of glasses of water
model6 <- glm(preference ~ age + county + device + glasses , family=binomial)
summary(model6)
exp(cbind(OR=coef(model6), confint(model6)))


factor_sick <- factor(water$Did.you.ever.fall.sick.because.of.drinking.tap.water.)
table(factor_sick)
sick <- recode(as.numeric(water$Did.you.ever.fall.sick.because.of.drinking.tap.water.), "1=2; 2=1; 3=2")
sick <-factor(sick, labels = c("No","Yes and Don't know"))
table(water$Did.you.ever.fall.sick.because.of.drinking.tap.water., sick)

# Independent variables: county, age group, device to filter tap water, number of glasses of water, fall sick
model7 <- glm(preference ~ age + county + device + glasses + sick, family=binomial)
summary(model7)
exp(cbind(OR=coef(model7), confint(model7)))

factor_taste <- factor(water$How.would.you.rate.the.taste.of.your.tap.water...Rate.from.1.to.5..with.5.being.the.highest.)
table(factor_taste)
taste <- as.numeric(water$How.would.you.rate.the.taste.of.your.tap.water...Rate.from.1.to.5..with.5.being.the.highest.)
table(taste)

# Independent variable: taste of tap water
model8 <- glm(preference ~ taste, family=binomial)
summary(model8)
exp(cbind(OR=coef(model8), confint(model8)))

# Independent variables: county, age group, falling sick, taste of tap water
model9 <- glm(preference ~ county + age + sick + taste, family=binomial)
summary(model9)
exp(cbind(OR=coef(model9), confint(model9)))

# Independent variables: county, age group, falling sick, taste of tap water, number of glasses of water
model10 <- glm(preference ~ county + age + sick + glasses + taste , family=binomial)
summary(model10)
exp(cbind(OR=coef(model10), confint(model10)))

factor_trust <- factor(water$How.would.you.rate.your.trust.in.the.tap.water.at.your.house.to.be.contaminate.free..Rate.from.1.to.5..with.5.being.trusting.completely.)
table(factor_trust)
trust <- as.numeric(water$How.would.you.rate.your.trust.in.the.tap.water.at.your.house.to.be.contaminate.free..Rate.from.1.to.5..with.5.being.trusting.completely.)
table(trust)

# Independent variables: county, age group, falling sick, taste of tap water, number of glasses of water, trust in tap water
model11 <- glm(preference ~ county + age + sick + glasses + taste + trust , family=binomial)
summary(model11)
exp(cbind(OR=coef(model11), confint(model11)))

factor_trust_bottled <- factor(water$How.would.you.rate.your.trust.in.all.brands.of.bottled.water.to.be.contaminate.free...Rate.from.1.to.5..with.5.being.trusting.completely.)
table(factor_trust_bottled)
trust_bottled <- as.numeric(water$How.would.you.rate.your.trust.in.all.brands.of.bottled.water.to.be.contaminate.free...Rate.from.1.to.5..with.5.being.trusting.completely.)
table(trust_bottled)

# Independent variables: county, age group, falling sick, taste of tap water, number of glasses of water, trust in tap water, trust in bottled water
model12 <- glm(preference ~ county + age + sick + glasses + taste + trust + trust_bottled , family=binomial)
summary(model12)
exp(cbind(OR=coef(model12), confint(model12)))


taste_bottled <- factor(water$How.would.you.rate.the.taste.of.bottled.water..Rate.from.1.to.5..with.5.being.the.highest.)
table(taste_bottled)
taste_bottled <- as.numeric(water$How.would.you.rate.the.taste.of.bottled.water..Rate.from.1.to.5..with.5.being.the.highest.)
table(taste_bottled)

# Independent variables: county, age group, falling sick, taste of tap water, number of glasses of water, trust in tap water, trust in bottled water
model13 <- glm(preference ~ county + age + sick + glasses + taste + trust + trust_bottled + taste_bottled , family=binomial)
summary(model13)
exp(cbind(OR=coef(model13), confint(model13)))

#checking assumptions- multicollinearity
cor.test(as.numeric(county), as.numeric(age))
cor.test(as.numeric(county), as.numeric(sick))
cor.test(as.numeric(county), as.numeric(glasses))
cor.test(as.numeric(county), as.numeric(trust))
cor.test(as.numeric(county), as.numeric(taste_bottled))
cor.test(as.numeric(county), as.numeric(trust_bottled))
cor.test(as.numeric(county), as.numeric(taste))
cor.test(as.numeric(age), as.numeric(sick))
cor.test(as.numeric(age), as.numeric(trust_bottled))
cor.test(as.numeric(age), as.numeric(glasses))
cor.test(as.numeric(age), as.numeric(trust))
cor.test(as.numeric(glasses), as.numeric(sick))
cor.test(as.numeric(glasses), as.numeric(taste))
cor.test(as.numeric(trust), as.numeric(trust_bottled))
cor.test(as.numeric(trust_bottled), as.numeric(taste))

#regression diagnostic plot
pl <- glm(as.numeric(preference)~as.numeric(age) + as.numeric(age) + as.numeric(sick) + as.numeric(glasses) + as.numeric(taste) + as.numeric(trust) + as.numeric(trust_bottled) + as.numeric(taste_bottled))
par(mfrow= c(2,2))
plot(pl)
par(mfrow= c(1,1))  



