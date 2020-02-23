# INST627 Project
# Team: Magnificent

#load the data 
water <- read.csv(file.choose())

install.packages("summarytools")
library(summarytools)

#number of variables and observations
dim(water)
str(water)

#descriptive statistics
summary(water)

#install Rcmdr package
install.packages("car")
library(car)

#plot for variables
par(mar = rep(1, 4))
plot(water$What.is.your.preferred.method.of.drinking.water., col="green", main="Preferred method of drinking water")
plot(water$Which.age.group.do.you.belong.to., col="green", main="Age group")
plot(water$Did.you.ever.fall.sick.because.of.drinking.tap.water., col="green", main="Falling sick because of drinking tap water")
par(mar=c(11, 4.1,4.1, 2.1))
plot(water$In.which.county.of.Maryland.do.you.reside., col="green", las=2, main="County")
mosaicplot(table(water$In.which.county.of.Maryland.do.you.reside., water$What.is.your.preferred.method.of.drinking.water), col = rainbow(4), las=2,  main = "Water preference vs county")
mosaicplot(table(water$Which.age.group.do.you.belong.to., water$What.is.your.preferred.method.of.drinking.water), col = rainbow(4), las=4,  main = "Water preference vs Age group")
mosaicplot(table(water$Did.you.ever.fall.sick.because.of.drinking.tap.water., water$What.is.your.preferred.method.of.drinking.water), col = rainbow(4), las=4,  main = "Water preference vs falling sick")
mosaicplot(table(water$How.many.glasses.bottles.of.water.do.you.drink.per.day.on.average...Consider.a.scale.of.16oz.glass.bottle., water$What.is.your.preferred.method.of.drinking.water.), col = rainbow(4), las=4,  main = "Water preference vs consumption of water in glasses")
hist(water$How.would.you.rate.your.trust.in.all.brands.of.bottled.water.to.be.contaminate.free...Rate.from.1.to.5..with.5.being.trusting.completely., col="green", main="Distribution of trust in bottled water rating", xlab="Trust in bottled water rating")
hist(water$How.would.you.rate.your.trust.in.the.tap.water.at.your.house.to.be.contaminate.free..Rate.from.1.to.5..with.5.being.trusting.completely., col="green", main = "Distribution of trust in tap water rating", xlab="Trust in tap water rating")
hist(water$How.would.you.rate.the.taste.of.bottled.water..Rate.from.1.to.5..with.5.being.the.highest., col="green", main="Distribution of taste of bottled water rating", xlab="Taste of bottled water rating")
hist(water$How.would.you.rate.the.taste.of.your.tap.water...Rate.from.1.to.5..with.5.being.the.highest., col="green", main=" Distribution of taste of tap water rating", xlab="Taste of tap water rating")

#recode the preferred method of drinking water variable
levels(water$What.is.your.preferred.method.of.drinking.water.)
water$water_pref <- recode(as.numeric(water$What.is.your.preferred.method.of.drinking.water.), "1=1; 2=0")
table(water$What.is.your.preferred.method.of.drinking.water., water$water_pref)

#recode the county variable - arundel county
water$arundel <- recode(as.numeric(water$In.which.county.of.Maryland.do.you.reside.), "1=1; 2:14=0")
table(water$In.which.county.of.Maryland.do.you.reside., water$arundel)
water$county_ar <-as.numeric(as.character(water$arundel))

arundel_model <- lm(water$water_pref ~ water$county_ar)
summary(arundel_model)

#recode the county variable -baltimore_city county
water$baltimore_city <- recode(as.numeric(water$In.which.county.of.Maryland.do.you.reside.), "2=1; 1=0; 3:14=0")
table(water$In.which.county.of.Maryland.do.you.reside., water$baltimore_city)
water$city_bl <-as.numeric(as.character(water$baltimore_city))

baltimore_model <- lm(water$water_pref ~ water$city_bl)
summary(baltimore_model)

#code the county variable -baltimore county
water$baltimore <- recode(as.numeric(water$In.which.county.of.Maryland.do.you.reside.), "3=1; 1:2=0; 4:14=0")
table(water$In.which.county.of.Maryland.do.you.reside., water$baltimore)
water$county_bl <-as.numeric(as.character(water$baltimore))

baltimore_county_model <- lm(water$water_pref ~ water$county_bl)
summary(baltimore_county_model)

#recode the county variable -harward county
water$harward <- recode(as.numeric(water$In.which.county.of.Maryland.do.you.reside.), "9=1; 1:8=0; 10:14=0")
table(water$In.which.county.of.Maryland.do.you.reside., water$harward)
water$county_harward <-as.numeric(as.character(water$harward))

harward_model <- lm(water$water_pref ~ water$county_harward)
summary(harward_model)

#recode the county variable -Montgomery county
water$Montgomery <- recode(as.numeric(water$In.which.county.of.Maryland.do.you.reside.), "11=1; 1:10=0; 11:14=0")
table(water$In.which.county.of.Maryland.do.you.reside., water$Montgomery)
water$county_Montgomery <-as.numeric(as.character(water$Montgomery))

Montgomery_model <- lm(water$water_pref ~ water$county_Montgomery)
summary(Montgomery_model)

#recode the county variable -Prince George's County 
water$pg<- recode(as.numeric(water$In.which.county.of.Maryland.do.you.reside.), "12=1; 1:11=0; 13:14=0")
table(water$In.which.county.of.Maryland.do.you.reside., water$pg)
water$county_pg <-as.numeric(as.character(water$pg))

pg_model <- lm(water$water_pref ~ water$county_pg)
summary(pg_model)

#recode the 18-24 age-group variable 
levels(water$Which.age.group.do.you.belong.to.)
water$age24 <- recode(as.numeric(water$Which.age.group.do.you.belong.to.), "1=1; 2:7=0")
table(water$Which.age.group.do.you.belong.to., water$age24)
water$age_1824 <-as.numeric(as.character(water$age24))

age_model <- lm(water$water_pref ~ water$age_1824)
summary(age_model)

#recode the 25-34 age-group variable 
water$age25 <- recode(as.numeric(water$Which.age.group.do.you.belong.to.), "1=0; 2=1; 3:7=0")
table(water$Which.age.group.do.you.belong.to., water$age25)
water$age_2534 <-as.numeric(as.character(water$age25))

age_model2 <- lm(water$water_pref ~ water$age_2534)
summary(age_model2)

#recode the how many glasses/bottles of water do you drink
levels(water$How.many.glasses.bottles.of.water.do.you.drink.per.day.on.average...Consider.a.scale.of.16oz.glass.bottle.)
water$glasses <- recode(as.numeric(water$How.many.glasses.bottles.of.water.do.you.drink.per.day.on.average...Consider.a.scale.of.16oz.glass.bottle.), "11=11;")
table(water$How.many.glasses.bottles.of.non.water.beverages.do.you.drink.per.day.on.average...Consider.a.scale.of.16oz.glass.bottle., water$glasses)

glass_model <- lm(water$water_pref ~ water$glasses)
summary(glass_model)

#How would you rate your trust in all brands of bottled water to be contaminate free?-  considering ordinal variable as numerical
water$trust <- as.numeric(water$How.would.you.rate.your.trust.in.all.brands.of.bottled.water.to.be.contaminate.free...Rate.from.1.to.5..with.5.being.trusting.completely.)
table(water$How.many.glasses.bottles.of.non.water.beverages.do.you.drink.per.day.on.average...Consider.a.scale.of.16oz.glass.bottle., water$trust)

trust_model <- lm(water$water_pref ~ water$trust)
summary(trust_model)

#How would you rate your trust in all brands of tap water to be contaminate free?-  considering ordinal variable as numerical
water$tap_trust <- as.numeric(water$How.would.you.rate.your.trust.in.the.tap.water.at.your.house.to.be.contaminate.free..Rate.from.1.to.5..with.5.being.trusting.completely.)
table(water$How.would.you.rate.your.trust.in.the.tap.water.at.your.house.to.be.contaminate.free..Rate.from.1.to.5..with.5.being.trusting.completely., water$tap_trust)

trust_model2 <- lm(water$water_pref ~ water$tap_trust)
summary(trust_model2)

ols_model <- lm(water$water_pref ~water$trust + water$age_2534 + water$county_bl)
summary(ols_model)

#Sickness Age relation
levels(water$Did.you.ever.fall.sick.because.of.drinking.tap.water.)
water$Did.you.ever.fall.sick.because.of.drinking.tap.water.[water$Did.you.ever.fall.sick.because.of.drinking.tap.water.=="I do not know"]<-NA
levels(water$sickwater)
water$Did.you.ever.fall.sick.because.of.drinking.tap.water.<-droplevels(water$Did.you.ever.fall.sick.because.of.drinking.tap.water.)
head(water$sickwater)
water$sickwater<-recode(as.numeric(water$Did.you.ever.fall.sick.because.of.drinking.tap.water.), "1=0; 2=1")
table(water$Did.you.ever.fall.sick.because.of.drinking.tap.water., water$sickwater)

sick_model<-lm(water$water_pref~water$sickwater)
summary(sick_model)

library(pwr)
pwr.f2.test(u=1, v=184, f2=(0.07337/1-0.07337), sig.level = 0.10, power = NULL)
