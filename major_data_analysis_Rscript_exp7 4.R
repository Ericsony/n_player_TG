library(stringr)
library(data.table) #apparently faster to work with with big data
library(ggplot2)
library(reshape2) #reshaping
library(corrplot) #corrplot
library(ltm) #rcor.test
library(psych) #pairs.panels
library(MVN) # for multivariate normality checks
library(lattice)
library(car)
library(dplyr)

setwd("/Users/mbp-6_2/Documents/MajorProject/NetLogo/experiments")

data <- read.table("FINAL_n_person_TG_deltaAB experiment7_2-table.csv",
                  header = T,   # set columns names true
                  sep = ',',    # define the separator between columns
                  skip = 6,     # skip first 6 rows 
                  quote = '\"',  # correct the column separator
                  fill = TRUE ) # add blank fields if rows have unequal length

#or this
dfs <- read.csv(file = "data_experiment7_total_short_final.csv")

str(dfs)

data_dt <- as.data.table(data) # data table so I can use the string splitting functions below

data$wealth <- data.frame(data_dt[,tstrsplit(str_remove_all(data$X.wealth..of.turtles,'\\]|\\['),' ')])

data$reputation <- data.frame(data_dt[,tstrsplit(str_remove_all(data$X.reputation..of.turtles,'\\]|\\['),' ')])

#for the bees
data$bees_wealth <- data.frame(data_dt[,tstrsplit(str_remove_all(data$X.wealth..of.bees,'\\]|\\['),' ')])

data$bees_reputation <- data.frame(data_dt[,tstrsplit(str_remove_all(data$X.reputation..of.bees,'\\]|\\['),' ')])

#for the rabbits
data$rabbits_wealth <- data.frame(data_dt[,tstrsplit(str_remove_all(data$X.wealth..of.rabbits,'\\]|\\['),' ')])

data$rabbits_reputation <- data.frame(data_dt[,tstrsplit(str_remove_all(data$X.reputation..of.rabbits,'\\]|\\['),' ')])



str(data)



#let's summarize the agents values into variables we want to use, first wealth: 
for (i in 1:nrow(data$wealth)){
  data$wealth_gini[i] <- DescTools::Gini(data$wealth[i,], na.rm = T)     # one row full of values for all the agents is aggregated into 1 gini coefficient
}

for (i in 1:nrow(data$wealth)){
  data$wealth_average[i] <- mean(as.numeric(data$wealth[i,]), na.rm = T)
}

# now also with reputation

for (i in 1:nrow(data$reputation)){
  data$reputation_gini[i] <- DescTools::Gini(data$reputation[i,], na.rm = T)
}

for (i in 1:nrow(data$reputation)){
  data$reputation_average[i] <- mean(as.numeric(data$reputation[i,]), na.rm = T)
}

#now seperatley for bees and rabbits:

#let's summarize the agents values into variables we want to use, first wealth: 
for (i in 1:nrow(data$wealth)){
  data$bees_wealth_gini[i] <- DescTools::Gini(data$bees_wealth[i,], na.rm = T)     # one row full of values for all the agents is aggregated into 1 gini coefficient
}

for (i in 1:nrow(data$wealth)){
  data$bees_wealth_average[i] <- mean(as.numeric(data$bees_wealth[i,]), na.rm = T)
}

# now also with reputation

for (i in 1:nrow(data$reputation)){
  data$bees_reputation_gini[i] <- DescTools::Gini(data$bees_reputation[i,], na.rm = T)
}

for (i in 1:nrow(data$reputation)){
  data$bees_reputation_average[i] <- mean(as.numeric(data$bees_reputation[i,]), na.rm = T)
}


#now rabbits: 
for (i in 1:nrow(data$wealth)){
  data$rabbits_wealth_gini[i] <- DescTools::Gini(data$rabbits_wealth[i,], na.rm = T)     # one row full of values for all the agents is aggregated into 1 gini coefficient
}

for (i in 1:nrow(data$wealth)){
  data$rabbits_wealth_average[i] <- mean(as.numeric(data$rabbits_wealth[i,]), na.rm = T)
}

# now also with reputation

for (i in 1:nrow(data$reputation)){
  data$rabbits_reputation_gini[i] <- DescTools::Gini(data$rabbits_reputation[i,], na.rm = T)
}

for (i in 1:nrow(data$reputation)){
  data$rabbits_reputation_average[i] <- mean(as.numeric(data$rabbits_reputation[i,]), na.rm = T)
}

names(data)

write.csv(data, file = "data_experiment7new.csv")

unique(data$num.turtles.A)
unique(data$num.turtles.B)
unique(data$delta.A)
unique(data$delta.B)
unique(data$phi)
unique(data$reciprocation.norm.A)
unique(data$reciprocation.norm.B)
unique(data$investment.norm.A)
unique(data$investment.norm.B)

names(data)

identical(data$beta, data$beta.1) # these are the same, will use beta

#let's save the ones we need from now on
all_variables <- c("X.run.number.",
                               "initial.reputation.random.",
                               "num.turtles.A",
                               "num.turtles.B",
                               "reciprocation.norm.A",
                               "reciprocation.norm.B",
                               "beta",
                               "investment.norm.A",
                               "investment.norm.B",
                               "wealth_gini",
                               "wealth_average",
                               "reputation_gini",
                               "reputation_average",
                               "phi",
                               "phi.r",
                               "delta.A",
                               "count.turtles",
                               "bees_wealth_gini",
                               "bees_wealth_average",
                               "bees_reputation_gini",
                               "bees_reputation_average",
                               "rabbits_wealth_gini",
                               "rabbits_wealth_average",
                               "rabbits_reputation_gini",
                               "rabbits_reputation_average")

#and now only what we need for the experiment at hand
variables_of_interest <- names(data[,c("X.run.number.",
                                       "initial.reputation.random.",
                                       "num.turtles.A",
                                       "num.turtles.B", 
                                       "reciprocation.norm.A",
                                       "reciprocation.norm.B",
                                       "beta",
                                       "investment.norm.A",
                                       "investment.norm.B",
                                       "wealth_gini",
                                       "wealth_average",
                                       "reputation_gini",
                                       "reputation_average",
                                       "bees_wealth_gini",
                                       "bees_wealth_average",
                                       "bees_reputation_gini",
                                       "bees_reputation_average",
                                       "rabbits_wealth_gini",
                                       "rabbits_wealth_average",
                                       "rabbits_reputation_gini",
                                       "rabbits_reputation_average")]) 


#choose the final dataframe to work with:

#dfs <- data
dfs <- data[,all_variables]
#dfs <- data[,variables_of_interest]


str(dfs)

categ_vars <- c("X.run.number.", 
                "num.turtles.A", 
                "num.turtles.B",
                "reciprocation.norm.A",
                "reciprocation.norm.B",
                "investment.norm.A",
                "investment.norm.B",
                "initial.reputation.random.",
                "phi",
                "phi.r",
                "delta.A")

numer_vars <- c("beta",
                "wealth_gini",
                "wealth_average",
                "reputation_gini",
                "reputation_average",
                "bees_wealth_gini",
                "bees_wealth_average",
                "bees_reputation_gini",
                "bees_reputation_average",
                "rabbits_wealth_gini",
                "rabbits_wealth_average",
                "rabbits_reputation_gini",
                "rabbits_reputation_average",
                "RepAvg_difference",
                "RepGini_difference",
                "WealthAvg_difference",
                "WealthGini_difference")

#adjust variable type
for (i in categ_vars) {
  dfs[,i] <- as.factor(dfs[,i])
}

for (i in numer_vars) {
  dfs[,i] <- as.numeric(dfs[,i])
}

str(dfs)

# now lets save it on a smaller format:
#write.csv(dfs, file = "data_experiment7_total_shorter.csv")


#maybe only 1 sample size:
#dfs <- dfs[which((dfs$num.turtles.A==100) & (dfs$num.turtles.B==100)),]
#ok let's inspect the relationships visually

# difference scores to compare groups:
dfs$RepAvg_difference <- dfs$bees_reputation_average - dfs$rabbits_reputation_average
dfs$RepGini_difference <- dfs$bees_reputation_gini - dfs$rabbits_reputation_gini
dfs$WealthAvg_difference <- dfs$bees_wealth_average - dfs$rabbits_wealth_average
dfs$WealthGini_difference <- dfs$bees_reputation_gini - dfs$rabbits_reputation_gini
# ALWAYS BEES MINUS RABBITS
#BEES HAVE THE VARYING REP NORM


str(dfs)
variables_of_interest <- c(variables_of_interest, names(dfs[,c(26:29)]))
all_variables <- c(all_variables, names(dfs[,c(26:29)]))

# lets save the dataframe again
write.csv(dfs, file = "data_experiment7_total_short_final.csv")


#curvilinear associations:

#careful this takes long!!
#pairs.panels(dfs[,numer_vars],  
#              method = "pearson",
#              hist.col = "#00AFBB",
#              density = TRUE,  # show density plots
#              ellipses = TRUE) # show correlation ellipses

# wealth disparity:
ggplot(dfs, aes(x = beta, y = wealth_gini, colour = reciprocation.norm.A)) +
  geom_point() +
  facet_wrap( ~ initial.reputation.random.) #no effect?

ggplot(dfs, aes(x = beta, y = wealth_gini, colour = reciprocation.norm.A)) +
  geom_point() +
  facet_wrap( ~ investment.norm.A)

ggplot(dfs, aes(x = beta, y = wealth_gini, colour = investment.norm.A)) +
  geom_point() +
  facet_wrap( ~ reciprocation.norm.A)

#lets only look at congruent inv norms between population halves:

dfs <- dfs[which((dfs$investment.norm.A==dfs$investment.norm.B)),]

#again:
# wealth disparity:
ggplot(dfs, aes(x = beta, y = wealth_gini, colour = reciprocation.norm.A)) +
  geom_point() +
  facet_wrap( ~ investment.norm.A)

ggplot(dfs, aes(x = beta, y = wealth_gini, colour = investment.norm.A)) +
  geom_point() +
  facet_wrap( ~ reciprocation.norm.A)

#average wealth
ggplot(dfs, aes(x = beta, y = wealth_average, colour = reciprocation.norm.A)) +
  geom_point() +
  facet_wrap( ~ investment.norm.A)

ggplot(dfs, aes(x = beta, y = wealth_average, colour = investment.norm.A)) +
  geom_point() +
  facet_wrap( ~ reciprocation.norm.A)

#average rep
ggplot(dfs, aes(x = beta, y = reputation_average, colour = reciprocation.norm.A)) +
  geom_point() +
  facet_wrap( ~ investment.norm.A)

ggplot(dfs, aes(x = beta, y = reputation_average, colour = investment.norm.A)) +
  geom_point() +
  facet_wrap( ~ reciprocation.norm.A)


#now for the difference scores!
# wealth disparity:
ggplot(dfs, aes(x = beta, y = WealthGini_difference, colour = reciprocation.norm.A)) +
  geom_point() +
  facet_wrap( ~ investment.norm.A)

ggplot(dfs, aes(x = beta, y = WealthGini_difference, colour = investment.norm.A)) +
  geom_point() +
  facet_wrap( ~ reciprocation.norm.A)

#average wealth
ggplot(dfs, aes(x = beta, y = WealthAvg_difference, colour = reciprocation.norm.A)) +
  geom_point() +
  facet_wrap( ~ investment.norm.A)

ggplot(dfs, aes(x = beta, y = WealthAvg_difference, colour = investment.norm.A)) +
  geom_point() +
  facet_wrap( ~ reciprocation.norm.A)

#average rep
ggplot(dfs, aes(x = beta, y = RepAvg_difference, colour = reciprocation.norm.A)) +
  geom_point() +
  facet_wrap( ~ investment.norm.A)

ggplot(dfs, aes(x = beta, y = RepAvg_difference, colour = investment.norm.A)) +
  geom_point() +
  facet_wrap( ~ reciprocation.norm.A)

#seems like investment norm is nuisance?


#now let's do some testing
#first repeat all basics from experiment 6:

#standardize continuous predictors?
dfs$Zbeta <- scale(dfs$beta)
#or use as factor after all?
dfs$betaf <- as.factor(dfs$beta)
#options

#first lets fit a linear regression model
lm1 <- lm(data = dfs, 
          wealth_gini ~ beta*reciprocation.norm.A*investment.norm.A*initial.reputation.random.)

summary(lm1) 
lm.beta::lm.beta(lm1)
plot(lm1$fitted.values,lm1$residuals) # this looks bad! heteroscedastic 
qqnorm(lm1$residuals) # also not good

#since assumptions are violated lets try a robust regression:

lmrob1 <- robustbase::lmrob(data = dfs, 
                            wealth_gini ~ beta*reciprocation.norm.A*investment.norm.A)
#did not converge

#beta factor:
lmrob1 <- robustbase::lmrob(data = dfs, 
                            wealth_gini ~ betaf*reciprocation.norm.A*investment.norm.A)
summary(lmrob1) #did not converge again

#let's try anova:

aov1 <- aov(data = dfs, 
            wealth_gini ~ beta*reciprocation.norm.A*investment.norm.A)

summary(aov1)
plot(aov1,3) # oh dear
qqnorm(aov1$residuals)

#assumptions are not met for linear regression or anova
#
#lets see what WRS2 package can do for us with robust estimators:

raov1 <- WRS2::t3way(data = dfs, 
                     formula = wealth_gini ~ betaf*reciprocation.norm.A*investment.norm.A)

raov1 #checks out

raov2 <- WRS2::t1way(data = dfs, 
                     formula = wealth_gini ~ betaf)
summary(raov2)# no values -.-

#walrus package:
ranova1 <- walrus::ranova(data = dfs,
                          dep = "wealth_gini",
                          factors = c("betaf", "reciprocation.norm.A", "investment.norm.A")
)
#ranova1_results <- as.data.frame(ranova1$main)
#ranova1_results[3]

ranova1$main
#p values all <.0001

ranova1$phs    #post hoc tests, what went wrong here?

#I do declare the effects I see on the plots as confirmed...
#...by a bunch of tests without fitting models, but with fitting results...

#now for the other DVs:

#average wealth:

#first lets fit a linear regression model 
lm1 <- lm(data = dfs, 
          wealth_average ~ beta*reciprocation.norm.A*investment.norm.A)

summary(lm1)
lm.beta::lm.beta(lm1)
plot(lm1$fitted.values,lm1$residuals) # this looks bad!
qqnorm(lm1$residuals) #super violated

raov1 <- WRS2::t3way(data = dfs, 
                     formula = wealth_average ~ betaf*reciprocation.norm.A*investment.norm.A)
raov1

ranova1 <- walrus::ranova(data = dfs,
                          dep = "wealth_average",
                          factors = c("betaf", "reciprocation.norm.A", "investment.norm.A")
)
#ranova1_results <- as.data.frame(ranova1$main)
#ranova1_results[3]

ranova1$main
#p values all <.0001
ranova1$phs    #post hoc tests, what went wrong here?





#now with reputation average:

#first lets fit a linear regression model 
lm1 <- lm(data = dfs, 
          reputation_average ~ beta*reciprocation.norm.A*investment.norm.A)

summary(lm1)
lm.beta::lm.beta(lm1)
plot(lm1$fitted.values,lm1$residuals)

qqnorm(lm1$residuals) #super violated

raov1 <- WRS2::t3way(data = dfs, 
                     formula = reputation_average ~ betaf*reciprocation.norm.A*investment.norm.A)
raov1

ranova1 <- walrus::ranova(data = dfs,
                          dep = "reputation_average",
                          factors = c("betaf", "reciprocation.norm.A", "investment.norm.A")
)

ranova1$main




# I am inclined to remove investment norm, especiallz after visual inspection
# but first lets predict the difference scores:

#wealth gini difference:
#first lets fit a linear regression model 
lm1 <- lm(data = dfs, 
          WealthGini_difference ~ beta*reciprocation.norm.A*investment.norm.A)

summary(lm1)
lm.beta::lm.beta(lm1)
plot(lm1$fitted.values,lm1$residuals) #hmm not as bad
qqnorm(lm1$residuals)

raov1 <- WRS2::t2way(data = dfs, 
                     formula = WealthGini_difference ~ betaf*reciprocation.norm.A*investment.norm.A)
raov1

# ranova1 <- walrus::ranova(data = dfs,
#                           dep = "WealthGini_difference",
#                           factors = c("betaf", "reciprocation.norm.A")
# )
# 
# ranova1$main
# # walrus package crashes R sometimes...

#looks like we could omit inv norm here!

#avg welath dif:
lm1 <- lm(data = dfs, 
          WealthAvg_difference ~ beta*reciprocation.norm.A*investment.norm.A)

summary(lm1)
lm.beta::lm.beta(lm1)
plot(lm1$fitted.values,lm1$residuals) #help
qqnorm(lm1$residuals)

raov1 <- WRS2::t3way(data = dfs, 
                     formula = WealthAvg_difference ~ betaf*reciprocation.norm.A*investment.norm.A)
raov1



#avg rep dif:
lm1 <- lm(data = dfs, 
          RepAvg_difference ~ beta*reciprocation.norm.A*investment.norm.A)

summary(lm1)
lm.beta::lm.beta(lm1)
plot(lm1$fitted.values,lm1$residuals) #hmm not as bad
qqnorm(lm1$residuals)

raov1 <- WRS2::t3way(data = dfs, 
                     formula = RepAvg_difference ~ betaf*reciprocation.norm.A*investment.norm.A)
raov1











# THIS PART BELOW IS NOT DONE YET
# i've just been playing around with a SEM, work in progress



model <- 'wealth_gini ~ beta + investment_norm + reciprocaiton_norm + death_broke + cost_of_life
gini_rep ~ beta + investment_norm + reciprocaiton_norm + death_broke + cost_of_life
total_wealth ~ beta + investment_norm + reciprocaiton_norm + death_broke + cost_of_life
total_reputation ~ beta + investment_norm + reciprocaiton_norm + death_broke + cost_of_life
'

fit <- lavaan(model = model,
              data = data,
              meanstructure = T, 
              sample.cov = cov, 
              sample.nobs = 406,
              auto.var = T, 
              int.ov.free= T, 
              fixed.x = F)

summary(fit, 
        fit.measures=T, 
        modindices=T, 
        rsquare = T)

parameterEstimates(fit, 
                   ci = T, 
                   level = 0.95, 
                   boot.ci.type = "bca.simple")[c("lhs", "rhs","est", "ci.lower", "ci.upper")]

#let's have a look at the finished model 

semPaths(fit, 
         what= "std", 
         layout = "tree", 
         rotation = 2, 
         intercepts = F, 
         residuals = F, 
         curve = 2, 
         nCharNodes = 10, 
         edge.label.cex = 1.8, 
         edge.color = "black", 
         sizeMan = 18, 
         sizeMan2 = 10)











