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

data <- read.table("FINAL_n_person_TG_deltaAB experiment7-table.csv",
                  header = T,   # set columns names true
                  sep = ',',    # define the separator between columns
                  skip = 6,     # skip first 6 rows 
                  quote = '\"',  # correct the column separator
                  fill = TRUE ) # add blank fields if rows have unequal length

data <- read.csv(file = "data_experiment7.csv")

str(data)

data_dt <- as.data.table(data) # data table so I can use the string splitting functions below

data$wealth <- data.frame(data_dt[,tstrsplit(str_remove_all(data$X.wealth..of.turtles,'\\]|\\['),' ')])

data$reputation <- data.frame(data_dt[,tstrsplit(str_remove_all(data$X.reputation..of.turtles,'\\]|\\['),' ')])

str(data)

#let's summarize the agents values into variables we want to use, first wealth: 
for (i in 1:nrow(data$wealth)){
  data$wealth_gini[i] <- DescTools::Gini(data$wealth[i,], na.rm = T)     # one row full of values for all the agents is aggregated into 1 gini coefficient
}

str(data$wealth_gini)

for (i in 1:nrow(data$wealth)){
  data$wealth_total[i] <- sum(as.numeric(data$wealth[i,]), na.rm = T)
}

for (i in 1:nrow(data$wealth)){
  data$wealth_average[i] <- mean(as.numeric(data$wealth[i,]), na.rm = T)
}

# now also with reputation

for (i in 1:nrow(data$reputation)){
  data$reputation_gini[i] <- DescTools::Gini(data$reputation[i,], na.rm = T)
}

str(data$reputation_gini)

for (i in 1:nrow(data$reputation)){
  data$reputation_total[i] <- sum(as.numeric(data$reputation[i,]), na.rm = T)
}

for (i in 1:nrow(data$reputation)){
  data$reputation_average[i] <- mean(as.numeric(data$reputation[i,]), na.rm = T)
}

str(data)

write.csv(data, file = "data_experiment7.csv")

unique(data$num.turtles.A)

names(data)

identical(data$beta, data$beta.1) # these are the same, will use beta

variables <- names(data[,c(1, 4, 6, 10, 13, 20, 23, 32, 33, 36, 37, 47, 48, 56, 57, 58, 59 , 60)])
variables
shortdf <- data[, variables]

str(shortdf)
#lets save it on a smaller format:
write.csv(shortdf, file = "data_experiment7_short.csv")


# let|s get the proper variable types here. 
# numeric variables:
for (i in c(8, 13, 14,15,16,17,18,19)) {
  dfs[,i] <- as.numeric(dfs[,i])
}
#and categorical:
for (i in c(1:7, 9, 10, 11, 12)) {
  dfs[,i] <- as.character(dfs[,i])
}

str(dfs)

write.csv(dfs, file = "data_experiment7_short.csv")

#ok let's inspect the relationships visually

#curvilinear associations:
data_short <- dfs[c("beta", "wealth_gini", "wealth_average", "reputation_gini", "reputation_average")]
pairs.panels(data_short,
             method = "pearson",
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE) # show correlation ellipses

#different group sizes:
ggplot(dfs, aes(x = beta, y = wealth_gini, colour = num.turtles.A)) +
  geom_point() +
  facet_wrap( ~ num.turtles.B)

# wealth disparity:
ggplot(dfs, aes(x = beta, y = wealth_gini, colour = num.turtles.A)) +
  geom_point() +
  facet_wrap( ~ delta.A)

ggplot(dfs, aes(x = beta, y = wealth_gini, colour = delta.A)) +
  geom_point() +
  facet_wrap( ~ num.turtles.A)

#hmm looks like delta doesnt have an effect!

summary(aov(data = dfs, 
            wealth_gini ~ delta.A + num.turtles.A + phi + phi.r + beta))

summary(aov(data = dfs, 
            wealth_gini ~ delta.A * num.turtles.A * phi * phi.r * beta))

# what about phi
ggplot(dfs, aes(x = beta, y = wealth_gini, colour = num.turtles.A)) +
  geom_point() +
  facet_wrap( ~ phi)

ggplot(dfs, aes(x = beta, y = wealth_gini, colour = phi)) +
  geom_point() +
  facet_wrap( ~ num.turtles.A)

#average wealth
ggplot(dfs, aes(x = beta, y = wealth_average, colour = num.turtles.A)) +
  geom_point() +
  facet_wrap( ~ delta.A)

ggplot(dfs, aes(x = beta, y = wealth_average, colour = delta.A)) +
  geom_point() +
  facet_wrap( ~ num.turtles.A)






#outliers:
for (i in 1:length(data_short)) {
  print(c(names(data_short[i]), "has these outliers:", c(which(abs(scale(data_short[,i]))>3))))
}

# bivariate associations:

#correlations:
cormatrix <- cor(na.omit(data_short))
corrplot(cormatrix, order = "hclust", addCoef.col = "grey")

# correlations with p values, pairwise:
#corr.test(data_short)


#let's test the relationships for statistical significance:
anova_wealth_gini <- aov(wealth_gini ~ beta,
                         data = data)

summary(anova_wgini)

anova_wealth_total <- aov(wealth_total ~ beta,
                          data = data)

summary(anova_wealth_total)

# ... more anovas


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











