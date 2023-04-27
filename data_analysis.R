setwd("~\\2023 Math Project III\\output")

install.packages("readxl")
install.packages("dplyr")
install.packages("tidyr")
install.packages("ggplot2")
install.packages("imputeTS")

#------------------------------------- Libraries
library(dplyr)
library(tidyr)
library("readxl")
library(ggplot2)
#library(igraph) 
library(imputeTS, include.only = 'na.replace') 

#------------------------------------- Data
df <- readRDS("~/2023 Math Project III/output/ANNGandObs.Rds")
countries <- unique(df$Country)

#------------------------------------- Clean Data
sum(is.finite(df$innov.obs) != TRUE)

# Correct Inf values (these arise for when observed output growth is 0
# so the denominator in the calculation of productivity is 0. we can
# replace these values with producitivty growth =0))
df$innov.obs <- ifelse(is.finite(df$innov.obs), df$innov.obs, 0)
#df$innov.pred <- ifelse(is.finite(df$innov.pred), df$innov.pred, 0)

# Convert 'inputs', 'sector, 'Industry code' and 'Country' to factor values
tmp <- sapply(df |> select(-c(innov.obs, innov.pred)), as.factor)
df <- cbind(tmp, select(df, innov.obs), select(df, innov.pred)) |> 
  as.data.frame() |> mutate("d2" = (innov.obs - innov.pred)^2)

# check NA
sum(is.na(select(df, c(innov.obs, innov.pred))))



#------------------------------------- results by industry
# df <- df |> filter(Year != "2008")

input_names <- unique(df$inputs)
sector <- df$sector
indusreg <- data.frame(input_names, 
                       "R2" = rep(0,35),
                       "estimate" = rep(0,35),
                       "ttest" =  rep(0,35))
for (i in (1:35)){
  tmp <- df |> filter(inputs == input_names[i])
  fit <- lm(tmp$innov.obs ~ tmp$innov.pred)
  results <- summary(fit)
  indusreg[i,(2:4)] <- c(results$r.squared, results$coefficients[2,1], results$coefficients[2,4])
  print(results)
}
indusreg <- left_join(indusreg, unique(select(df, inputs, sector)), by = c("input_names" = "inputs"))
indusreg <- arrange(indusreg, desc(R2))
setwd("~/2023 Math Project III/output")
saveRDS(indusreg, file = "indusreg")

names(indusreg) <- c("Industry", "R-squared", "ANNG-Coefficient", "Pr(>|t|)", "Sector")
install.packages("xtable")
library(xtable)
print(xtable(indusreg, type = "latex"), file = "indusreg.tex")

#------------------------------------- results by country
country_names <- unique(df$Country)
len <- length(country_names)
countryreg <- data.frame(country_names,
                         "R2" = rep(0,len),
                         "estimate" = rep(0,len),
                         "ttest" =  rep(0,len))
for (i in (1:length(country_names))){
  C = country_names[i]
  tmp <- df |> filter(Country == C)
  fit <- lm(tmp$innov.obs ~ tmp$innov.pred)
  results <- summary(fit)
  countryreg[i,(2:4)] <- c(results$r.squared, results$coefficients[2,1], results$coefficients[2,4])
  print(results)
  ggplot(df |> filter(Country != C),aes(x=innov.pred, y=innov.obs)) +
    geom_point() +
    labs(x = "Average nearest neighbour 10-year growth rate", y = "10-year Industry growth rate")
}
countryreg <- arrange(countryreg, desc(R2))
setwd("~/2023 Math Project III/output")
saveRDS(countryreg, file = "countryreg")
print(xtable(countryreg, type = "latex"), file = "countryreg.tex")

high_assortativity <- indusreg %>% filter(`R-squared`>0.20)
low_assortativity <- indusreg %>% filter(`R-squared`<0.20)
high_assortativityC <- countryreg %>% filter(`R2`>0.20)
low_assortativityC <- countryreg %>% filter(`R2`<0.20)
