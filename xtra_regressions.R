#------------------------------------- Libraries
library(tidyverse)
library(patchwork)
library(MASS)

#------------------------------------- Data
df <- readRDS("~/2023 Math Project III/output/ANNGandObs.Rds")
countries <- unique(df$Country)

#------------------------------------- Clean Data
# Correct Inf values
df$innov.obs <- ifelse(is.finite(df$innov.obs), df$innov.obs, 0)
df$innov.pred <- ifelse(is.finite(df$innov.pred), df$innov.pred, 0)

# Convert 'inputs', 'sector, 'Industry code' and 'Country' to factor values
tmp <- sapply(df |> select(-c(innov.obs, innov.pred)), as.factor)
df <- cbind(tmp, select(df, innov.obs), select(df, innov.pred)) |> 
  as.data.frame() |> mutate("d2" = (innov.obs - innov.pred)^2)
sum(is.na(select(df, c(innov.obs, innov.pred))))

#------------------------------------- Regression
#These visual results can be confirmed by regressing:
fit <- lm(innov.obs ~ innov.pred + sector + sector*income, df)

# The standard errors violate the normality assumption, so we try to fix this by
# taking logs, or more efficiently a box-cox regression: 

# logs
y <- log(df$innov.obs - min(df$innov.obs))
y <- ifelse(is.finite(y), y, 0)
fit <- lm(y ~ df$innov.pred + df$sector + df$sector*df$income)
summary(fit)
plot(fit)

# Box Cox
# i start with a box cox transformation because to respect the assumptions of
# linear regression
boxcoxreg <- function(df){
  K <- min(df$innov.obs)
  y <- df$innov.obs + 1
  x <- df$innov.pred + 1
  bc <- boxcox(y ~ x)
  lambda <- bc$x[which.max(bc$y)]
  new_model <- lm(((y^lambda-1)/lambda) ~ x)
  return(new_model)
}

model1 <- boxcoxreg(df)
results <- summary(model1)


#------------------------------------- Dropping 2008
# We can also check whether the period taken has an influence, for
# example due to the 2008 recession: 
fit <- lm(innov.obs ~ innov.pred + Year + Year*innov.pred, df)
summary(fit)

# Years following the GFC seem to have significant impact on productivity,
# however, the interaction of this with"

export_summs(fit, fit2, scale = TRUE)