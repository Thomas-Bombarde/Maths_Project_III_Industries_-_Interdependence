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

#------------------------------------- Clean Data

# 1) Treating outliers"
# Outliers are data-points outside the overall pattern of data. They might be 
# due to measurement errors or exogenous shocks that hinder our analysis.
# To find outliers, compute the centre and covariance matrix of our
# 2 variables:

vars <- df[c("innov.pred" , "innov.obs")]
vars.centre <- colMeans(vars)
vars.cov <- cov(vars)

# (The following method was taken from : 
# https://towardsdatascience.com/mahalonobis-distance-and-outlier-detection-in-r-cb9c37576d7d)

# To visualise outliers, we can plot the the ellipse with radius the threshold of
# mahalanobis distance threshold we would like to cut off. Ellipse radius from 
# Chi-Sqaure distrubiton:"
rad <- qchisq(p = 0.99999 , df = ncol(vars)) |> sqrt()

# Finding ellipse coordiantes
ellipse <- car::ellipse(center = vars.centre , shape = vars.cov , radius = rad ,
                        segments = 150 , draw = FALSE) |> 
  as.data.frame()
colnames(ellipse) <- colnames(vars)

# Create scatter Plot
par(mfrow=c(2,1))
figure.mahalanobis <- ggplot(vars , aes(x = innov.pred , y = innov.obs)) +
  geom_point(size = 2) +
  geom_polygon(data = ellipse , fill = "orange" , color = "orange" , alpha = 0.5)+
  geom_point(aes(vars.centre[1] , vars.centre[2]) , size = 5 , color = "blue") +
  geom_text(aes(label = row.names(vars)) , hjust = 1 , vjust = -1.5 ,size = 2.5 ) + 
  labs(x = "Average nearest neighbour 10-year growth rate",
       y = "10-year Industry growth rate")

# Finding mahalanobis distances of data points
distances <- mahalanobis(x = vars , center = vars.centre , cov = vars.cov)

# Cutoff value for distances from Chi-Sqaure Dist. with p = 0.95 df = 2
cutoff <- qchisq(p = 0.99999, df = ncol(vars))

# drop observations whose distance is greater than cutoff value
outliers <- vars[distances > cutoff ,]
drop <- rownames(outliers) |> as.numeric()
df <- slice(df, -drop)

# -------------------------------------------------------------------------