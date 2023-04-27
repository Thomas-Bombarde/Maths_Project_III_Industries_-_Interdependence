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
countryreg <- readRDS("~/2023 Math Project III/output/countryreg")
indusreg <- readRDS("~/2023 Math Project III/output/indusreg")
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

#------------------------------------- Split by Income group

df <- df |> mutate("income" = if_else(df$Country == "AUS" | df$Country == "AUT"|
                                        df$Country == "BEL"| df$Country ==  "CAN"
                                      | df$Country == "CYP" | df$Country == "DEU"
                                      | df$Country == "DNK"|df$Country ==  "ESP"
                                      | df$Country == "FIN"| df$Country == "GBR"
                                      |df$Country == "ITA"| df$Country == "JPN"
                                      |df$Country == "NLD"| df$Country ==  "PRT"
                                      | df$Country == "SWE"| df$Country =="USA"|
                                        df$Country == "FRA"|df$Country ==  "KOR",
                                      "high", "low"))
nrow(filter(df, income == "high"))
nrow(filter(df, income == "low"))
plot_names <- c("high" = "High Income Countries", "low" = "Low Income Countries")

#------------------------------------ Plots
figure.alldata <- ggplot(df,
                         aes(x=innov.obs, y=innov.pred)) +
  labs(x = "Average nearest neighbour 10-year growth rate",
       y = "10-year Industry growth rate") + 
  geom_point(aes(col = sector))

plot.inc <- ggplot(df, aes(x=innov.pred, y=innov.obs)) +
  geom_point(aes(col = sector)) + 
  labs(x = "Average nearest neighbour 10-year growth rate",
       y = "10-year Industry growth rate") +
  stat_smooth(data = filter(df, sector == "Industry"),
              method = "lm",
              se = TRUE,
              formula = y ~ x,
              geom = "smooth", colour = "green3") +
  stat_smooth(data = filter(df, sector == "Services"),
              se = TRUE,
              method = "lm",
              formula = y ~ x,
              geom = "smooth", colour = "cadetblue1") +
  stat_smooth(data = filter(df, sector == "Agriculture"),
              method = "lm",
              se = TRUE,
              formula = y ~ x,
              geom = "smooth", colour = "red")+
  facet_wrap(vars(income), labeller = as_labeller(plot_names), scales='free') +
  theme(legend.position = "none") +
  theme(legend.position = "none", text=element_text(size=30))

plot.agri <- ggplot(filter(df, sector =="Agriculture"),aes(x=innov.pred, y=innov.obs)) +
  geom_point(aes(col = income)) + 
  labs(x = "Average nearest neighbour 10-year growth rate",
       y = "10-year Industry growth rate")+
  stat_smooth(data = filter(df, income =="low" & sector == "Agriculture"),
              method = "lm",
              se = TRUE,
              formula = y ~ x,
              geom = "smooth", colour = "cadetblue1")+
  stat_smooth(data = filter(df, income =="high" & sector == "Agriculture"),
              method = "lm",
              se = TRUE,
              formula = y ~ x,
              geom = "smooth", colour = "tomato") +
  theme(legend.position = "none", text=element_text(size=30))

#png("plot.inc.png", width=2500, height=1400, res = 200)
plot.inc
#dev.off()

#png("plot.agri.png", width=2000, height=1400, res = 200)
plot.agri
#dev.off()



# Plot ANNG and observed innovation rates by country
plt_ctry <- list()
dev.off()
ncountries <- length(countries)
for (i in (1:ncountries)){
  C <- countries[i]
  y.lab = paste("10-year productivity improvement,", C)
  x.lab = paste("10-year ANNG,", C)
  tmp <- df |> filter(Country == C)
  pearson <- countryreg %>% filter(country_names == C) %>% select(R2) %>% round(digits=3)
  
  max <- max(tmp$innov.obs)
  max_x <- max(tmp$innov.pred)
  plt_ctry[[i]] <- ggplot(tmp, aes(innov.pred, innov.obs)) +  labs(x=x.lab, y=y.lab) +   geom_point() +
    theme(text = element_text(size = 20)) +
    annotate("text", x=0.25*max_x, y= max-1, label= paste("Pearson cor:", as.numeric(pearson)), size = 6) +
    scale_y_continuous(labels = waiver()) +
    stat_smooth(data = tmp,
                method = "lm",
                se = TRUE,
                formula = y ~ x,
                geom = "smooth", col="black") 
}
# Save plots to tiff. Makes a separate file for each plot.
setwd("~/2023 Math Project III/output")
for (i in 1:ncountries) {
  file_name = paste("ctry", i, ".tiff", sep="")
  tiff(file_name)
  print(plt_ctry[[i]])
  dev.off()
}

plt_indus <- list()
input_names <- unique(df$inputs)
nindus <- length(input_names)
for (i in (1:nindus)){
  C <- input_names[i]
  y.lab = paste("10-year productivity improvement, \n", C)
  x.lab = paste("10-year ANNG, \n", C)
  tmp <- df |> filter(inputs == C)
  pearson <- indusreg %>% filter(input_names == C) %>% select(R2) %>% round(digits=3)
  
  max <- max(tmp$innov.obs)
  max_x <- max(tmp$innov.pred)
  plt_indus[[i]] <- ggplot(tmp, aes(innov.pred, innov.obs)) +  labs(x=x.lab, y=y.lab) +   geom_point() +
    theme(text = element_text(size = 20)) +
    annotate("text", x=0.25*max_x, y= max-1, label= paste("Pearson cor:", as.numeric(pearson)), size = 6) +
    scale_y_continuous(labels = waiver()) +
    stat_smooth(data = tmp,
                method = "lm",
                se = TRUE,
                formula = y ~ x,
                geom = "smooth", col="black") 
}
# Save plots to tiff. Makes a separate file for each plot.
setwd("~/2023 Math Project III/output")
for (i in 1:nindus) {
  file_name = paste("indsutry", i, ".tiff", sep="")
  tiff(file_name)
  print(plt_indus[[i]])
  dev.off()
}


tmp <- df |> filter(Country %in% low_assortativityC$country_names)
ggplot(tmp, aes(innov.pred, innov.obs)) +  labs(x=x.lab, y=y.lab) +   geom_point() +
  theme(text = element_text(size = 20)) +
  annotate("text", x=2, y=2, label= "Pearson cor: 0.05015", size = 6) +
  stat_smooth(data = tmp,
              method = "lm",
              se = TRUE,
              formula = y ~ x,
              geom = "smooth", col="black") 

# Plot ANNG and observed innovation rates in the UK
dev.off()
x.lab = "10-year industry productivity improvement (, 1995-2001)"
y.lab = "10-year neighborhood industries \n productivity improvement (, 1995-2001)"
INDUS <- "Other Non-Metallic Mineral"
INDUS <- "Hotels and Restaurants"
INDUS <- "Construction"
INDUS <- "Private Households with Employed Persons"
tmp <- df |> filter(income == "high")
ggplot(tmp, aes(innov.pred, innov.obs)) +  labs(x=x.lab, y=y.lab) +   geom_point() +
  theme(text = element_text(size = 20)) +
  annotate("text", x=2, y=2, label= "Pearson cor: 0.05015", size = 6) +
  stat_smooth(data = tmp,
              method = "lm",
              se = TRUE,
              formula = y ~ x,
              geom = "smooth", col="black") 


#scale_x_discrete(labels = function(x) str_wrap(x, width = 10))
#par(mar = c(5.1, 7, 4.1, 2.1))
# bottom, left, top, and right. The default is c(5.1, 4.1, 4.1, 2.1).
#plot(tmp$innov.pred, tmp$innov.obs, xlab=x.lab, ylab=y.lab, pch=16, col="cornflowerblue", 
#cex.lab=1.7)=
#model <- lm(tmp$innov.obs ~ tmp$innov.pred)
#results <- summary(model)

#add fitted regression line
#abline(model, col="red", lwd = 2)
#text(2, 2, "Pearson cor:", cex =1.7)
