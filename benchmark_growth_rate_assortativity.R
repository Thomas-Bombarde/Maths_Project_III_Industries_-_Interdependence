#install.packages("ggplot2")
#install.packages("readxl")
#install.packages("dplyr")
#install.packages("tidyr")
library(dplyr)
library(tidyr)
library(ggplot2)
library(readxl)
library(R.matlab)
library(stringr)
library(imputeTS, include.only = 'na.replace') 
library(latex2exp)

df.prods <- readRDS(
  "~/2023\\2023 Math Project III\\output\\df.prod_anng_Mcnerney.Rds")
simu <- readRDS(
  "~/2023\\2023 Math Project III\\output\\df.randommat1.Rds") |> slice(-1)

df.prods.cleaned <- readRDS(
  "~/2023\\2023 Math Project III\\output\\innovs_cleaned.Rds")
sim_cleaned <-  readRDS(
  "~/2023\\2023 Math Project III\\output\\sim_cleaned.Rds")

setwd("~/2023/2023 Math Project III/output")


years.innov <- (1996:2008)
n <- length(years.innov)
ANNG_yr <- data.frame("Year" = years.innov, "ANNG_correlation" = rep(0,n))
for (i in (1:n)){
  tmp <- df.prods.cleaned |> mutate(Year = as.character(Year))
  tmp <- tmp |> 
    filter(Year == as.character(years.innov[i]))
  ANNG_yr[i,2] <- cor(tmp$ANNG_in, tmp$Gamma)
}
ANNG_yr

# how about only EU countries
eu <- df.prods.cleaned |> filter(Country %in% c("AUT", "BEL", "FRA", "CYP", "CZE", 
                                                "DEU", "DNK", "ESP", "EST", "FIN",
                                                "GBR", "GRC", "HUN", "ITA", "IRL",
                                                "LUX", "LUX", "LVA", "NLD", "POL", 
                                                "PRT", "ROM", "SVK", "SVN", "SWE"))
europe <- df.prods.cleaned |> filter(Country %in% c("AUT", "BEL", "FRA", "CYP", "CZE", 
                                                    "DEU", "DNK", "ESP", "EST", "FIN",
                                                    "GBR", "GRC", "HUN", "ITA", "IRL",
                                                    "LUX", "LUX", "LVA", "NLD", "POL", 
                                                    "PRT", "ROM", "SVK", "SVN", "SWE"))

ggplot(filter(df.prods.cleaned, Year == "2000"), aes(ANNG_in, Gamma, col = Country)) +
  geom_point() +
  theme(axis.text = element_text(size =13), axis.title=element_text(size=14)) +
  labs(x = "Productivity ANNG (2000)", y = "Productivity Growth rate", size=7)+
  annotate("text", x=-0.03, y=0.17, label= "Pearson cor: 0.400", size=5)
ggplot(filter(df.prods.cleaned, Year == "2002"), aes(ANNG_in, Gamma, col = Country)) + geom_point()
ggplot(ANNG_yr, aes(Year, ANNG_correlation)) + geom_point()
#year.nobs <- count(simu, Year)
#left_join(simu, year.nobs)
groupped <- sim_cleaned |> group_by(Year, sim) |>
  mutate("rho" = cor(Gamma, ANNG_in)) |> 
  ungroup() |> 
  group_by(Year) |> 
  mutate("avrg_rho" = cor(Gamma, ANNG_in)) |> 
  mutate("Var" = mean((rho-avrg_rho)^2))
unique(groupped$Var)
unique(groupped$`95CI_upr`)

groupped <- groupped |> mutate("95CI_lwr" = avrg_rho - qnorm(0.95)*sqrt(Var),
                               "95CI_upr" = avrg_rho + qnorm(0.95)*sqrt(Var))

df <- inner_join(groupped, ANNG_yr, copy=TRUE)
anngcorplot <- df[!duplicated(df[,8]),]
unique(df$Year)
plot <- ggplot(anngcorplot) + geom_point(aes(Year, ANNG_correlation), size =2) +
  annotate("text", x=1999, y=-0.05, label= "Random Network", size =5) +
  annotate("text", x=1999, y=0.3, label= "WIOD", size = 5) +
  theme(axis.text = element_text(size =14), axis.title=element_text(size=15)) +
  labs(x = "Year (1997-2007)", y = "Growth Rate assortativity", size=7)+
  geom_line(aes(Year, avrg_rho), size=0.5) +
  geom_ribbon(aes(x=Year, ymin= `95CI_lwr`, ymax= `95CI_upr`),alpha=0.2) 
#+  theme(text = element_text(size = 20))
pdf("anngplotwoim.pdf")
print(plot)


# Save plots to tiff. Makes a separate file for each plot.
for (i in 1:ncountries) {
  file_name = paste("ANNGplot", i, ".tiff", sep="")
  tiff(file_name)
  print(plot_list[[i]])
  dev.off()
}

# Another option: create pdf where each page is a separate plot.
pdf("plots.pdf")
for (i in 1:ncountries) {
  print(plot_list[[i]])
}
dev.off()


plot(Year,obscor, bty ="l", ylim=c(-0.3,1), pch=15, xlab = "Observation year (1995-2001)", ylab="Average nearest neighbour \n growth correlation (GBR)", col="brown1", 
     cex.lab=1.7)
lines(Year, bstp_estimate, col = "brown1",lwd=2)
matlines(Year, se.bands1, col = "brown1", lty =2, lwd=2)



