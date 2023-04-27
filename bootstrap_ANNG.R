install.packages("ggplot2")
install.packages("readxl")
install.packages("dplyr")
install.packages("tidyr")


library(dplyr)
library(tidyr)
library(ggplot2)
library(readxl)

library(stringr)
library(imputeTS, include.only = 'na.replace') 
library(latex2exp)


setwd("~/2023 Math Project III/output")

# ---------------------------- Data

periods <- readRDS(
  "~/2023 Math Project III/output/periods.Rds")

sea <- read_excel("~/2023 Math Project III\\input\\Socio_Economic_Accounts_July14.xlsx", 
                  sheet = "DATA")

df <- readRDS("~/2023 Math Project III\\output\\ANNGandObs.RDs")

# ---------------------------- Functions
ttoutput <- 
  function(periods, C, j){
    "Compute the total output (including household consumption) table for a
    country C in observation year j"
    tt_output <- periods[[j]] |>
      filter(Country == C) |>
      dplyr::select("Industry", "Total Output") |> 
      mutate(across(!Industry, as.numeric))
    tmp1 <-  sea |>
      filter(Country==C) |> 
      filter(Variable=="GO" & Code!= "TOT")|> 
      dplyr::select(4+j) |> as.matrix() |> as.numeric()
    tmp2 <-  sea |>
      filter(Country==C) |> 
      filter(Variable=="II" & Code!= "TOT")|> 
      dplyr::select(4+j) |> as.matrix() |> as.numeric()
    cons <- data.frame("Industry"="Labour", "Total Output"=sum(tmp1)-sum(tmp2))
    names(cons) <- names(tt_output)
    tt_output <- rbind(tt_output,cons)
    rm(tmp1, tmp2, cons)
    return(tt_output)
  }
WIOTi <- function(periods, i){
  "Extract the world input-output table for observation year i"
  WIOT <- periods[[i]]
  C <- WIOT[(6:1441),3] #country names
  X1 <- WIOT[(6:1441),] |> dplyr::select((5:1440)) |> as.matrix()
  X1 <- apply(X1, 2, as.numeric)
  X1 <- cbind(C, X1)
  names(X1[,-1]) <- names(WIOT)[(6:1441)]
  return(X1)
}
Ai <- function(NIOT){
  "Return the column-normalised adjacency matrix for a given NIOT"
  A <- NIOT
  #as.data.frame(NIOT) |> dplyr::select(contains(C)) |>    dplyr::select(ends_with("1"):ends_with("35")) |> slice(-36) |> as.matrix()
  A <- sweep(A, 1, colSums(A), "/")
  A[which(is.finite((A))==FALSE)] <- 0
  A[is.na(A)] <- 0 #fix NaN that arises from rowSums(A)=0
  return(A)
}
A.offdiag <- function(Ai){
  "Return the off diagonal elements of the matrix A"
  A <- Ai - diag(diag(Ai))
  return(A)
}
NIOTi <- function(C, WIOT, sea, j){
  "Compute a country C's input/output matrix including households for 
   observation year j"
  # total flow of money from industries to households in country C
  lab <- sea |>
    filter(Country==C) |> 
    filter(Variable=="LAB" & Code!= "TOT") |> 
    dplyr::select(4+j)
  
  # compute flow of money from households to industries as the residual between 
  # total industry output and industry output serving as inputs to industries
  tmp1 <-  sea |>
    filter(Country==C) |> 
    filter(Variable=="GO" & Code!= "TOT")|> 
    dplyr::select(4+j) |> as.matrix() |> as.numeric()
  tmp2 <-  sea |>
    filter(Country==C) |> 
    filter(Variable=="II" & Code!= "TOT")|> 
    dplyr::select(4+j) |> as.matrix() |> as.numeric()
  cons <- t(tmp1-tmp2)
  cons <- cbind(cons,0) # total labour consumption to each industry + households
  rm(tmp1, tmp2)
  
  # Extract country input-output table
  X1 <- WIOT |> 
    filter(Country == C) |> 
    dplyr::select(contains(C)) |>
    dplyr::select(ends_with("1"):ends_with("35")) |> 
    mutate_all(as.numeric)
  
  # Combine labour compensations per industries and national IO table
  X1 <- cbind(X1, lab)
  cons <- cons |> as.data.frame()
  names(cons) <- names(X1)
  X1 <- rbind(X1, cons) |> sapply(as.numeric)
  X1[is.na(X1)] <- 0
  print(C)
  return(X1)
}
pred_innov2 <- function(A, innovs, n){
  "Compute predicted innovations for a given country k based on actual 
  innovation rates"
  pred <- rep(0,n) # a vector of the number of industries
  Abis <- A - diag(diag(A)) # the adjacency matrix minus self-links
  tmp <- 0
  for (i in (1:n)){
    tmp <- 0
    # for a given industry i
    for (k in (1:n)){
      # compute the weighted (by A) productivity improvements of all its industries j
      if(is.infinite(innovs[k,1])!=TRUE & is.na(innovs[k,1])!=TRUE){
        # Summing over all industries j != i
        # add the innovation rate of industry j in country k weighted by its
        # input share to industry i 
        tmp <- tmp + as.numeric(Abis[k,i])*innovs[k] 
      }
    }
    pred[i] <- tmp #industry i's productivity improvement
  }
  return(pred)
}
rewire_A <- function(A){
  C <- A |> as.vector()
  d <- dim(A)[1]
  rewired_col <- sample(1:d, d)
  rewired_row <- sample(1:d, d)
  A.rewired <- A
  for (i in (1:d)){
    for (j in (1:d)){
      A.rewired[i,j] <- A[rewired_row[i], rewired_col[j]]
    }
  }
  return(A.rewired)
}

# ---------------------------- Data clean
WIOTS <- list()
max_obs <- length(periods)
for (t in 1:(max_obs)){
  WIOT1 <- WIOTi(periods, t)
  WIOTS[[t]] <- WIOT1
}
saveRDS(WIOTS, file = "WIOTS.Rds")

WIOTS <- readRDS("~/WIOTS.Rds")

# ---------------------------- Global Variables
n <- 35 # number of industries
inputs <- periods[[1]] |> filter(Industry!="Industry")|> dplyr::select(Industry) |>
  unique() |>  na.omit() |> slice(1:n) |> as.matrix() |> as.vector()
all_inputs <- c(inputs, "Labour")

countries <- periods[[1]] |> filter(Country!="Country")|>  dplyr::select(Country) |> 
  unique() |>   filter(Country !="TOT") |>   na.omit()
countries <- countries |> filter(Country != "ROM") |> filter(Country != "RoW")
ncountries <- nrow(countries)
i <- 16


# ------------------------------ Analysis
# prepare matrix for new rewired predicted innovations
# an extract observed innovations for country in question
plot_list <- list()
c <- 16
for (c in (1:ncountries)){
  C <- countries[c,1] %>% as.character()
  innovs <- df |> filter(Country == C) |> dplyr::select(innov.obs)
  boot_iter <- 50
  bootstrap_cors <- list(c(), c(), c(), c(), c(), c(), c())
  max_obs <- length(WIOTS)
  span <- 10
  pred_innovs <- c()
  
  # compute the mean correlation over rewired A matrices
  bt_cors_iter <- c()
  bt_cors <- c()
  for (i in 1:boot_iter){  
    bt_cors <- c()
    for (t in (1:(max_obs-span))){
      # resample and rewire A
      WIOT1 <- WIOTS[[t]]
      X1 <- NIOTi(C, WIOT1, sea, t)
      X1 <- A.offdiag(X1[(1:35), (1:35)])
      A <- Ai(X1)
      A <- rewire_A((A))
      innov_yr_t <- innovs[seq((t-1)*35+1,(t)*35),1] |> as.matrix()
      gamma <- pred_innov2(A, innov_yr_t, n)
      bt_cors <- rbind(bt_cors, cor(gamma, as.vector(innov_yr_t)))
    }
    bt_cors_iter <- cbind(bt_cors_iter, bt_cors)
  }
  
  bstp_estimate <- c()
  varhat <- c()
  obscor <- c()
  for (t in (1:(max_obs-span))){
    # estimate and variance of rho given random matrix in year t
    bstp_estimate[t] <- rowMeans(bt_cors_iter)[t]
    varhat <- mean((-bstp_estimate[t] + bt_cors_iter[t,])^2)
    
    # observed rho for year t in country C
    tmp <- df |> filter(Country == C & Year == 2005+(t-1))
    obscor[t] <- cor(tmp$innov.obs,tmp$innov.pred)
  }
  
  ci.lwr <- bstp_estimate - qnorm(0.95)*sqrt(varhat)
  ci.upr <- bstp_estimate + qnorm(0.95)*sqrt(varhat)
  Year <- unique(df$Year) -10
  
  
  anngcorplot <- data.frame("Year" = Year, "obscor" = obscor, "bstp_estimate" = bstp_estimate, "ci.lwr"=ci.lwr, "ci.upr"=ci.upr)
  plot_list[[c]] <- ggplot(anngcorplot) +  labs(x= paste("Year (1995-2001) in", C) , y=("Growth Assortativity")) +   geom_point(aes(Year, obscor), size =3) +
    annotate("text", x=1999, y=-0.1, label= "Random Network", size =7) +
    annotate("text", x=1999, y=0.6, label= "WIOD", size = 7) +
    geom_line(aes(Year, bstp_estimate), size=1) + geom_ribbon(aes(x=Year, ymin=ci.lwr,ymax=ci.upr),alpha=0.1) +  theme(text = element_text(size = 20))
}

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



