

# This code is essentially the same as compute_ANNG, but we take
# A[j,i] instead of A[i,j]
setwd("~\\2023 Math Project III\\output")

install.packages("readxl")
install.packages("dplyr")
install.packages("tidyr")
install.packages("imputeTS")

#------------------------- Libraries
library(dplyr)
library(tidyr)
library("readxl")
#library(igraph) 
library(imputeTS, include.only = 'na.replace') 

#------------------------- Data

periods <- readRDS(
  "~\\2023 Math Project III\\output\\periods.Rds")

sea <- read_excel(
  "~\\2023 Math Project III\\input\\Socio_Economic_Accounts_July14.xlsx", 
  sheet = "DATA")

#------------------------- Global Variables
n <- 35 # number of industries
inputs <- periods[[1]] |> filter(Industry!="Industry")|>
  select(Industry) |> 
  unique() |>  
  na.omit()
inputs <- inputs[(1:n),] |> as.matrix() |> as.vector()
all_inputs <- c(inputs, "Labour")
countries <- periods[[1]] |> filter(Country!="Country")|>
  select(Country) |> 
  unique() |> 
  filter(Country !="TOT") |> 
  na.omit()
countries <- countries |> filter(Country != "ROM") |> filter(Country != "RoW")


#------------------------- Functions
ttoutput <- 
  function(periods, C, j){
    "Compute the total output (including household consumption) table for a
    country C in period j"
    tt_output <- periods[[j]] |>
      filter(Country == C) |>
      select("Industry", "Total Output") |> 
      mutate(across(!Industry, as.numeric))
    tmp1 <-  sea |>
      filter(Country==C) |> 
      filter(Variable=="GO" & Code!= "TOT")|> 
      select(4+j) |> as.matrix() |> as.numeric()
    tmp2 <-  sea |>
      filter(Country==C) |> 
      filter(Variable=="II" & Code!= "TOT")|> 
      select(4+j) |> as.matrix() |> as.numeric()
    cons <- data.frame("Industry"="Labour", "Total Output"=sum(tmp1)-sum(tmp2))
    names(cons) <- names(tt_output)
    tt_output <- rbind(tt_output,cons)
    rm(tmp1, tmp2, cons)
    return(tt_output)
  }
niot <- 
  function(periods, C, j){
    "Compute the national input/output matrix including households"
    # total flow of money from industries to households
    lab <- sea |>
      filter(Country==C) |> 
      filter(Variable=="LAB" & Code!= "TOT") |> 
      select(4+j)
    # compute flow of money from households to industries
    # as the residual of total industry output and industry output to 
    # other industries
    tmp1 <-  sea |>
      filter(Country==C) |> 
      filter(Variable=="GO" & Code!= "TOT")|> 
      select(4+j) |> as.matrix() |> as.numeric()
    tmp2 <-  sea |>
      filter(Country==C) |> 
      filter(Variable=="II" & Code!= "TOT")|> 
      select(4+j) |> as.matrix() |> as.numeric()
    cons <- t(tmp1-tmp2)
    cons <- cbind(cons,0)
    rm(tmp1, tmp2)
    X1 <- periods[[j]] |> 
      filter(Country == C) |> 
      select(contains(C)) |>
      select(ends_with("1"):ends_with("35")) |> 
      mutate_all(as.numeric)
    X1 <- cbind(X1, lab)
    cons <- cons |> as.data.frame()
    names(cons) <- names(X1)
    X1 <- rbind(X1, cons) |> sapply(as.numeric)
    X1[is.na(X1)] <- 0
    print(C)
    return(X1)
  }
adjmat_xtra <- 
  function(X1){
    "Compute the matrix with weights defined by the share of all 
    extra-industry outputs"
    A <- X1
    for (i in (1:nrow(A))){
      if(sum(X1[i,]) - X1[i,i] != 0){
        A[i,] <- X1[i,]/(sum(X1[i,]) - X1[i,i])
      }
      else{
        A[i,] <- c(rep(0,nrow(A)))
      }
    }
    A <- as.matrix(A)
  }
prodimprov <- 
  function(X1, X2, A, tt_output1, tt_output2){
    "Compute productivity improvement between output tables X1 and X2
    given a static adjacency matrix A"
    
    TT1 <- select(tt_output2, !Industry) |> as.matrix()
    TT2 <- select(tt_output1, !Industry) |> as.matrix()
    input_diff <- X2-X1
    input_diff <- sweep(input_diff, 2, TT1, "/")
    input_diff[is.na(input_diff)] <- 0 #fix NaN
    input_diff <- diag(input_diff%*%A)
    
    output_diff <- cbind(tt_output1$Industry, 
                         select(tt_output2, !Industry) - 
                           select(tt_output1, !Industry))
    output_diffhat <- output_diff$`Total Output`/tt_output1$`Total Output`
    output_diffhat[is.na(output_diffhat)] <- 0
    
    prodimprov <- output_diffhat - input_diff
    return(prodimprov)
  }
pred_innov <- function(ctry_innov, A, k, C, n){
  "Compute predicted innovations for a given country k based on actual 
  innovation rates"
  pred <- rep(0,n)
  Abis <- A - diag(diag(A))
  tmp <- 0
  for (i in (1:n)){
    # for a given industry i
    for (j in (1:n)){
      # compute the weighted (by A) productivity improvements of all its industries j
      if(is.infinite(ctry_innov[j,k])!=TRUE & is.na(ctry_innov[j,k])!=TRUE){
        tmp <- tmp + as.numeric(Abis[i,j])*ctry_innov[j,k] #problem with Inf
      }
    }
    pred[i] <- tmp
  }
  return(pred)
}
compute_ctry_innov <- 
  function(periods, countries, niot, adjmat, ttoutput, inputs, t, span, n){
    "compute 'span'-yr innovation rate for period i for every industry in
    a given country"
    ctry_innov <- matrix(nrow=n+1, ncol=nrow(countries)) |> as.data.frame()
    for (j in (1:nrow(countries))){
      C = countries[j,1] |> as.character()
      tt_output1 <- ttoutput(periods, C, t)
      tt_output2 <- ttoutput(periods, C, t+span)
      X1 <- niot(periods, C, t)
      X2 <- niot(periods, C, (t+span))
      A <- adjmat(X1)
      gamma <- prodimprov(X1, X2, A, tt_output1, tt_output2) |> as.vector()
      ctry_innov[,j] <- gamma
    }
    names(ctry_innov) <- countries[,1] |> as.matrix() |> as.vector()
    return(ctry_innov[(1:35),])
  }
compute_ctry_pred <- 
  function(pred_innov, ctry_innov, A, n, t){ 
    "compute 'span'-yr average nearest neighbour innovation rate for period i
    for every industry in a given country"
    ctry_pred_innov <- matrix(nrow=n, ncol=nrow(countries)) |> as.data.frame()
    for (j in (1:nrow(countries))){
      C <-  countries[j,1] |> as.character()
      X1 <- niot(periods, C, t)
      A <- adjmat(X1)
      gamma <- pred_innov(ctry_innov, A, j, C, n)
      ctry_pred_innov[,j] <- gamma
    }
    names(ctry_pred_innov) <- countries[,1] |> as.matrix() |> as.vector()
    return(ctry_pred_innov[(1:35),])
  }

# ANNG
# Generate WIOD tables of 10-year productivity improvement over periods
# using adjmat_xtra
span <- 10
tables_innov <- list()
tables_pred_innov <- list()
for (t in (1:(length(periods)-span))){
  ctry_innov <- compute_ctry_innov(periods, countries, niot, adjmat_xtra, ttoutput, inputs, t, span = span, n)
  ctry_pred_innov <- compute_ctry_pred(pred_innov, ctry_innov, A, n, t)
  #get rid of columns with missing data
  ctry_innov <- ctry_innov[ , colSums(is.na(ctry_innov)) == 0] 
  ctry_pred_innov <- ctry_pred_innov[ , colSums(is.na(ctry_innov)) == 0]
  len <- length(tables_innov)
  tables_innov[[len+1]] <- ctry_innov
  tables_pred_innov[[len+1]] <- ctry_pred_innov
}
setwd("~\\2023 Math Project III\\output")
saveRDS(tables_innov, file = "tables_innov_xtra_in.Rds")
saveRDS(tables_pred_innov, file = "tables_pred_innov_xtra_in.Rds")




# Combine observations
tables_innov <- readRDS(
  "~\\2023 Math Project III\\output\\tables_innov_xtra_in.Rds")
tables_pred <- readRDS(
  "~\\2023 Math Project III\\output\\tables_pred_innov_xtra_in.Rds")
y <- 1995+10
big_innov_table <- cbind(inputs, tables_innov[[1]]) |>
  as.data.frame() |>
  pivot_longer(cols = !inputs, names_to = "Country", values_to = "innov")
Year <- rep(y, nrow(big_innov_table))
big_innov_table <- cbind(big_innov_table, Year)
big_pred_table <- cbind(inputs, tables_pred[[1]]) |>
  as.data.frame() |>
  pivot_longer(cols = !inputs, names_to = "Country", values_to = "innov")
Year <- rep(y, nrow(big_pred_table))
big_pred_table <- cbind(big_pred_table, Year)# set up tables of all observations

for (i in (2:length(tables_innov))){
  df1 <- cbind(inputs, tables_innov[[i]]) |>
    as.data.frame() |>
    pivot_longer(
      cols = !inputs, names_to = "Country", values_to = "innov")
  Year <- rep(y+i-1, nrow(df1))
  df1 <- cbind(df1, Year)
  big_innov_table <- rbind(big_innov_table, df1)
  
  df1 <- cbind(inputs, tables_pred[[i]]) |>
    as.data.frame() |>
    pivot_longer(cols = !inputs, names_to = "Country", values_to = "innov")
  Year <- rep(y+i-1, nrow(df1))
  df1 <- cbind(df1, Year)
  big_pred_table <- rbind(big_pred_table, df1)
}

df <- inner_join(big_innov_table, big_pred_table,
                 by = c("inputs", "Country", "Year"),
                 suffix = c(".obs", ".pred")) #table of observed innov and ANNG
# Add sector of each industry
wiot <- periods[[i]]
codes <- wiot |> select(`Industry Code`, Industry)
codes <- codes[(6:40),]
sector <- c("Agriculture", rep("Industry", 16), rep("Services", 35-17))
sector <- cbind(sector, codes)
df <- left_join(df, sector, by = c("inputs"="Industry"))
setwd("~\\2023 Math Project III\\output")
saveRDS(df, file = "ANNGandObs_in.Rds")


#diffr("~/2023 Math Project III/code/R/compute_ANNG.R", "~/3H_DSN_Bombarde/compute_prod.R")
