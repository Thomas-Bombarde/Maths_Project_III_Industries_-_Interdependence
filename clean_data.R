
install.packages("readxl")
install.packages("dplyr")
install.packages("imputeTS")
library(stringr)
library(readxl)
library(dplyr)

#next step: take average 1-year growth rates over ten years

#------------------------- Load Data from Mcnerney (2018)'s repository
wiot95 <- 
  read_excel("~/2023 Math Project III/growth-productionchains-main/data/WIOD/WIOT95_ROW_Apr12.xlsx")
wiot96 <- 
  read_excel("~/2023 Math Project III/growth-productionchains-main/data/WIOD/WIOT96_ROW_Apr12.xlsx")
wiot97 <- 
  read_excel("~/2023 Math Project III/growth-productionchains-main/data/WIOD/WIOT97_ROW_Apr12.xlsx")
wiot98 <- 
  read_excel("~/2023 Math Project III/growth-productionchains-main/data/WIOD/WIOT98_ROW_Apr12.xlsx")
wiot99 <- 
  read_excel("~/2023 Math Project III/growth-productionchains-main/data/WIOD/WIOT99_ROW_Apr12.xlsx")
wiot00 <- 
  read_excel("~/2023 Math Project III/growth-productionchains-main/data/WIOD/WIOT00_ROW_Apr12.xlsx")
wiot01 <- 
  read_excel("~/2023 Math Project III/growth-productionchains-main/data/WIOD/WIOT01_ROW_Apr12.xlsx")
wiot02 <- 
  read_excel("~/2023 Math Project III/growth-productionchains-main/data/WIOD/WIOT02_ROW_Apr12.xlsx")
wiot03 <- 
  read_excel("~/2023 Math Project III/growth-productionchains-main/data/WIOD/WIOT03_ROW_Apr12.xlsx")
wiot04 <- 
  read_excel("~/2023 Math Project III/growth-productionchains-main/data/WIOD/WIOT04_ROW_Apr12.xlsx")
wiot05 <- 
  read_excel("~/2023 Math Project III/growth-productionchains-main/data/WIOD/WIOT05_ROW_Apr12.xlsx")
wiot06 <- 
  read_excel("~/2023 Math Project III/growth-productionchains-main/data/WIOD/WIOT06_ROW_Apr12.xlsx")
wiot07 <- 
  read_excel("~/2023 Math Project III/growth-productionchains-main/data/WIOD/WIOT07_ROW_Apr12.xlsx")
wiot08 <- 
  read_excel("~/2023 Math Project III/growth-productionchains-main/data/WIOD/WIOT08_ROW_Sep12.xlsx")
wiot09 <- 
  read_excel("~/2023 Math Project III/growth-productionchains-main/data/WIOD/WIOT09_ROW_Sep12.xlsx")
wiot10 <- 
  read_excel("~/2023 Math Project III/growth-productionchains-main/data/WIOD/WIOT10_ROW_Sep12.xlsx")
wiot11 <- 
  read_excel("~/2023 Math Project III/growth-productionchains-main/data/WIOD/WIOT11_ROW_Sep12.xlsx")


#------------------------- Store Data
periods = list(wiot95 = wiot95,
               wiot96 = wiot96,
               wiot97 = wiot97,
               wiot98 = wiot98,
               wiot99 = wiot99,
               wiot00 = wiot00,
               wiot01 = wiot01,
               wiot02 = wiot02,
               wiot03 = wiot03,
               wiot04 = wiot04,
               wiot05 = wiot05,
               wiot06 = wiot06,
               wiot07 = wiot07,
               wiot08 = wiot08,
               wiot09 = wiot09,
               wiot10 = wiot10,
               wiot11 = wiot11)


#------------------------- Tidy Data
clean.data <- function(wiot){
  for (i in (1:ncol(wiot))){
    wiot[1,i] <- str_c(wiot[4,i],wiot[5,i])
  }
  wiot[1,1] = "Industry Code"
  wiot[1,2] = "Industry"
  wiot[1,3] = "Country"
  wiot[1,4] = "row code"
  wiot[1,ncol(wiot)] = "Total Output"
  names(wiot) <- wiot[1,]
  
  return(wiot)
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

max_obs <- length(periods)
span <- 10
for (i in (1:max_obs)){
  wiot <- periods[[i]]
  periods[[i]] <- clean.data(wiot)
}

#------------------------- Save Data
setwd("~/2023 Math Project III/output")
saveRDS(periods, file = "periods.Rds")

#------------------------- Clean Environment
exsubset <- matrix(ls()) |>
  t() |> as.data.frame()
names(exsubset) <- exsubset[1,]
exsubset <- exsubset |> select(contains("wiot"))
rm(list=names(exsubset))

  