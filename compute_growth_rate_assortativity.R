library(tidyverse)
library(readxl)


setwd("C:\\Users\\TomBo\\Documents\\2023\\2023 Math Project III\\output")

# Data
# World IO tables
periods <- readRDS(
  "C:\\Users\\TomBo\\Documents\\2023\\2023 Math Project III\\output\\periods.Rds")
# Socio-Economic Account
sea <- read_excel(
  "C:\\Users\\TomBo\\Documents\\2023\\2023 Math Project III\\input\\Socio_Economic_Accounts_July14.xlsx", 
  sheet = "DATA")
# Exchange rates to US dollar fo revery indvustry in every country
er <- read_excel("~/2023/2023 Math Project III/input/Exchange_Rates(1).xlsx", sheet = "EXR")
lab_nat_curr <- sea |> filter(Variable == "LAB")|>
  filter(Code != "TOT")
years <- (1996:(1996+13))

# Deprecate code: grab productivity improvements from reviewed article
  # productivity improvements using the code provided by Mcnerneny et al 2019
  # and setting `-Inf` price changes in rvec to 0
  #gamma_char <- read.csv("~/2023/2023 Math Project III/input/gamma_estimates.csv", header=FALSE)
  #extract_real <- function(s) {
  #  "Extract the substring before the first whitespace character"
  #  idx <- regexpr("\\s", s)
  #  result <- substr(s, 1, idx - 1)
  #  return(result)
  #}
  #gamma <- gamma_char |> mutate_all(extract_real) |> 
  #  mutate_all(as.numeric)  # dropped 2010, 2011 due to missing values
  #gamma[is.na(gamma)] <- 0 # replace NA values with 0 for now, as these
  # arise from Persons and Households with 0 prof improv
  #colMeans(gamma)
  #names(gamma) <- years
  #gamma <- as.data.frame((gamma))
  #rm(gamma_char)

# 0. wrangle data
# Compute price changes across 1995-2009
  # extract price changes 
    # price level with 1995=100
    p_nom <- sea |> filter(Variable == "GO_P")|>
      filter(Code != "TOT") |>
      select(-c("_2010", "_2011")) |> # drop 2010, 2011, missing data
      mutate(across(!c(`Country`,
                       `Variable`,
                       `Description`,
                       `Code`), as.numeric)) 
    # compute wage rate 
    # total labor income earned in national currency
    L_income <- sea |> filter(Variable == "LAB") |>
      filter(Code != "TOT") |>
      select(-c("_2010", "_2011")) |> # drop 2010, 2011, missing data
      mutate(across(!c(`Country`,
                       `Variable`,
                       `Description`,
                       `Code`), as.numeric)) 
    hourspp <- sea |> filter(Variable == "EMP") # hours worked per person engaged
    npe <- sea |> filter(Variable == "H_EMP") # number of persons engaged
    hourspp <- hourspp |>
      filter(Code != "TOT") |> # drop values for total industries
      select(-c("_2010", "_2011")) |> # drop 2010, 2011, missing data
      mutate(across(!c(`Country`,
                       `Variable`,
                       `Description`,
                       `Code`), as.numeric)) # convert hours to numeric
    
    npe <- npe |>  filter(Code != "TOT") |> # drop values for total industries
      select(-c("_2010", "_2011")) |> # drop 2010, 2011, missing data
      mutate(across(!c(`Country`,
                       `Variable`,
                       `Description`,
                       `Code`), as.numeric)) 
  
  # drop NA values in SEA persons data
    index <- which(is.na(npe), arr.ind=TRUE)
    npe_mod <- npe[-index[,1],]
    hourspp_mod <- hourspp |> slice(-(index[,1]))
    L_income_mod <- L_income |> slice(-(index[,1]))
    p_nom_mod <- p_nom |>  slice(-(index[,1])) |> select(-(1:4))
    is.na(p_nom_mod) |> sum()
    is.na(L_income_mod) |> sum()
    is.na(hourspp_mod) |> sum()
    
  #drop NA values for price changes
    index2 <- which(is.na(p_nom_mod), arr.ind=TRUE)
    npe_mod <- npe_mod[-index2[,1],]
    hourspp_mod <- hourspp_mod |> slice(-(index2[,1]))
    L_income_mod <- L_income_mod |> slice(-(index2[,1]))
    p_nom_mod <- p_nom_mod |>  slice(-(index2[,1]))
  
  # Compute wages as total labour income / number hours worked per person * hours per person
    wages_mod <- L_income_mod[,-(1:4)]/npe_mod[,-(1:4)]*hourspp_mod[,-(1:4)]
    wages_mod[is.na(wages_mod)] <- 0
  
  # compute wage inflation  
    rho_mod <- log(wages_mod[,-1]) - log(wages_mod[,-ncol(wages_mod)])
  
  # compute price level change
    r_nom_mod <- log(p_nom_mod[,-1]) - log(p_nom_mod[,-ncol(wages_mod)])
  
  # compute real price change  
    r_mod <- (r_nom_mod - rho_mod)
    r_mod <- r_mod |> mutate_all(as.numeric) |> as.matrix()
    #r_mod <- cbind(L_income_mod[,(1:4)], r_nom - rho_mod)
    # NaNs arise from log(0)
    r_mod[is.na(r_mod)] <- 0
    r_mod[is.infinite(r_mod)] <- 0
  

  anng_yrs <- list()
  OM_yrs <- list()
  
# prepare dataframe for observations
  wiot_yr <- periods[[1]]
  names_count <- unique(wiot_yr$Country)[(3:42)]
  names_indus <- unique(wiot_yr$Industry)[(3:37)]
  countries <- rep(unlist(lapply(names_count, function(x) rep(x, 35))), each = 1) |> 
    as.data.frame()
  industries <- rep(names_indus, 40)|> 
    as.data.frame()
  
  # exclude missing values
  countries_mod <- countries |> slice(-index[,1]) |> slice(-index2[,1])
  industries_mod <- industries |> slice(-index[,1]) |> slice(-index2[,1])
  
  df_mod <- cbind(countries_mod, industries_mod)
  tmp1 <- cbind(c(), c())
  
  n_mod <- nrow(df_mod)
  
  
# compute average nearest neighbour productivity improvements 
# and output multipliers
for (yr in (1:(length(years)))){
  year_mod <- data.frame("year" = rep(1996 + (yr-1), n_mod))
  
  # grab data
  sea_yr <- sea |> select(1:4, 4+yr)
  wiot_yr <- periods[[yr]]
  
  # (differing from the report, we know use notation as in Mcenrney et al 2019)
  # compute column sums, total expenditures of industry j, "M_j" in Mcnerney
  
  # check that r60 = sum of all II for 1 example
    #exp_agri_inter <- wiot_yr[(6:1440),] |> select("AUSc1") |> as.matrix() |> as.numeric()
    #sum(exp_agri_inter)
    #wiot_yr |> filter(`row code`== "r60") |> select("AUSc1")
    # which is correct
    #rm(exp_agri_inter)
  
  # compute total expenditure (hence revenue by model assumptions)
    exp_ii <- wiot_yr |> filter(`row code`== "r60") |>
      as.data.frame() |>
      mutate(across(!c(`Industry Code`,
                       `Industry`,
                       `Country`,
                       `row code`), as.numeric)) # expenditure on industry inputs
    
    # taking all factor payements
    #exp_hh <- wiot_yr |> filter(`row code`== "r64") |> 
    #  as.data.frame() |>
    #  mutate(across(!c(`Industry Code`,
    #                   `Industry`,
    #                   `Country`,
    #                   `row code`), as.numeric)) # expenditure on households
    
    # taking only labour income as in mcnerneyr, converting to US dollars
    # get exchange rate vector for all industries in countries
    er_yr <- er|> select(3) |> slice((4:44))
    er_yr <- rep(unlist(lapply(er_yr, function(x) rep(x, 35))), each = 1) |> 
      as.numeric() 
    lab_nat_yr <- lab_nat_curr[,4+yr] |> mutate_all(as.numeric) |> as.matrix()
    lab_nat_yr[is.na(lab_nat_yr)] <- 0 #NAs come from China's `Persons and Private households, which I will
    # assume labour compensation is 0 as in other countries`
    exp_hh <- lab_nat_yr*er_yr
    
    # Compute total exp: ii+hh
    exp_tot <- exp_hh + exp_ii[,-(1:4)]
    exp_tot <- exp_tot[,(1:1400)] |> as.matrix() |> as.numeric()
    #exclude RoW and keep only industry-country stats
    
  # divide all industry expenditures by total expenditures
    # get matrix of expenditures with coef Mij: expenditure of j on i 
    # (excluding RoW)
    M <- wiot_yr[(6:1405),(5:1404)] |>
      as.data.frame() |> 
      mutate(across(.cols = everything(), as.numeric)) |> 
      as.matrix() 
    M_mod <- M |> as.data.frame() |> slice(-index[,1]) |> select(-index[,1]) |> 
      slice(-index2[,1]) |> select(-index2[,1]) |> 
      as.matrix()
    # divide columns by total expenditure of industries
    A <- t(t(M)/exp_tot) |> as.data.frame()
    # NaN values appear when dividing numerator 0, replace with 0.
    A[is.na(A)] <- 0
    A_mod <- A |> slice(-index[,1]) |> select(-index[,1]) |> 
      slice(-index2[,1]) |> select(-index2[,1])
    #A <- A |> as.matrix()
    A_mod <- A_mod |> as.matrix()
    
# compute output multiplier:
    I <- diag(n_mod) 
    leontief <- I - t(A_mod)
    ncol(A_mod)
    det(leontief)
    leontief_inverse <- solve(leontief)
    OM <- leontief_inverse %*% c(rep(1, n_mod))
# compute productivity improvement
    gamma <- -leontief%*%r_mod[,yr]
# compute ANNG
    delta <- diag(diag(M_mod)) 
    M_anng <- M_mod - delta
    # taking in (inputs) degree
    D_in <- colSums(M_anng) |> as.numeric()
    A_anng_in <- t(t(M_anng)/D_in)
    A_anng_in [is.na(A_anng_in)] <- 0
    anng_in <- t(A_anng_in) %*% gamma

  # taking out (output) degree
    D_out <- rowSums(M_anng) |> as.numeric()
    A_anng_out <- M_anng/D_out
    A_anng_out [is.na(A_anng_out)] <- 0
    anng_out <- A_anng_out %*% as.numeric(gamma)
    
    new <- cbind(year_mod, anng_in)
    new <- cbind(new, anng_out)
    new <- cbind(new, gamma)
    new <- cbind(new, OM)
    tmp1 <- rbind(tmp1, new)
}  

# bring observations into one dataframe
  df_mod <- cbind(df_mod, tmp1)
  names(df_mod) <- c("Country", "Industry", "Year", "ANNG_in", "ANNG_out", "Gamma", "Output Multiplier")
  cor(df_mod$Gamma, df_mod$`Output Multiplier`)
  df_mod <- df_mod |>
    mutate(Gamma=as.numeric(Gamma),
           ANNG_in=as.numeric(ANNG_in),
           ANNG_out=as.numeric(ANNG_out),
           Year = as.factor(Year)
  
  
# 1. Look at assortativity by industry

# 1.1. for ANNG-output (inputs, forward linkages)
  indusreg <- data.frame(names_indus, 
                         "R2" = rep(0,35),
                         "estimate" = rep(0,35),
                         "ttest" =  rep(0,35))
  for (i in (1:35)){
    tmp <- df_mod |> filter(Industry == names_indus[i])
    fit <- lm(tmp$Gamma ~ tmp$ANNG_in)
    results <- summary(fit)
    indusreg[i,(2:4)] <- c(results$r.squared, results$coefficients[2,1], results$coefficients[2,4])
    print(results)
  }
  indusreg <- arrange(indusreg, desc(R2))
  names(indusreg) <- c("Industry", "R-squared", "ANNG_in-Coefficient", "Pr(>|t|)")
  
  
# 1.2. for ANNG-output (outputs, forward linkages)
  indusreg_out <- data.frame(names_indus,
                         "R2" = rep(0,35),
                         "estimate (ANNG-out)" = rep(0,35),
                         "ttest (ANNG-out)" =  rep(0,35))
  for (i in (1:35)){
    tmp <- df_mod |> filter(Industry == names_indus[i])
    fit <- lm(tmp$Gamma ~ tmp$ANNG_out)
    results <- summary(fit)
    indusreg_out[i,(2:4)] <- c(results$r.squared, results$coefficients[2,1], results$coefficients[2,4])
    print(results)
  }
  indusreg_out <- arrange(indusreg_out, desc(R2))
  names(indusreg_out) <- c("Industry", "R-squared", "ANNG_out-Coefficient", "Pr(>|t|)")
  

# 2. look at assortatitivity by country
# 2.1. collect stats
  countryreg <- data.frame(names_count, 
                         "R2" = rep(0,40),
                         "estimate" = rep(0,40),
                         "ttest" =  rep(0,40))
  for (i in (1:40)){
    tmp <- df_mod |> filter(Country == names_count[i])
    fit <- lm(tmp$Gamma ~ tmp$ANNG_in)
    results <- summary(fit)
    countryreg[i,(2:4)] <- c(results$r.squared, results$coefficients[2,1], results$coefficients[2,4])
    print(results)
  }
  countryreg <- arrange(countryreg, desc(R2))
  names(countryreg) <- c("Country", "R-squared", "ANNG_in-Coefficient", "Pr(>|t|)")

# 2.2 eliminate outliers
  vars <- df_mod[c("ANNG_in" , "Gamma")]
  vars.centre <- colMeans(vars)
  vars.cov <- cov(vars)
  # (The following method was taken from : 
  # https://towardsdatascience.com/mahalonobis-distance-and-outlier-detection-in-r-cb9c37576d7d)
  # To visualise outliers, we can plot the the ellipse with radius the threshold of
  # mahalanobis distance threshold we would like to cut off. Ellipse radius from 
  # Chi-Sqaure distrubiton:
  rad <- qchisq(p = 0.999999999999 , df = ncol(vars)) |> sqrt()
  # Finding ellipse coordiantes
  ellipse <- car::ellipse(center = vars.centre , shape = vars.cov , radius = rad ,
                          segments = 150 , draw = FALSE) |> 
    as.data.frame()
  colnames(ellipse) <- colnames(vars)
  # Create scatter Plot
  par(mfrow=c(2,1))
  figure.mahalanobis <- ggplot(vars , aes(x = Gamma , y = ANNG_in)) +
    geom_point(size = 2) +
    geom_polygon(data = ellipse , fill = "orange" , color = "orange" , alpha = 0.5)+
    geom_point(aes(vars.centre[1] , vars.centre[2]) , size = 5 , color = "blue") +
    geom_text(aes(label = row.names(vars)) , hjust = 1 , vjust = -1.5 ,size = 2.5 ) + 
    labs(x = "Average nearest neighbour 10-year growth rate",
         y = "10-year Industry growth rate")
  # Finding distances
  distances <- mahalanobis(x = vars , center = vars.centre , cov = vars.cov)
  # Cutoff value for ditances from Chi-Sqaure Dist. 
  # with p = 0.99999 df = 2 which in ncol(air)
  cutoff <- qchisq(p = 0.99999, df = ncol(vars))
  # drop observations whose distance is greater than cutoff value
  outliers <- vars[distances > cutoff ,]
  drop <- rownames(outliers) |> as.numeric()
  df <- slice(df_mod, -drop)
  
# 2.3. look at plots by country
  dev.off()
  
  x.lab = "10-year industry productivity improvement (Textiles and Textile Products, 1995-2001)"
  y.lab = "10-year neighborhood industries \n productivity improvement (Textiles and Textile Products, 1995-2001)"
  tmp <- df |> filter(Country == "KOR")
  
  
  ggplot(tmp, aes(Gamma, ANNG_in)) + labs(x=x.lab, y=y.lab) +   geom_point() +
    theme(text = element_text(size = 20)) +
    stat_smooth(data = tmp,
                method = "lm",
                se = TRUE,
                formula = y ~ x,
                geom = "smooth", col="black")   
