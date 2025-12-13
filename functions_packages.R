
### VIENNA FUNCTIONS 


## install and load necessary packages 
packages = c("dplyr", "lubridate", "tidyr", "ggplot2", "tsibble", "fpp3", "openxlsx", "EnvStats", 
             "gridExtra")

to_install = packages[!packages %in% installed.packages()[,"Package"]]
if (length(to_install) > 0) install.packages(to_install)

lapply(packages, library, character.only = T)


### Function to compute centered moving average window n

###########################################################
## Function to compute centered moving average ############
## with window n ##########################################
###########################################################
ma <- function(x, n) {
  
  result <- stats::filter(x, rep(1/n, n), sides = 2)
  
  return(as.numeric(result))
}

###########################################################
## Function to compute net present value ##################
## and discount rate given cashflows (cf) #################
###########################################################
npv = function(cf, k){
  
  t = 0:(length(cf)-1)
  
  disc.flow = cf/(1+k)^t
  
  npv = sum(disc.flow)
  
  return(npv)
}

###########################################################
## Function to compute internal rate ######################
## of return given cashflows (cf) #########################
###########################################################
irr = function(cf, initial.guess = 0.1, tol = 1e-6, max.iter = 1000) {
  

  irr <- initial.guess
  
  
  for (i in 1:max.iter) {
    
    npv.iter <- npv(cf, irr)
    
    npv.derivative <- sum(-cf*(0:(length(cf)-1))/(1+irr)^(0:(length(cf)- 1)+1))
    
    if(npv.derivative==0){
      return(NA)
    }
    
    irr.new <- irr-npv.iter/npv.derivative
    
    if (abs(irr.new-irr) < tol) {
      return(irr.new)
    }
    
    irr <- irr.new
  }
  
  return(NA)
}



## Variance-based sensitivity analysis

###########################################################
## Variance-based sensitivity analysis ####################
###########################################################
###########################################################

sens.analysis = function(A, B, gen.Y.fun){
  
  ## A: matrix with N observations, and k columns, with values of input parameters
  ## B: matrix with N observations, and k columns, with values of input parameters
  ## A and B independent matrix with same data generation process 
  ## gen.Y.fun: function to generate output Y returning a scalar
  #   
  # eg
  #   
  #   gen.y = function(vec){
  #     npv(compute_cf(c0=vec[1], revenues.t1=vec[2], revenues.grate=vec[3], 
  #                    var.costs.t1=vec[4], var.costs.grate=vec[5]), k=vec[6])
  #   }
  
  N = nrow(A)
  k = ncol(A)
  params.name = colnames(A)
  
  AB_list <- lapply(1:k, function(i){
    AB <- A
    AB[, i] <- B[, i]
    AB
  })
  names(AB_list) <- paste0("AB", 1:k)
  
  # Evaluate all Y
  Y_A = apply(A, 1, gen.Y.fun)
  Y_B = apply(B, 1, gen.Y.fun)
  
  Y_AB <- lapply(AB_list, function(AB) apply(AB, 1, gen.Y.fun))
  names(Y_AB) <- paste0("AB", 1:k)
  
  ### Compute total variance of model outputs
  f0 <- 1 / (2 * N) * sum(Y_A + Y_B)
  VY <- 1 / (2 * N) * sum((Y_A - f0)^2 + (Y_B - f0)^2)
  
  ## Compute first order indices (jansen)
  Vi <- sapply(1:k, function(i) (VY - 1 / (2 * N) * sum((Y_B - Y_AB[[i]])^2)))
  Si.jansen <- Vi[1:length(params.name)] / VY
  Si.saltelli <- sapply(1:k, function(i) sum(Y_B * (Y_AB[[i]] - Y_A)) / (N * VY))
  
  ### Compute total-order indices 
  
  ### total (jansen)
  Ti.jansen <- sapply(1:k, function(i) (1 / (2 * N) * sum((Y_A - Y_AB[[i]])^2)) / VY)
  Ti.sobol <- sapply(1:k, function(i) ((1 / N) *sum(Y_A * (Y_A - Y_AB[[i]]))) / VY)
  
  
  ### correlation between input and output
  all.mat = rbind(A, B)
  Y.mat = c(Y_A, Y_B)
  
  
  cor.x = sapply(1:ncol(all.mat), function(i) cor(all.mat[,i], Y.mat))
  
  
  ### export
  
  res.ls = data.frame(input = params.name, 
                      sensitivity = Ti.sobol)

  return(res.ls)
}
