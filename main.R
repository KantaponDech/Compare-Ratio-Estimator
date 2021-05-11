start.times <- Sys.time() ## Starting Time
## Local and package setting
# library(moments)
# library(MethylCapSig)
# library(DescTools)
# library(ggplot2)

source("C:/Users/student/Desktop/Final Project/import.R")
setwd("C:/Users/student/Desktop/Final Project")

## Property Setting
N = 10000
n_ <- c(10, 30, 100, 400)
skewed_ <- c(1, 3, 5, 7)
index <- c(1:N)

## Main
for(skewed in skewed_){ ## Loop by skewness of Y
     ## Range of skewness setting
     L_skew <- skewed
     R_skew <- skewed + 0.5
     ## End of Range of skewness setting
     
     ## Simulate Population data
     ranY <- 0.1
     repeat{
          rr <- runif(1, 0.8, 1)
          DataSet <- mvlognormal(n = N, Mu = c(7, 70000),
                             Sigma = c(15, 5 ^ ranY),
                             R = matrix(c(1,rr,rr,1), ncol = 2))
          X <- DataSet[,1]
          Y <- DataSet[,2]
          
          if(!is.na(skewness(Y)) & cor(X,Y) >= 0.8 & cor(X,Y) <= 0.85
             & skewness(Y) >= L_skew & skewness(Y) < R_skew)
          {
                setwd("C:/Users/student/Desktop/Final Project/Plot")
                plot_dataset()
                setwd("C:/Users/student/Desktop/Final Project")
                break
          }
          
          if(ranY >= 50){
               ranY <- 0.1
               next
          }
          ranY <- ranY + 0.01
     }
     rm(ranY)
     ## End of Simulate Population data
     
     ## Population Detail
     data_detail <- rbind(median(X), median(Y), mean(X), mean(Y),
                          sd(X), sd(Y), kurtosis(X), kurtosis(Y),
                          skewness(X), skewness(Y), cor(X,Y))
     
     row.names(data_detail) <- c("Median X", "Median Y", "Mean X", "Mean Y",
                                 "SD X", "SD Y", "Kurtosis X", "Kurtosis Y",
                                 "Skewness X", "Skewness Y", "Cor")
     
     write.table(data_detail, sep = ",", append = T,
                 file = "C:/Users/student/Desktop/Final Project/Detail.csv")
     ## End of Population Detail
     
     ## Sampling and Estimation
     for(sampleSize in n_)## Loop by sample size
     {
         store <- numeric()
         store_Trad_Est    <- numeric()
         store_SisoDwive   <- numeric()
         store_SinghKa     <- numeric()
         store_UpaSingh    <- numeric()
         store_SinghTailor <- numeric()
         store_YanTian     <- numeric()
         store_SubKum      <- numeric()
         store_JerKis      <- numeric()
         store_MuiliAudu   <- numeric()
         
         for (i in c(1:2000))
         {
            x <- numeric()
            y <- numeric()
            
            r <- sample(index, sampleSize, FALSE)
            x <- X[r]
            y <- Y[r]
            
            cv.X <- sd(x) / mean(x)
            cv.Y <- sd(y) / mean(y)
            alpha <- (cor(x,y) * cv.Y) / ((mean(X) / (mean(X) + sampleSize)) * cv.X)
            
            store_Trad_Est    <- append(store_Trad_Est,    Trad_Est())
            store_SisoDwive   <- append(store_SisoDwive,   SisoDwive())
            store_SinghKa     <- append(store_SinghKa,     SinghKa())
            store_UpaSingh    <- append(store_UpaSingh,    UpaSingh())
            store_SinghTailor <- append(store_SinghTailor, SinghTailor())
            store_YanTian     <- append(store_YanTian,     YanTian())
            store_SubKum      <- append(store_SubKum,      SubKum())
            store_JerKis      <- append(store_JerKis,      JerKis())
            store_MuiliAudu   <- append(store_MuiliAudu,   MuiliAudu())
         }
         rm(x,y,r,i)
         
         ## Setting output
         ## List of estimated value
         store <- list(store_Trad_Est, store_SisoDwive, store_SinghKa,
                  store_UpaSingh, store_SinghTailor, store_YanTian,
                  store_SubKum, store_JerKis, store_MuiliAudu)
         
         MAE <- sapply(lapply(store, abs_dist), mean)            ## MAE
         RMSE <- sqrt(sapply(lapply(store, sqrt_dist), mean))    ## RMSE
         
         result <- cbind(MAE, RMSE)
         
         row.names(result) <- estimator
         names(result) <- c("MAE", "RMSE")
         
         write.table(result, sep = ",", append = T,
                    file = "C:/Users/student/Desktop/Final Project/Result.csv")
         
         MAEs <- rbind(MAEs, MAE)
         RMSEs <- rbind(RMSEs, RMSE)
         ## End of Setting output
     }
     ## End of Sampling and Estimation
     data_pic = data_pic + 1
}

## error plot
setwd("C:/Users/student/Desktop/Final Project/Plot")

mae_n10 <- data.frame(rbind(MAEs[c(1,5,9,13),]))
mae_n30 <- data.frame(rbind(MAEs[c(2,6,10,14),]))
mae_n100 <- data.frame(rbind(MAEs[c(3,7,11,15),]))
mae_n400 <- data.frame(rbind(MAEs[c(4,8,12,16),]))
mae_n <- list(mae_n10, mae_n30, mae_n100, mae_n400)

rmse_n10 <- data.frame(rbind(RMSEs[c(1,5,9,13),]))
rmse_n30 <- data.frame(rbind(RMSEs[c(2,6,10,14),]))
rmse_n100 <- data.frame(rbind(RMSEs[c(3,7,11,15),]))
rmse_n400 <- data.frame(rbind(RMSEs[c(4,8,12,16),]))
rmse_n <- list(rmse_n10, rmse_n30, rmse_n100, rmse_n400)

plot_error(mae_n, "MAE ")
plot_error(rmse_n, "RMSE ")

setwd("C:/Users/student/Desktop/Final Project")

end.times <- Sys.time()
runningTime <- end.times - start.times