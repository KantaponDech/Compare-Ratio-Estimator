## Prepare element
data_pic <- 1
MAEs <- numeric()
RMSEs <- numeric()
MAEn <- numeric()
RMSEn <- numeric()
estimator <- c("Traditional", "Sisodia and Dwivesdi", "Singh and Kakran",
               "Upadhyaya and Singh", "Singh and Tailor", "Yan and Tian",
               "Subramani and Kumarpandiyan", "Jerajuddin and Kishun", "Muili and Audu")

## Approximate Function
Trad_Est <- function(){ ## Traditional Estimator
  return(mean(y) * mean(X) / mean(x))
}
SisoDwive <- function(){ ## Sisodia and Dwivesdi
  return(mean(y) * (mean(X) + cv.X) / (mean(x) + cv.X))
}
SinghKa <- function(){ ## Singh and Kakran
  return(mean(y) * (mean(X) + kurtosis(x)) / (mean(x) + kurtosis(x)))
}
UpaSingh <- function(){ ## Upadhyaya and Singh
  return(mean(y) * (sd(x) + kurtosis(x)) / (sd(x) + kurtosis(x)))
}
SinghTailor <- function(){ ## Singh and Tailor
  return(mean(y) * (mean(X) + cor(X,Y)) / (mean(x) + cor(X,Y)))
}
YanTian <- function(){ ## Yan and Tian
  return(mean(y) * (mean(X) + skewness(x)) / (mean(x) + skewness(x)))
}
SubKum <- function(){ ## Subramani and Kumarpandiyan
  return(mean(y) * (mean(X) + median(x)) / (mean(x) + median(x)))
}
JerKis <- function(){ ## Jerajuddin and Kishun
  return(mean(y) * (mean(X) + sampleSize) / (mean(x) + sampleSize))
}
MuiliAudu <- function(){ ## Muili and Audu
  return(mean(y) * (((mean(X) + sampleSize) / (mean(x) + sampleSize)) ^ alpha))
}

## Error distance
abs_dist <- function(arr){
  abs(arr - mean(Y))
}
sqrt_dist <- function(arr){
  (arr - mean(Y)) ** 2
}

## Error plot
plot_dataset <- function(){
    png(filename = paste0("Dataset ", data_pic, ".png"), width = 840, height = 640)
    plt = ggplot(map = aes(X,Y, color = "red")) +
      geom_point() +
      geom_smooth(method = "lm", color = "chartreuse4") +
      theme(plot.title = element_text(hjust = 0.5),
            legend.position = "none", text = element_text(size = 28),
            panel.border = element_rect(colour = "black", fill = NA, size = 1)) +
      labs(title = paste0("Dataset ", data_pic,
                          " with skewness of Y = ", skewed))
    print(plt)
    dev.off()
    
    png(filename = paste0("Density X", data_pic, ".png"), width = 840, height = 640)
    plt = ggplot(map = aes(X)) +
      geom_density(map = aes(y = ..scaled..), fill = "green", alpha = 0.5) +
      theme(plot.title = element_text(hjust = 0.5),
            legend.position = "none", text = element_text(size = 20),
            panel.border = element_rect(colour = "black", fill = NA, size = 1)) +
      labs(title = paste0("Density of X in Dataset ", data_pic,
                          " with skewness of Y = ", skewed))
    print(plt)
    dev.off()
    
    png(filename = paste0("Density Y", data_pic, ".png"), width = 840, height = 640)
    plt = ggplot(map = aes(Y)) +
      geom_density(map = aes(y = ..scaled..), fill = "blue", alpha = 0.5) +
      theme(plot.title = element_text(hjust = 0.5),
            legend.position = "none", text = element_text(size = 20),
            panel.border = element_rect(colour = "black", fill = NA, size = 1)) +
      labs(title = paste0("Density of Y in Dataset ", data_pic,
                          " with skewness of Y = ", skewed))
    print(plt)
    dev.off()
}

plot_error <- function(error, err_name){
    for(i in 1:4){
        dat <- unlist(error[[i]], use.names = FALSE)
        grp <- factor(sort(rep(1:9,4)))
        dat <- data.frame(rep(c(1,3,5,7)), grp, dat)
        names(dat) <- c("skw", "grp", "val")
        
        png(filename = paste0(err_name, i ,".png"), width = 1280, height = 720)
        
        plt = ggplot(dat, aes(x = skw, y = val, group = grp, colour = grp)) +
            geom_line(size = 1) +
            theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 28),
                  legend.background = element_rect(fill = "lightblue", size = 1,
                                                   linetype = "solid", colour = "darkblue"),
                  panel.border = element_rect(colour = "black", fill = NA, size = 1)) +
            labs(title = paste0(err_name, i), x = "Skewness", y = err_name) +
            scale_x_continuous(breaks = c(1,3,5,7)) +
            scale_color_discrete(name = "Ratio Estimator", labels = estimator)
        
        print(plt)
        dev.off()
    }
}