#' Convert table of number of survivals per day to individual information table
#'
#' @param dataset dataset with all the data
#' @param concentration concentration for each dilution
#' @param standard_col a vector with two values representing the standard columns
#' @return a data frame with calculated concentration
#' @export
#' 
elisa_ifg <- function(dataset, concentration = concentration, standard_col = standard_col)
{
  standard_cal <- apply(ifg[,c(11+1,12+1)],1,mean)
  ifg2 <- ifg[,-c(1,11+1,12+1)]
  
  concon <- data.frame(concentration, standard_cal)
  
  plot(concon)
  
  ggplot(concon, aes(x=concentration, y=standard_cal)) +  geom_point(shape=1) +  geom_smooth(method=lm , color="red", se=TRUE)  # Add linear regression line 
  
  fit <- lm(standard_cal~concentration, data = concon)
  
  s1 <- summary(fit)
  inter <- s1$coefficients[1,1]
  xval <- s1$coefficients[2,1]
  # valor <- 0.102
  # pred <- ((valor - inter)/xval)*5
  
  dfpred <- ((as.matrix(ifg2) - inter)/xval)*5
  
  return(dfpred)
  
  # tumorsolo <- 5
  # grupos <- 4
  # 
  # celtumsolas1 <- mean(dfpred[1:4,5])
  # celtumsolas2 <- mean(dfpred[5:8,5])
  # 
  # final_table1 <- dfpred[1:4,] - celtumsolas1
  # final_table2 <- dfpred[5:8,] - celtumsolas2
}
