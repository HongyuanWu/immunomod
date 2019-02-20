#' Convert table of number of survivals per day to individual information table
#'
#' @param data dataset with survivals per day
#' @param num_col_group number of columns after day column
#' @return a data frame with information per individual
#' @export


raw2survival <- function(data = datos, num_col_group = 4) {
    individuos <- rowSums(data[, -1])
    cambio_individuos <- individuos - individuos[1]
    
    primer_dia_muerte <- which(cambio_individuos < 0)[1]
    
    days <- NA
    grupo <- NA
    
    for (i in primer_dia_muerte:nrow(data)) {
        print(i)
        for (f in 2:(num_col_group + 1)) {
            cuantos_muertos <- data[i - 1, f] - data[i, f]
            if (cuantos_muertos > 0) {
                days <- c(days, rep(data$Days[i], cuantos_muertos))
                grupo <- c(grupo, rep(names(data)[f], cuantos_muertos))
            }
        }
    }
    
    ultima_fila <- nrow(data)
    cuantos_vivos <- c(0, as.numeric(data[ultima_fila, -1]))
    for (f in 2:(num_col_group + 1)) {
        if (cuantos_vivos[f] > 0) {
            days <- c(days, rep(data[ultima_fila, 1], cuantos_vivos[f]))
            grupo <- c(grupo, rep(names(data)[f], cuantos_vivos[f]))
        }
    }
    
    sum_cuantos_vivos <- sum(cuantos_vivos)
    days2 <- days[-1]
    grupo2 <- grupo[-1]
    
    df <- data.frame(days2, grupo2)
    ultima_fila_df <- nrow(df)
    df$fustat <- 1
    df$fustat[(ultima_fila_df - sum_cuantos_vivos + 1):ultima_fila_df] <- 0
    
    return(df)
    
    # surv_object <- Surv(time = df$days2, event = df$fustat) surv_object if(plot1=='meyer') { fit1 <-
    # survfit(surv_object ~ grupo2, data = df) print(summary(fit1)) ggsurvplot(fit1, data = df, pval = TRUE) } else
    # if(plot1=='cox') { # Fit a Cox proportional hazards model fit.coxph <- coxph(surv_object ~ grupo2,data = df)
    # ggforest(fit.coxph, data = df) }
}

