#' @export

plot.ABM_D <- function(D = D){
  # まだsimulationが更新されていない場合
  if(length(D$log)==1){
    message("This ABM object have not run the simulation yet.")
    return(NULL)
  }

  # agent attributes
  df <- D$.agent_attr(log = "all")
  time <- 2:length(df)
  attr_labels <- setdiff(colnames(df[[1]]), c("ID","f_label"))

  attr_change <- lapply(attr_labels, function(X){sapply(time, function(t){
    same_cell <- (df[[t]][[X]] == df[[t-1]][[X]])
    1-sum(same_cell)/length(same_cell)
  })})
  attr_change <- data.frame(attr_change)
  dimnames(attr_change) <- list(time, attr_labels)
  out_attr <- lapply(1:ncol(attr_change), function(i){
    plot(x = rownames(attr_change), y = attr_change[ ,i], type = "l",
         col = "red", ylim = c(0, 1),
         xlab = "time", ylab = "change in entry",
         main = attr_labels[i])
  })

  # output
  out <- c(out_attr)
  invisible(out)
}

