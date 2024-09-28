### Functions for processing & analyzing GOCI satellite data ###

threshold_aggregate <- function(x, na_threshold = 0.5) {
  if (sum(is.na(x)) / length(x) > na_threshold) {
    return(NA)
  } else {
    return(mean(x, na.rm = TRUE))
  }
}

aggregate_propNA <- function(x) {
  return(sum(is.na(x)) / length(x))
}

overall_p <- function(my_model) {
  f <- summary(my_model)$fstatistic
  p <- pf(f[1],f[2],f[3],lower.tail=F)
  attributes(p) <- NULL
  return(p)
}

region_ID <- function(df) {
  df$ID <- ifelse(df$x>=122.5&df$x<=125.85 & df$y>=23.7&df$y<=25.3,"A",
                  ifelse(df$x>=123.05&df$x<=124.95 & df$y>=25.4&df$y<=26.3, "B",
                         ifelse(df$x>=126.3&df$x<=129.5 & df$y>=25.2&df$y<=27.3,"C",
                                ifelse(df$x>=128.15&df$x<=129.45 & df$y>=27.2&df$y<=28.25,"D",
                                       ifelse(df$x>=130.55&df$x<=131.95 & df$y>=25.2&df$y<=26.35,"E",
                                              ifelse(df$x>=130.55&df$x<=131.8 & df$y>=23.95&df$y<=25.05,"F",NA))))))
}

panels <- data.frame(ID = c("A","B","C","D","E","F"),
                     x_min = c(122.5,123.05,126.3,128.15,130.55,130.55),
                     x_max = c(125.85,124.95,129.5,129.45,131.95,131.8),
                     y_min = c(23.7,25.4,25.2,27.2,25.2,23.95),
                     y_max = c(25.3,26.3,27.3,28.25,26.55,25.05))
