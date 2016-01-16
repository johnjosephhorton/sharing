ConvertFactor <- function(x) {
  # Converts factors with "na" levels to factor with missing values
  # 
  # Args:
  #   x: factor vector
  #
  # Returns:
  #   Factor with missing values
  
  lev <- levels(x)
  lev <- lev[!lev %in% c("", "na")]
  
  x <- as.character(x) %>% tolower()
  x[!x %in% lev] <- NA
    
  factor(x)
}
