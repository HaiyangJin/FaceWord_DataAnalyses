# function to generate the colors for p values
# Created by Haiyang Jin (4/12/2019)
sig_colors <- function(pvalue_list, default_color = "black"){
  # convert to numeric
  pvalue_list <- as.numeric(pvalue_list)
  
  color_list <- ifelse(pvalue_list > .1, default_color,
                       ifelse(pvalue_list <= .1 & pvalue_list > .05, "blue", 
                              ifelse(pvalue_list <= .05, "red", default_color)))
  
  return(color_list)
}