# function to generate the colors for p values
# Created by Haiyang Jin (4-Dec-2019)
sig_colors <- function(pvalue_list, default_color = "black"){
  # convert to numeric
  pvalue_list <- as.numeric(pvalue_list)
  
  color_list <- ifelse(pvalue_list > .1, default_color,
                       ifelse(pvalue_list <= .1 & pvalue_list > .05, "blue", 
                              ifelse(pvalue_list <= .05, "red", default_color)))
  
  return(color_list)
}


# convert p-values to asterisks
sig_ast <- function(pvalue_list, default_ast = ""){
  # convert to numeric
  pvalue_list <- as.numeric(pvalue_list)
  
  ast_list <- ifelse(pvalue_list < .001, "***",
                       ifelse(pvalue_list < .01, "**", 
                              ifelse(pvalue_list <= .05, "*", default_ast)))
  
  return(ast_list)
}
