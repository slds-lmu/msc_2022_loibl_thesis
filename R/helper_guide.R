# functions for finding the best splitting variable with guide





guide_test <- function(x, residuals, xgroups = NULL) {
  # categorize residuals
  # split Y into 2 parts based on whether residuals are positive or non-positive
  # separately for each parameter
  ybin <- (-1)^((residuals>0)+1)   # -1 or +1
  # browser()
  curv_test = sapply(x, function(xval){
    test_curvature(xval = xval, ybin = ybin, xgroups = xgroups)
  })

  interaction_set = combn(colnames(x), 2, simplify = FALSE)
  
  int_test = sapply(interaction_set, function(cols){
    test_interaction(x = x, xvals = cols, ybin = ybin, xgroups = xgroups)
  })
  
  int_test = as.data.table(t(int_test))
  int_test[,p.value := as.numeric(p.value)]
  
  if(max(curv_test["p.value",]) > max(int_test[, p.value])){
    z = colnames(curv_test)[curv_test["p.value",] == max(curv_test["p.value",])]
    type = "curvature"
  } else {
    z_vec = int_test[p.value == max(p.value), c(z1,z2)]
    # if there is one best interactionpair, do the following:
    if(length(z_vec) == 2){
      # if both are categorical, use the variable with the smaller p value in the curvature test
      if((is.factor(x[,z_vec[1]]) & is.factor(x[,z_vec[2]])) ){
        curv_test_small = curv_test[,z_vec]
        z = colnames(curv_test_small)[curv_test_small["p.value",] == max(curv_test_small["p.value",])]
      } 
      # if both variables are numeric, use both as potential split variables
      else if (is.numeric(x[,z_vec[1]]) & is.numeric(x[,z_vec[2]])){
        z = z_vec
      } 
      # if one is numeric and one is categorical, use the categorical for splitting
      else {
        z = z_vec[is.factor(x[,z_vec])]
      }
    } else {
      # if there are multiple "best" pairs, count which variable is involved in 
      # the most significant interaction most often and choose this as splitting variable
      z_table = table(z_vec)
      z_candidates = names(z_table)[z_table == max(z_table)]
      if (length(z_candidates) == 1){
        z = z_candidates
      } else {
        if(length(z_candidates[is.factor(x[,z_candidates])]) > 0){
          # if there are still more competing variables including categorical variables include only these as possible splitting variables
          z = z_candidates[is.factor(x[,z_candidates])]
        } else {
          # if all are numerical, use all as possible splitting variables
          z = z_candidates
        }
      }
    }
    
    type = "interaction"
  }
  
  return(list(z = z, type = type))
  
}



test_curvature = function(xval, ybin, xgroups){
  # test function returning 'p.value' = log(1-pval) and 'statistic' = log(stat) of independence tests
  # browser()
  # if all values of the selected covariate are equal return highest possible p.value 
  # and Teststatistic = 0
  if(length(unique(xval))<2) return(list(p.value = log(1-1), statistic = log(0)))
  
  
  # categorize split variable
  
  if(is.numeric(xval)){
    if(is.null(xgroups)) xgroups <- 4
    if(length(xgroups)>1) {
      xbreaks <- xgroups
    } else {
      xbreaks <- quantile(xval, c(0:xgroups)/xgroups)
    }
    x_cat <- cut(xval, breaks = xbreaks, labels = c(1:xgroups), 
                 include.lowest = TRUE)
  } else {
    x_cat <- xval
  }
  
  
  # compute curvature test (for each parameter separately)
  tst_curv <- chisq.test(x = x_cat, y = ybin)
  ret <- c(p.value = log(1 - as.numeric(tst_curv$p.value)), statistic = log(as.numeric(tst_curv$statistic)))
  return(ret)
}



test_interaction = function(x, xvals, ybin, xgroups){
  # browser()
  xval1 = x[,xvals[1]]
  xval2 = x[,xvals[2]]
  
  if(length(unique(xval1)) < 2 | length(unique(xval2)) < 2){
    return(list(z1 = xvals[1], z2 = xvals[2], p.value = log(1-1), statistic = log(0)))
  }
  
  
  # categorize split variables
  if(is.null(xgroups)){
    xgroups = 2
  } 
  
  if(is.numeric(xval1)){
    if(length(xgroups)>1) {
      x1breaks = xgroups
    } else {
      x1breaks = quantile(xval1, c(0:xgroups)/xgroups)
    }
    x1_cat = cut(xval1, breaks = x1breaks, labels = c(1:xgroups), 
                 include.lowest = TRUE)
  } else {
    x1_cat = xval1
  }
  
  if(is.numeric(xval2)){
    if(length(xgroups)>1) {
      x2breaks = xgroups
    } else {
      x2breaks = quantile(xval2, c(0:xgroups)/xgroups)
    }
    x2_cat = cut(xval2, breaks = x2breaks, labels = c(1:xgroups), 
                  include.lowest = TRUE)
  } else {
    x2_cat = xval2
  }
  
  
  # combine the two categorized variables in one factor variable
  level_comb = as.data.table(expand.grid(unique(x1_cat), unique(x2_cat)))
  level_comb$id = as.factor(1:nrow(level_comb))

  x_cat = c()
  for(i in 1:length(x1_cat)){
    x_cat[i] = level_comb[Var1 == x1_cat[i] & Var2 == x2_cat[i], id]
  }

  
  # compute interaction test 
  tst_curv = chisq.test(x = x_cat, y = ybin)
  ret = c(z1 = xvals[1], z2 = xvals[2], p.value = log(1 - as.numeric(tst_curv$p.value)), 
          statistic = log(as.numeric(tst_curv$statistic)))
  return(ret)
  
}
