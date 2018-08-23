#pat has has no missings except for first_line
conditional_impute = function(pat, predictor_names, coupling, mean_parameters, cov_parameters) {
  outcome = ifelse(pat$radio_imp,"radio",ifelse(pat$resection_imp,"resection",stop("Nothing to impute")))
  pat = pat[, predictor_names]
  if (!(sum(is.na(pat))>0 & sum(is.na(pat))< length(pat)))
    stop("No missings or only missings!")
  
  available = which(coupling %in% names(pat)[!is.na(pat)])
  
  transformed = pat[which(!is.na(pat))]
  if (length(transformed)>0) {
    transformed=as.numeric(model.matrix(~.,data=transformed))
    transformed=transformed[-1]
  }
  
  # methode 2
  V22=cov_parameters[available,available]
  V12=cov_parameters[-available,available]
  V21=cov_parameters[available,-available]
  V11=cov_parameters[-available,-available]
  mean_missing = mean_parameters[-available] - (V12 %*% solve(V22)) %*% (transformed-mean_parameters[available] )
  cov_missing = V11 - (V12 %*% solve(V22)) %*% V21
  generated = rmvnorm(100, mean_missing, cov_missing) %>% set_colnames(fit$Design$values$first_line[-1])
  
  generated[,!str_detect(colnames(generated),outcome)] = -Inf
  colMeans(generated) %>% which.max %>% names
}