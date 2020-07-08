
forwardSelection_adjRsquared <- function(df, response, max_steps = NaN, verbose = TRUE){
  
  selected_vars = c()
  remain_vars <- names(df)
  remain_vars <- remain_vars[-which(remain_vars == response)] # removing response variable
  if (is.na(max_steps)){
    max_steps <- length(remain_vars)
  }
  current_adjR <- 0
  best_formula <- ""
  
  step = 1
  while(step <= max_steps){
    results = c()
    for (var in remain_vars){
      if (is.null(selected_vars)){
        vars <- var
      } else{
        vars <- paste(paste(selected_vars, collapse = ' + '), var, sep = ' + ')    
      }
      formula = paste(response, vars, sep = ' ~ ')
      summary <- summary(lm(as.formula(formula), data = df))
      results <- c(results, summary$adj.r.squared)
    }
    
    if (max(results) > current_adjR){
      current_adjR <- max(results)
      selected_vars <- c(selected_vars, remain_vars[which.max(results)])
      remain_vars <- remain_vars[-which.max(results)]
      best_formula <- paste(response, paste(selected_vars, collapse = ' + '), sep = ' ~ ')
      if (verbose){
        cat(paste("\n\nStep ", step, ":\n"))
        print(best_formula)
        cat(paste("Adjusted R Squared: ", current_adjR))
      }
    } else{
      if (verbose){
        cat(paste("\n\nNo improvment in Adjusted R Squared, finished in step ", step-1))
      }
      break
    }
    step <- step + 1
  }
  return (lm(formula = as.formula(best_formula), data = df))
}



backwardSelection_adjRsquared <- function(df, response, max_steps = NaN, verbose = TRUE){
  
  selected_vars <- names(df)
  selected_vars <- selected_vars[-which(selected_vars == response)] # removing response variable
  if (is.na(max_steps)){
    max_steps <- length(selected_vars) - 1
  }
  current_adjR <- 0
  best_formula <- paste(response, paste(selected_vars, collapse = ' + '), sep = ' ~ ')
  
  step = 1
  while(step <= max_steps){
    results = c()
    for (var in selected_vars){
      vars <- paste(selected_vars[-which(selected_vars == var)], collapse = ' + ')    
  
      formula = paste(response, vars, sep = ' ~ ')
      summary <- summary(lm(as.formula(formula), data = df))
      results <- c(results, summary$adj.r.squared)
    }
    
    if (max(results) > current_adjR){
      current_adjR <- max(results)
      selected_vars <- selected_vars[-which.max(results)]
      best_formula <- paste(response, paste(selected_vars, collapse = ' + '), sep = ' ~ ')
      if (verbose){
        cat(paste("\n\nStep ", step, ":\n"))
        print(best_formula)
        cat(paste("Adjusted R Squared: ", current_adjR))
      }
    } else{
      if (verbose){
        cat(paste("\n\nNo improvment in Adjusted R Squared, finished in step ", step-1))
      }
      break
    }
    step <- step + 1
  }
  
  return (lm(formula = as.formula(best_formula), data = df))
}

