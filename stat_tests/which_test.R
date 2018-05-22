which_test <- function(){
  
  n_outcome_variables <- menu(c("One", "Two or more"), title="How many outcome variables?")
  
  if(n_outcome_variables == 1){
    
    outcome_type <- menu(c("Continuous", "Categorical"), title="What type of outcome?")
    
    if(outcome_type == 1){
      
      n_predictors <- menu(c("One", "Two or more"), title="How many predictor variables?")
      
      if(n_predictors == 1){
        
        predictor_type <- menu(c("Continuous", "Categorical"), title="What type of predictor?")
        
        if(predictor_type == 1){
          
          message("Parametric test: Pearson correlation or regression")
          message("If parametric assumptions fail: Bootstrap correlation/ regression, Spearman correlation, Kendal's tau")
          
        } else {
          
          n_categories <- menu(c("Two", "More than two"), title="How many levels in the category?")
          
          same_entities <- menu(c("Same", "Different"), title="Are the entities in each category same or different?")
            
          if(n_categories == 1){
            
            if(same_entities == 1){
              message("Parametric test: Dependent t-test")
              message("If parametric assumptions fail: Bootstrap t-test, Wilcoxon matched pair t-test")
            } else{
              message("Parametric test: Independent t-test, point bi-serial correlation")
              message("If parametric assumptions fail: Bootstrap t-test, Mann-Whitney test")
            }
            
          } else {
            if(same_entities == 1){
              message("Parametric test: One-way repeated measures ANOVA")
              message("If parametric assumptions fail: Bootstrapped ANOVA or Friedman's ANOVA")
            } else{
              message("Parametric test: One-way independent ANOVA (stat_tests/oneWayANOVA transformations.R)")
              message("If parametric assumptions fail: Robust ANOVA or Krushal Wallis test (stat_tests/oneWayANOVA transformations.R)")
            }
          }
        }
        
      } else {
        
      }
      
    } else{
      
    }
    
  } else{
    
  }
  
}


