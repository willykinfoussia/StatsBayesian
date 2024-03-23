remove_columns <- function(dataset, columns_to_remove) {
  new_dataset <- dataset %>% select(-all_of(columns_to_remove))
  
  return(new_dataset)
}


to_factor <- function(data, columns){
  new_df = data
  new_df[, columns] <- lapply(new_df[, columns], factor)
}