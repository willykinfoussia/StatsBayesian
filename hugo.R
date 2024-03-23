remove_columns <- function(dataset, columns_to_remove) {
  new_dataset <- dataset %>% select(-all_of(columns_to_remove))
  
  return(new_dataset)
}