remove_columns <- function(dataset, columns_to_remove) {
  new_dataset <- dataset %>% select(-all_of(columns_to_remove))
  
  return(new_dataset)
}


to_factor <- function(data, columns_to_convert = c(), columns_to_exclude = c()){
  new_df = data
  columns <- setdiff(colnames(data), columns_to_exclude)
  all_col = c(columns_to_convert, columns)
  #print(all_col)
  new_df[, all_col] <- lapply(new_df[, all_col], factor)
  
  return(new_df)
}