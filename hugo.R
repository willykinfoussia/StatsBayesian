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
  new_df[,'statename'] = as.factor(as.numeric(new_df[,'statename']))
  
  return(new_df)
}


construct_data_text <- function(data, column_of_text){
  data_res <- data.frame(text = data[, column_of_text]) %>% mutate(linenumber = row_number())
  return(data_res)
}

text_to_sentiment <- function(data){
  bing <- tidytext::get_sentiments("bing")
  
  text_bing <- data %>% unnest_tokens(word, text) %>% inner_join(bing, by = "word")
  text_bing[, "sentiment"] <- ifelse(text_bing[, "sentiment"] == "positive", 1, -1)
  
  text_res <- text_bing %>% group_by(linenumber) %>% summarise(moyenne = mean(sentiment))
  text_res[, "moyenne"] = ifelse(text_res[,'moyenne'] > 0, 1, ifelse(text_res[, "moyenne"] < 0, -1, 0))
  
  text_join = data %>% left_join(text_res, by = 'linenumber')
  text_join[, "moyenne"] <- ifelse(is.na(text_join[, "moyenne"]), 0, text_join[, "moyenne"])
  
  return(text_join)
}

sentiment_columns <- function(data, columns){
  new_df <- data
  for (i in columns){
    data_text <- construct_data_text(new_df, i)
    data_sentiment <- text_to_sentiment(data_text)
    var <- paste0(i, "_sentiment")
    new_df <- new_df %>% select(-matches(i))
    new_df <- cbind(new_df, data_sentiment[, "moyenne"])
    names(new_df)[ncol(new_df)] <- var
  }
  
  return(new_df)
}










