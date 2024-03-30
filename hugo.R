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



generate_fake_data <- function(data){
  
  columns_to_exclude <- c("percenttp", "wastecents", "feelfedgov_1", "weightvec")
  columns_to_convert <- setdiff(names(Data), columns_to_exclude)
  
  n = dim(Data)[1]
  p_gender = table(Data$gender)/n
  gender_fake = sample(1:4, size=n, replace=TRUE, prob=p_gender)
  fake_data = data.frame(gender = as.factor(gender_fake))
  
  for (i in columns_to_convert){
    p = tabulate(as.integer(sapply(Data[i], function(x) x + ifelse(min(Data[i]) == 0, 1, 0))))/n
    d_fake = sample(min(Data[i]):max(Data[i]), size=n, replace=TRUE, prob=p)
    fake_data[i] = d_fake
  }
  fake_data$wastecents = pmin(100, pmax(0, as.integer(rnorm(n, mean = mean(Data$wastecents), sd = sd(Data$wastecents)))))
  fake_data$feelfedgov_1 = pmin(100, pmax(0, as.integer(rnorm(n, mean = mean(Data$feelfedgov_1), sd = sd(Data$feelfedgov_1)))))
  fake_data$weightvec = pmax(0, rnorm(n, mean = mean(Data$weightvec), sd = sd(Data$weightvec)))
  
  
  coef = c()
  for (i in colnames(fake_data)){
    res <- lm(percenttp ~ get(i), data=Data)
    coef <- c(coef, res$coefficients[2])
  }
  coef = c(coef, 50)
  fake_data$percenttp = rep(1, n)
  
  
  for (i in 1:dim(fake_data)[1]){
    fake_data$percenttp[i] = as.integer(sum(coef * fake_data[i,]))
  }
  
  names(coef) = colnames(fake_data)
  
  return(list(fake_data = fake_data, coef = coef))
}








