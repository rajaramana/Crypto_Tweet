# Install the libraries
if(!require('rtweet')){install.packages('rtweet'); library('rtweet')}else{library('rtweet')}
if(!require('purrr')){install.packages('purrr'); library('purrr')}else{library('purrr')}
if(!require('dplyr')){install.packages('dplyr'); library('dplyr')}else{library('dplyr')}
if(!require('ggplot2')){install.packages('ggplot2'); library('ggplot2')}else{library('ggplot2')}
if(!require('shinydashboard')){install.packages('shinydashboard'); library('shinydashboard')}else{library('shinydashboard')}
if(!require('lubridate')){install.packages('lubridate'); library('lubridate')}else{library('lubridate')}


# Name assigned to crypto app on twitter
appname <- "crypto_tweet_news"

# Api key (example below is not a real key)
key <- "10VZ7hhTtIVoR7fN7rslvlFUR"

# Api secret (example below is not a real key)
secret <- "BZeEAmma4DmasEoE7nXcHAddzQkU7ZEylEnxlSNPr6OAnarum"

# Run only for the first time
# twitter_token <- create_token(
# app = appname,
# consumer_key = key,
# consumer_secret = secret, set_renv = FALSE)
# saveRDS(twitter_token, 'token.rds')

# read twitter token
twitter_token <- readRDS("token.rds")

# Steam of words to filter the tweets
query <- "#btc,#ltc,#eth,#xlm"

########################################################
# Data processing function
########################################################
# Function which runs at the start of the session
removeFiles <- function(session){

# Validate if there is already a existing file
  if(length(list.files('./data/')) > 0){
	
	# if existing read the file and verify if it has most recent information
	df <- read.csv('./data/stream_1.csv')
	
	# check the last updated UTC time
	if(nrow(df) > 0){
	 doc_time <- 	as.Date(df[1,"created_at"])
	 current_time <- as.Date(now("Iceland"))
	 
	 # if both times are not equal delete the file
	 if(doc_time != current_time){
	   
	   map(paste0('./data/',list.files('./data/')[grepl('*.csv', list.files('./data/'))]),file.remove)
	   
	 }else{
	   
	   fetch_tweetdata(query, 10)
	   
	 }
	} else{
	  
	  fetch_tweetdata(query, 10)
	  
	}
}
}

# Unaccent
unaccent <- function(x) {
  x = gsub("@\\w+", "", x) 
  x = gsub("[[:punct:]]", " ", x)
  x = gsub("[ |\t]{2,}", " ", x) 
  x = gsub("^ ", " ", x) 
  x
}

# Data processing function
dataProcess <- function(rt){
  
  # Convert text to character
  rt$text <- as.character(rt$text)
  rt$text <- map_chr(rt$text, unaccent)
  rt$text <- map_chr(rt$text, iconv, from = "ISO_8859-2", to = 'UTF-8')
  
  # Creating empty data frames
  temp_btc <- data.frame(matrix(nrow=0, ncol=0),stringsAsFactors=FALSE)
  temp_ltc <- data.frame(matrix(nrow=0, ncol=0),stringsAsFactors=FALSE)
  temp_eth <- data.frame(matrix(nrow=0, ncol=0),stringsAsFactors=FALSE)
  temp_xlm <- data.frame(matrix(nrow=0, ncol=0),stringsAsFactors=FALSE)
  temp_other <- data.frame(matrix(nrow=0, ncol=0),stringsAsFactors=FALSE)
  
  # Segmenting BTC
  if(length(rt$text)>0){
    
    # BTC segment
    btc <- grepl('.*btc.*|.*bit coin.*|.*bitcoin.*|.*BITCOIN.*|.*Bit Coin.*|.*BTC.*',rt$text)
    
    # Segmenting LTC
    ltc <- grepl('.*ltc.*|.*litecoin.*|.*lite coin.*|LITE COIN.*|.*LTC.*|.*Lite.*',rt$text)
    
    # Ether
    eth <-  grepl('.*eth.*|.*ether.*|.*ethereum.*|.*ETH.*|.*Ether.*|.*ETHEREUM.*',rt$text)
    
    # XLM
    xlm <- grepl('.*xlm.*|.*stellar.*',rt$text)
    
    # Other random text
    other <- !(btc|ltc|eth|xlm)
    
    # coin category - BTC
    temp_btc <- rt[btc,]
    if(length(temp_btc$text)>0){
      temp_btc$coin <- 'BTC' 
    }
    
    # coin category - LTC
    temp_ltc <- rt[ltc,]
    if(length(temp_ltc$text) > 0){
      temp_ltc$coin <- 'LTC' 
    }
    
    
    # coin category - XLM
    temp_xlm <- rt[xlm,]
    if(length(temp_xlm$text) > 0){
      temp_xlm$coin <- 'XLM' 
    }
    
    # coin category - ETH
    temp_eth <- rt[eth,]
    if(length(temp_eth$text)>0){
      temp_eth$coin <- 'ETH' 
    }
    
    # if(length(other) >0 & sum(other) > 0){
    #   temp_other <- rt[other,]
    #   temp_ltc$coin <- 'OTHER'
    # }
    
    # Binding all the data
    temp_btc <- rbind(temp_btc, temp_ltc, temp_eth, temp_xlm)
    
    # Return the file
    return(temp_btc)
  }
}


#####################################
# fetch_tweetdata function
#####################################
fetch_tweetdata <- function(query, stream_time){
  
  # Number of files
  files <- length(list.files('./data/'))

  # Remove the json file
  map(list.files()[grepl('*.json', list.files())], file.remove)
  
  # Check if they are any files
  if(files == 0){
    
    # Fetch the twitter data
    rt <- stream_tweets(q = query,timeout = stream_time, language = 'en')
    
    # Convert to a data frame
    rt <- map(rt, as.character) %>% as.data.frame(.,stringsAsFactors=FALSE)
    
    if(nrow(rt) > 0){
      
      # Remove the duplicates
      rt <- rt[!duplicated(rt),]
      
      # Processing data
      rt <- dataProcess(rt)
      
      # write the file to stream1
      write.csv(rt,'./data/stream_1.csv', row.names = FALSE)
    }else{
      
      print("Did not receive data from twitter API")
      
    }
  }else{
    
    # read the first file
    tweet_data <- read.csv('./data/stream_1.csv')
    
    # Removing if they are any duplicates
    tweet_data <- tweet_data[!duplicated(tweet_data),]
    
    # Due to hosting issues, deleting the data after it reaches 1000 rows. Change the value if you would like to have a smaller or bigger data frame
    if(nrow(tweet_data) > 10000){
      
      map(paste0('./data/',list.files('./data/')[grepl('*.csv', list.files('./data/'))]),file.remove)
      
      fetch_tweetdata(query, 10)
      
    }else{
     
      # Needed columns
      columns <- c('status_id',	'created_at',	'user_id',	'screen_name',	'text',	'source',	'reply_to_status_id',	'reply_to_user_id',	'reply_to_screen_name',	'is_quote',	'is_retweet',	'favorite_count',	'retweet_count',	'hashtags',	'symbols',	'urls_url',	'urls_t.co',	'urls_expanded_url',	'media_url',	'media_t.co',	'media_expanded_url',	'media_type',	'ext_media_url',	'ext_media_t.co',	'ext_media_expanded_url',	'ext_media_type',	'mentions_user_id',	'mentions_screen_name',	'lang',	'quoted_status_id',	'quoted_text',	'retweet_status_id',	'retweet_text',	'place_url',	'place_name',	'place_full_name',	'place_type',	'country',	'country_code',	'geo_coords',	'coords_coords',	'bbox_coords','coin')
      
      # tweet data
      tweet_data <- tweet_data[,columns]
      
      # Empty df
      rt <- data.frame()
      
      # Fetch the twitter data
      rt <- stream_tweets(q = query,timeout = stream_time, language = 'en')
      
      # Convert rt to data frame
      rt <- map(rt, as.character) %>% as.data.frame(.,stringsAsFactors=FALSE)
      
      
      # Processing data
      rt <- dataProcess(rt)
    

      # Remove the json file
      map(list.files()[grepl('*.json', list.files())], file.remove)
      
      # row bind the data
      tweet_data <- rbind(rt,tweet_data)
      
      # write the file
      if(nrow(tweet_data)>0){
        write.csv(tweet_data,'./data/stream_1.csv', row.names = FALSE)
      }
    }
  }
}




