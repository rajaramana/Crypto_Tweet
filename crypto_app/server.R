function(input, output, session) {
  
  # Delete all the files at the start of the session
  removeFiles(session)
  
  output$currentTime <- renderText({
    
    # The below function runs every 1 and half min
    invalidateLater(1500 * 60, session)
    
    # Run the main function
    fetch_tweetdata(query = query, stream_time = 10)
  })
  
  
  output$rawtable <- renderDataTable({
    
    # Load the data every 3 mins
    invalidateLater(1000*60*3)
    
    # Listing the file names
    file_name <- list.files('./data/')
    
    # Check if the file has data or not
    if(file_name == 'stream_1.csv' | length(file_name) > 0){
      
      # Read the file
      df <- read.csv('./data/stream_1.csv')
      
      # Print only the necessary columns
      df1 <-  df %>% select(created_at, screen_name, text, urls_url,coin)
      
      # toggle
      if(input$select == 'ALL'){
        
        return(df1 %>% select(-coin))
        
      }else if(input$select == 'BTC'){
        
        df2 <- data.frame(matrix(nrow=0, ncol=0))
        
        df2 <- df1 %>% filter(coin == 'BTC')
        
        return(df2 %>% select(-coin))
      }else if(input$select == 'LTC'){
        
        df3 <- data.frame(matrix(nrow=0, ncol=0))
        
        df3 <- df1 %>% filter(coin == 'LTC')
        
        return(df3 %>% select(-coin))
      }else if(input$select == 'ETH'){
        
        df4 <- data.frame(matrix(nrow=0, ncol=0))
        
        df4 <- df1 %>% filter(coin == 'ETH')
        
        return(df4 %>% select(-coin))
      }else if(input$select == 'XLM'){
        
        df5 <- data.frame(matrix(nrow=0, ncol=0))
        
        df5 <- df1 %>% filter(coin == 'XLM')
        
        return(df5 %>% select(-coin))
      }
    }else{
      
      df <- data.frame(matrix(nrow = 1, ncol = 1))
      
      names(df)[1] <- "Text"
      
      df[1,] <- "Please wait, fetching data"
      
      return(df)
    }
  })
  
}