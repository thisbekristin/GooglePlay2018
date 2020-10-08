library(DT)
library(shiny)
library(googleVis)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggcorrplot)

shinyServer(function(input,output){
  
# Background Tab
  
  output$totalapps <- renderInfoBox({
    totalapps = length(df$App)
    totalappstate = infoBox("Total Apps", totalapps, icon = icon("google-play"), color = "green")
  })
  
  output$uniquecat <- renderInfoBox({
    unique_cat = length(unique(df$Category))
    unique_cat_state = infoBox("Number of Categories", unique_cat, icon = icon("layer-group"),
                               color = "green")
  })
  
  output$uniquegenre <- renderInfoBox({
    unique_genre = length(unique(df$Genres))
    unique_genre_state = infoBox("Number of Genres", unique_genre, 
                                 icon = icon("layer-group"), color = "green")
  })
  
  output$picture <- renderUI({
    img2 = "https://www.derryjournal.com/images-i.jpimedia.uk/imagefetch/https://jpgreatcontent.co.uk/wp-content/uploads/2020/06/Google-play-store.jpg?width=640"
    tags$img(src=img2, width = 600, height = 500)
  })

  
  # Overview and Rankings 
  
  # Correlation
  output$correlation <-renderPlot({
    corr_info = cor(data.matrix(df[,c("Reviews", "Installs", "Price", "Rating", "Size")]), use = "complete.obs")
    ggcorrplot(corr_info, hc.order = TRUE, type = "lower", lab = TRUE) +
      ggtitle("Correlation Matrix") + 
      theme(plot.title = element_text(size = 20, face = "bold"))
    
    
  })
  
  output$ratinghist <- renderPlot({
    ggplot(subset(df, !is.na(Rating)), aes(x = Rating)) + geom_histogram(binwidth = 0.1, fill = "#23AE46")+
      ggtitle("App Rating Distribution") + 
      theme(plot.title = element_text(size = 20, face = "bold"),
            panel.border = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            axis.line = element_line(colour = "black"))
    
  })
  
  # Tabset
  output$topcat <-renderPlot({
    
    df = df %>%
      select(., Category) %>%
      filter(., Category %in% top_cat)
    df %>%
      ggplot(aes(x = fct_infreq(Category))) + geom_bar(fill="#23AE46") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            panel.border = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            axis.line = element_line(colour = "black")) +
      xlab(label = "Categories") +
      ylab(label = "Number of Apps") 
    
  })
  
  
  output$bottomcat <-renderPlot({
    df = df %>% 
      select(., Category) %>% 
      filter(., Category %in% bottom_cat)
    df %>% 
      ggplot(aes(x = fct_infreq(Category))) + geom_bar(fill="#23AE46") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            panel.border = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            axis.line = element_line(colour = "black"),
            plot.title = element_text(hjust = 0.5, size = 20)) +
      xlab(label = "Categories") +
      ylab(label = "Number of Apps") 
    
  })
  
  output$topinstalls <-renderPlot({
    df = df %>% 
      group_by(., Category) %>% 
      mutate(., Installs = Installs/1000000) %>% 
      filter(., Category %in% top_ins) %>% 
      summarise(., Installs = sum(Installs))
    df %>%
      ggplot(aes(x = reorder(Category, -Installs), y = Installs)) + geom_bar(fill="#23AE46", stat= "identity") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            panel.border = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            axis.line = element_line(colour = "black"),
            plot.title = element_text(hjust = 0.5, size = 20)) +
      xlab(label = "Categories") +
      ylab(label = "Number of Installs in Millions")
    
  })
  
  output$bottominstalls <-renderPlot({
    df = df %>% 
      group_by(., Category) %>% 
      mutate(., Installs = Installs/1000000) %>% 
      filter(., Category %in% bot_ins) %>% 
      summarise(., Installs = sum(Installs))
    df %>%
      ggplot(aes(x = reorder(Category, -Installs), y = Installs)) + geom_bar(fill="#23AE46", stat= "identity") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            panel.border = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            axis.line = element_line(colour = "black"),
            plot.title = element_text(hjust = 0.5, size = 20)) +
      xlab(label = "Categories") +
      ylab(label = "Number of Installs in Millions")
    
  })
  
  
  
  output$topboth <- renderTable({
    inner_join(top_cat_df,top_ins_df,by="Category")
  })
  
  
# Category Tab

  # Tabset
  output$categoryinfo <- renderPlot({
    df = df %>% 
      select(., Category, Type) %>% 
      filter(., Category %in% input$category)
    
    df %>% ggplot(aes(x = fct_infreq(Category))) +
      geom_bar(aes(fill = Type)) +
      xlab(label = "Categories") +
      ylab(label = "Number of Apps") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            panel.border = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            axis.line = element_line(colour = "black"))
  })
  
  output$categoryrating <- renderPlot({
    df = df %>% 
      select(., Category, Rating) %>% 
      filter(., Category %in% input$category)
    
    df %>% ggplot(aes(x = fct_infreq(Category), y = Rating)) +
      geom_boxplot() + 
      xlab(label = "Categories") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1), 
            panel.border = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            axis.line = element_line(colour = "black")) +
      stat_summary(fun = "mean", color = "red", geom = "point", size = 3, na.rm = TRUE) 
  })
  
  output$categorycontent <- renderPlot({
    df = df %>% 
      select(., Category, Content.Rating) %>% 
      filter(., Category %in% input$category)
    
    df %>% ggplot(aes(x = fct_infreq(Category))) +
      geom_bar(aes(fill = Content.Rating), position = "dodge") +
      xlab(label = "Categories") +
      ylab(label = "Number of Apps") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            panel.border = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            axis.line = element_line(colour = "black")) 
    
  })
  
  output$categoryinstalls <- renderPlot({
    df = df %>% 
      select(., Category, Installs) %>% 
      mutate(., Installs = Installs/1000000) %>% 
      filter(., Category %in% input$category)
    
    df %>% ggplot(aes(x = fct_infreq(Category), y = Installs)) +
      geom_bar(stat="identity", fill="#23AE46") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            panel.border = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            axis.line = element_line(colour = "black")) +
      xlab(label = "Categories") +
      ylab(label = "Installations in Millions")
    
  })
  
  output$genres <- renderPlot({
    df = df %>% group_by(., Category, Genres) %>% 
      filter(., Category %in% input$category) %>% 
      summarise(., Genres = length(unique(Genres))) 
    
    df %>% ggplot(aes(x = fct_infreq(Category), y = Genres)) +
      geom_bar(stat="identity", fill="#23AE46") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            panel.border = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            axis.line = element_line(colour = "black")) +
      xlab(label = "Categories") +
      ylab(label = "Number of Genres")
    
  })
  
  output$topinstall <- renderTable({
    df %>% group_by(., Category) %>% 
      arrange(desc(Installs)) %>%  
      filter(., Category %in% input$category, Installs == max(Installs))
  })
  
  output$toprated <- renderTable({
    df %>% group_by(., Category) %>% 
      arrange(desc(Weighted_Rating)) %>%  
      filter(., Category %in% input$category, Weighted_Rating == max(Weighted_Rating[is.finite(Weighted_Rating)]))
    
  })

  

  # Info boxes 
  
  output$averate <- renderInfoBox({
    filter_rate = df[df$Category %in% input$category,]
    ave_rate = round(mean(filter_rate$Rating, na.rm = TRUE),2)
    ave_rate_state = infoBox("Average Rating", ave_rate, icon = icon("calculator"), color = "green")
  })
  
  output$totcatinstalls <- renderInfoBox({
    filter_install = df[df$Category %in% input$category,]
    totalcat_installs = round(sum(filter_install$Installs)/1000000000, 2)
    totalcat_installs_state = infoBox("Installs in Billions", totalcat_installs, 
                                      icon = icon("download"), color = "green")
  })
  
  output$medprice <- renderInfoBox({
    filter_rate = df[df$Category %in% input$category,]
    paid = filter_rate[filter_rate$Price > 0,]
    med_price = median(paid$Price, na.rm = TRUE)
    med_price_state = infoBox("Median Paid Price", med_price, icon = icon("calculator"), color = "green")
  })
  

  
# Data Tab
  
  output$table <- DT::renderDT(
    df, filter = list(position = "top")
  )
  
# end of shinyServer 
})


