pacman::p_load(tidyverse,readxl,janitor,lubridate,hms,memoise,quanteda,SentimentAnalysis,openai,tm,shiny,shinydashboard,DT,bslib,tokenizers, gganimate)

#setwd("C:/Users/MatthewAdams/Eduserve Solutions/FPATeam - Documents/Matthew Adams/Personal Doc/Dashboard/Text Analysis Dashboard/Dashboard Demo")
#setwd("/Users/tejaswinimode/Desktop/DashboardBeta")

# API Access
Sys.setenv(
  OPENAI_API_KEY = Sys.getenv("KKey")
)

# Read master data file
#csv <- read_xlsx("rocky_masterdf.xlsx")

column_types <- c(
  rep("guess", 23),  # readxl guess the types for the first 23 columns
  "text",            # Explicitly set the 24th column to "text" 
  rep("guess", 24 - 24)  # Let readxl guess the types for any remaining columns
)

csv <- read_excel("rocky_masterdf.xlsx", col_types = column_types)

# Data preprocessing steps
master_df <- csv %>%
  clean_names() %>%
  separate(timezone, into = c("region", "city"), sep = "/", remove = TRUE) %>%
  separate(date, into = c("date", "time"), sep = 10, remove = TRUE) %>%
  separate(timestamp, into = c("date_stamp", "time_stamp"), sep = 10, remove = TRUE) %>%
  mutate(date = as_date(date)) %>%
  mutate(date_stamp = as_date(date_stamp)) %>%
  mutate(time = as_hms(time)) %>%
  mutate(time_stamp = as_hms(time_stamp)) %>%
  separate(time, into = c("time", "rm"), sep = 8, remove = TRUE) %>%
  separate(time_stamp, into = c("time_stamp", "rm"), sep = 8, remove = TRUE) %>%
  filter(interaction == "chat") %>%
  pivot_longer(cols = c("textfull", "botresponse","botquestion"), names_to = "text_type", values_to = "text") %>%
  mutate(text = iconv(text, to = "ASCII", sub = "")) %>%
  filter(text_type == "textfull") %>%
  rename(user_rating = user_assigned_ratings) %>%
  group_by(user) %>%
  mutate(short_user = paste0("user_", cur_group_id())) %>%
  relocate(short_user, .before = user) %>%
  ungroup() %>%
  select(-x1) %>%
  mutate(X = row_number()) %>%
  relocate(X, .before = short_user)

# Condense data and remove chatbot messages
user_message_df <- master_df %>%
  select(X, short_user, profile, region, city, date_stamp, time_stamp, member, text) %>%
  filter(member == "user") %>%
  select(-member) %>% 
  mutate(id = row_number())

# Create document-term matrix from user text 
dtm <- user_message_df %>% 
  corpus(text_field = "text") %>% 
  tokens(remove_punct = TRUE) %>%
  tokens_remove(stopwords("en")) %>% 
  dfm()

# Creating a sentiment dictionary using the General Inquirer dictionary
GI_dict <- dictionary(DictionaryGI)

# assign words and remove from dictionary
negative_cleaned <- setdiff(GI_dict$negative, c("get","need", "can", "want", "try", "make", "show", "bit"))

# create cleaned dictionary
clean_neg_DICT <- dictionary(list(negative = negative_cleaned))

# Create a new dictionary with cleaned negative words merged with original positive words
merged_dict <- dictionary(
  list(
    positive = GI_dict$positive,
    negative = c(clean_neg_DICT$negative)
  )
)

# Looking up sentiment terms in the DTM and compute scores
sentiment_scores <- dtm %>% 
  dfm_lookup(merged_dict) %>% 
  convert(to = "data.frame") %>% 
  as_tibble() %>% 
  mutate(length = ntoken(dtm)) %>% 
  mutate(sentiment1 = round(((positive - negative)/ (positive + negative)),2)) %>%
  mutate(sentiment2 = round(((positive - negative)/ length),2)) %>% 
  mutate(subjectivity = round((positive + negative)/ length, 2)) %>% 
  separate(doc_id, into = c("rm", "id"), sep = 4) %>%
  select(-rm) %>% 
  mutate(id = as.numeric(id))

# Merge score results with text strings
sentiment_results <- inner_join(sentiment_scores, user_message_df, by = "id") %>% 
  select(-id)

# Fine-tune for messages with more than 15 words 
filtered_sentiment_results <- sentiment_results %>% 
  filter(str_count(text, "\\S+") >= 10) %>% 
  filter(sentiment2 < 0) %>% 
  arrange(sentiment2) %>% 
  mutate(id = row_number())

# Create reference object of users for lookup in all user messages data
user_reference <- filtered_sentiment_results %>% 
  distinct(short_user)

# Use reference to pull all messages for each user
all_user_messages <- user_message_df %>% 
  mutate(test = if_else(short_user %in% user_reference$short_user, 1, 0)) %>%
  group_by(short_user) %>% 
  mutate(cumsum = sum(test)) %>% 
  filter(cumsum > 0) %>% 
  select(-test, -cumsum) %>% 
  filter(str_count(text, "\\S+") >= 3) %>% 
  ungroup() %>% 
  mutate(id = row_number())

# Create document-term matrix with all filtered user messages.  
dtm2 <- all_user_messages %>% 
  corpus(text_field = "text") %>% 
  tokens(remove_punct = TRUE) %>%
  tokens_remove(stopwords("en")) %>% 
  dfm()

# Looking up sentiment terms in the DTM and compute scores
sentiment_scores_all <- dtm2 %>% 
  dfm_lookup(merged_dict) %>% 
  convert(to = "data.frame") %>% 
  as_tibble() %>% 
  mutate(length = ntoken(dtm2)) %>% 
  mutate(sentiment1 = round(((positive - negative)/ (positive + negative)),2)) %>%
  mutate(sentiment2 = round(((positive - negative)/ length),2)) %>% 
  mutate(subjectivity = round((positive + negative)/ length, 2)) %>% 
  separate(doc_id, into = c("rm", "id"), sep = 4) %>%
  select(-rm) %>% 
  mutate(id = as.numeric(id))    

all_messages_results <- inner_join(sentiment_scores_all, all_user_messages, by = "id")

final_data <- all_messages_results %>% 
  relocate(date_stamp, .before = time_stamp) %>% 
  mutate(date_time = ymd_hms(paste(date_stamp, time_stamp))) %>% 
  relocate(date_time, .after = time_stamp)

# Assign % to filter by
y <- (round(nrow(filtered_sentiment_results)*.10, 0))

filtered_results <- filtered_sentiment_results %>% 
  head(n = y)

cleaned_data <- filtered_results %>%
  mutate(cleaned_text = tolower(text)) %>%
  mutate(cleaned_text = removePunctuation(cleaned_text)) %>%
  mutate(cleaned_text = removeNumbers(cleaned_text)) %>%
  mutate(cleaned_text = removeWords(cleaned_text, stopwords("en"))) %>%
  mutate(cleaned_text = stripWhitespace(cleaned_text)) %>% 
  select(time_stamp, region, city, text, cleaned_text)

# Create function to get text embeddings for each emotion
get_text_embedding <- memoise(function(text) {
  embedding <- create_embedding(
    model = "text-embedding-3-large",
    input = text
  )
  
  return(embedding$data$embedding[[1]])
})

# Map function to text data in data frame
embedding_result <- cleaned_data %>%
  mutate(embeddings = map(cleaned_text, get_text_embedding))

ui <- dashboardPage(
  dashboardHeader(title = "Dashboard Demo"), 
  dashboardSidebar(
    sidebarMenu(
      menuItem("User Monitoring",
               tabName = "user_tab"),
      menuItem("Keyword Search",
               tabName = "keyword_tab"),
      menuItem("Reports",
               tabName = "reports_tab")
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "user_tab",
        fluidPage(
          fluidRow(
            column(width = 3,
                   selectInput("region",
                               label = "Select Region", 
                               choices = unique(final_data$region)),
                   selectInput("city",
                               label = "Select City", 
                               choices = ""),
                   selectInput("user",
                               label = "Select User",
                               choices = NULL, 
                               selected = NULL)
            ),
            column(width = 9,
                   uiOutput("emotions_ui")
            )
          ),
          tabsetPanel(
            tabPanel(
              "Plot",
              imageOutput("sentiment_plot")
            ),
            tabPanel(
              "Table",
              div(
                style = "height: 625px; overflow-y: auto;",
                dataTableOutput("message_table")
              )
            )
          )
        )
      ),
      tabItem(
        tabName = "keyword_tab",
        sidebarLayout(
          sidebarPanel(
            textInput("keyword", "Search keyword")
          ),
          mainPanel(
            DTOutput("resultTable")
          )
        )
      ),
      tabItem(
        tabName = "reports_tab",
        sidebarLayout(
          sidebarPanel(
            selectInput("region_report", 
                        "Select Region",
                        choices = unique(final_data$region),
                        multiple = TRUE),
            selectInput("city_report", 
                        "Select City",
                        choices = "",
                        multiple = TRUE)
          ),
          mainPanel(
            DTOutput("report_data")
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  # Update City Select Input Box based on region choice
  observeEvent(
    input$region,
    updateSelectInput(
      session, 
      "city", 
      choices = unique(final_data$city[final_data$region == input$region])
    )
  )
  
  # Update City Select Input Box based on region choice
  observeEvent(
    input$city,
    updateSelectInput(
      session, 
      "user", 
      choices = unique(final_data$short_user[final_data$city == input$city & final_data$region == input$region])
    )
  )
  
  output$sentiment_plot <- renderImage({
    
    filtered_data <- final_data %>% 
      filter(region == input$region) %>% 
      filter(city == input$city) %>% 
      filter(short_user == input$user) %>% 
      mutate(date_time = ymd_hms(date_time)) %>%
      mutate(id = row_number())
    
    # create plot object
    p <- filtered_data %>%
      ggplot(aes(x = id, y= sentiment2, color = sentiment2)) + # try replacing id with date_time
      geom_point(aes(group = seq_along(id))) +
      geom_line(linewidth = 3) +
      geom_hline(yintercept = 0) +
      scale_color_gradient2(low = "tomato", mid = "yellow", high = "darkolivegreen2", midpoint = 0, guide = "legend")+
      guides(color = guide_legend(title = "Sentiment")) +
      labs(title = "User Messages - Time Stamp: {frame_along}",
           x = "Time",
           y = "Sentiment Score") +
      transition_reveal(id) + # try replacing id with date_time
      view_follow(fixed_y = TRUE)
    
    # Save the animation to a file (e.g., "outfile.gif")
    anim_save("outfile.gif", animate(p, fps = 20,  width = 1000))
    
    # Return a list containing the filename
    list(src = "outfile.gif", contentType = "image/gif")
  }, deleteFile = TRUE)  # Specify deleteFile argument
  
  output$message_table <- renderDataTable({
    
    filtered_data <- final_data %>% 
      filter(region == input$region) %>% 
      filter(city == input$city) %>% 
      filter(short_user == input$user) %>% 
      mutate(id = row_number())
    
    summary_table <- filtered_data %>%
      select(text, date_time, sentiment2) %>%
      relocate(date_time, .before = text) %>%
      relocate(sentiment2, .after = date_time) %>%
      rename('Sentiment Score' = sentiment2) %>%
      rename(Text = text) %>%
      rename(Time = date_time) %>% 
      datatable(
        options = list(pageLength = 10,
                       searching = FALSE,
                       paging = FALSE,
                       scrolling = TRUE,
                       initComplete = JS("function(settings, json) {
        $(this.api().table().container()).css({'background-color': 'white'});
                          }")))
  })
  
  
  output$emotions_ui <- renderUI({
    
    filtered_data <- final_data %>% 
      filter(region == input$region) %>% 
      filter(city == input$city) %>% 
      filter(short_user == input$user) %>% 
      mutate(id = row_number())
    
    if (nrow(filtered_data) == 0) {
      return(NULL)  # Return NULL if no data is available
    }
    
    cleaned_data <- filtered_data %>%
      arrange(sentiment2) %>%
      slice(1) %>%
      mutate(cleaned_text = tolower(text)) %>%
      mutate(cleaned_text = removePunctuation(cleaned_text)) %>%
      mutate(cleaned_text = removeNumbers(cleaned_text)) %>%
      mutate(cleaned_text = removeWords(cleaned_text, stopwords("en"))) %>%
      mutate(cleaned_text = stripWhitespace(cleaned_text))
    
    # Check if cleaned_text is empty
    if (nchar(cleaned_data$cleaned_text[1]) == 0) {
      return(NULL)  # Return NULL if cleaned text is empty
    }
    
    # Extracting text from the first row
    text <- cleaned_data$cleaned_text[1]
    
    # Creating embedding for the text
    embeddings <- create_embedding(
      model = "text-embedding-3-large",
      input = text
    )
    
    # Assigning the embedding to the dataframe as a list
    cleaned_data <- cleaned_data %>%
      mutate(embeddings = list(embeddings$data$embedding[[1]]))
    
    
    # Define emotions for classification
    emotions <- c("joy", "suprise", "anger", "sadness", "fear", "disgust", "hope", "love", "envy", "guilt",
                  "shame", "excitement", "anticipation", "contempt", "confusion", "trust", "doubt", "regret", "pride", "relief")
    
    # Create function to get text embeddings for each emotion
    get_emotion_embeddings <- memoise(function(emotion) {
      embeddings <- create_embedding(
        model = "text-embedding-3-large",
        input = emotion
      )
      return(embeddings$data$embedding[[1]])
    })
    
    # Map function over list of emotions
    emotion_embeddings <- map(emotions, get_emotion_embeddings)
    
    compute_similarity <- function(text_embedding, emotion_embedding) {
      dot_product <- sum(text_embedding * emotion_embedding)
      magnitude_vec1 <- sqrt(sum(text_embedding^2))
      magnitude_vec2 <- sqrt(sum(emotion_embedding^2))
      similarity <- round((dot_product / (magnitude_vec1 * magnitude_vec2)), 4)
      return(similarity)
    }
    
    # Create column for each emotion that contains cosine score with corresponding user text
    emotion_results <- cleaned_data %>%
      rowwise() %>%
      mutate(
        cosine_joy = compute_similarity(embeddings, emotion_embeddings[[1]]),
        cosine_suprise = compute_similarity(embeddings, emotion_embeddings[[2]]),
        cosine_anger = compute_similarity(embeddings, emotion_embeddings[[3]]),
        cosine_sadness = compute_similarity(embeddings, emotion_embeddings[[4]]),
        cosine_fear = compute_similarity(embeddings, emotion_embeddings[[5]]),
        cosine_disgust = compute_similarity(embeddings, emotion_embeddings[[6]]),
        cosine_hope = compute_similarity(embeddings, emotion_embeddings[[7]]),
        cosine_love = compute_similarity(embeddings, emotion_embeddings[[8]]),
        cosine_envy = compute_similarity(embeddings, emotion_embeddings[[9]]),
        cosine_guilt = compute_similarity(embeddings, emotion_embeddings[[10]]),
        cosine_shame = compute_similarity(embeddings, emotion_embeddings[[11]]),
        cosine_excitement = compute_similarity(embeddings, emotion_embeddings[[12]]),
        cosine_anticipation = compute_similarity(embeddings, emotion_embeddings[[13]]),
        cosine_contempt = compute_similarity(embeddings, emotion_embeddings[[14]]),
        cosine_confusion = compute_similarity(embeddings, emotion_embeddings[[15]]),
        cosine_trust = compute_similarity(embeddings, emotion_embeddings[[16]]),
        cosine_doubt = compute_similarity(embeddings, emotion_embeddings[[17]]),
        cosine_regret = compute_similarity(embeddings, emotion_embeddings[[18]]),
        cosine_pride = compute_similarity(embeddings, emotion_embeddings[[19]]),
        cosine_relief = compute_similarity(embeddings, emotion_embeddings[[20]])
      ) %>%
      select(-cleaned_text, -embeddings)
    
    # Create summary data with user text and the three emotions most associated with it
    emotion_summary <- emotion_results %>%
      select(17:36) %>%
      pivot_longer(cols = 1:20, names_to = "names", values_to = "values") %>%
      separate(names, into = c("cos", "emotion"), sep = "_") %>%
      mutate(emotion = str_to_title(emotion)) %>% 
      select(-cos) %>%
      arrange(emotion, -values) %>%
      mutate(max_value_index = which.max(values)) %>%
      mutate(second_max_value_index = which.max(lag(values))) %>%
      mutate(third_max_value_index = which.max(lag(values, 2))) %>%
      mutate(primary_emotion = emotion[max_value_index]) %>%
      mutate(secondary_emotion = emotion[second_max_value_index]) %>%
      mutate(tertiary_emotion = emotion[third_max_value_index]) %>%
      select(-max_value_index, -second_max_value_index, -third_max_value_index) %>%
      pivot_wider(id_cols = c("primary_emotion", "secondary_emotion", "tertiary_emotion"),
                  names_from = "emotion",
                  values_from = "values") %>%
      select(1:3)
    
    # Extract primary emotion from the first row of emotion_summary
    valueBox(
      "Alerted Emotions",
      emotion_summary,
      icon = icon("exclamation"),
      color = "red"
    )
    
  })
  
  output$resultTable <- renderDT({
    
    # Create function to get text embeddings for each emotion
    get_keyword_embedding <- memoise(function(keyword) {
      embedding <- create_embedding(
        model = "text-embedding-3-large",
        input = keyword
      )
      
      return(embedding$data$embedding[[1]])
    })
    
    # Map function over list of emotions
    keyword_embedding <- map(input$keyword, get_keyword_embedding)
    
    compute_similarity <- function(text_embedding, keyword_embedding) {
      
      dot_product <- sum(text_embedding * keyword_embedding)
      magnitude_vec1 <- sqrt(sum(text_embedding^2))
      magnitude_vec2 <- sqrt(sum(keyword_embedding^2))
      
      similarity <- round((dot_product / (magnitude_vec1 * magnitude_vec2)), 4)
      
      return(similarity)
    }
    
    r <- embedding_result %>%
      rowwise() %>%
      mutate(keyword_cosine = compute_similarity(embeddings, keyword_embedding[[1]])) %>% 
      select(-embeddings, -cleaned_text) %>% 
      arrange(-keyword_cosine) %>% 
      head(10) %>% 
      datatable(
        options = list(searching = FALSE,
                       paging = FALSE,
                       scrolling = TRUE,
                       initComplete = JS("function(settings, json) {
        $(this.api().table().container()).css({'background-color': 'white'});
                          }")))
    
  })
  
  
  ## Server functions for Reports tab
  
  # Update City Select Input Box based on region choice
  observeEvent(
    input$region_report,
    updateSelectInput(
      session, 
      "city_report", 
      choices = unique(final_data$city[final_data$region %in% input$region_report])
    )
  )
  
  report_data <- filtered_sentiment_results %>% 
    arrange(sentiment2)
  
  # Assign % to filter by
  y <- (round(nrow(report_data)*.10, 0))
  
  report_data <- report_data %>% 
    head(n = y)
  
  alert_ref <- report_data %>% 
    group_by(city) %>% 
    summarise(n_alerts = n_distinct(text))
  
  output$report_data <- renderDT({
    
    # Create report for viewing summaries by region and city
    sentiment_results %>% 
      filter(region %in% input$region_report) %>% 
      filter(city %in% input$city_report) %>% 
      filter(length >= 3) %>% 
      group_by(region, city) %>% 
      summarise(user_count = n_distinct(short_user), 
                mean_sentiment = round(mean(sentiment2), 3)) %>% 
      left_join(alert_ref, by = "city") %>% 
      mutate(n_alerts = if_else(is.na(n_alerts), 0, n_alerts)) %>% 
      relocate(n_alerts, .after = user_count) %>% 
      mutate(alerts_per_user = if_else(n_alerts > 0, round(n_alerts/user_count, 3), 0))
    
  })
}

shinyApp(ui, server) 
