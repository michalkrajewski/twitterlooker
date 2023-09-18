#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#



library(shiny)

library(stringr)
library(dplyr)

library("rtweet")

library(rsconnect)

library(polished)

library(DT)

#library(polishedpayments)

library(tibble)

library(tidyr)

# configure polished auth when the app initially starts up.
polished_config(
  app_name = "rarog2",
  api_key = "msaCWuxV2VPJve17cgjVpGbEMN9cqrsYY2"
)

polished_payments_config(
  stripe_secret_key = "sk_test_51M54bgJz0syr1lITBkKMFlz2WtpbHn0wVpUe454dsoScI0rtekb5UkGpastCd1muhQJvqyACEMNhwGrzRmBxhzMa00Kr3isj9Z",
  stripe_public_key = "pk_test_51M54bgJz0syr1lIT89On3FYf6aM8Q7sscLXxcVy4mYwZeXfAWxazT24tq0iWeKej6KQ2G81xhFvhFJM5cLyPTPBA00SkLut0Z5"
)

auth <- rtweet_app("AAAAAAAAAAAAAAAAAAAAAM3QhgEAAAAAYFanxtleJ0MEZfi3dx%2Fqu7gFxYo%3DBawjS8VA5RCGAdz2afUJ044rN7H4kXBlInQhgsReN5JxAUg0lT")
auth_save(auth, "default")
auth_as("default")

auth_setup_default()

ui <- fluidPage(
  
  # Application title
  titlePanel("Rarog2 v.0.2"),
  
  
  # Sidebar
  sidebarLayout(
    sidebarPanel(

      
      
      
      helpText("Please connect to twitter API first"),
      
      #actionButton("connect", "Connect"), 
      
      actionButton("check_rate_limit", "Check rate limit"), 
      
      hr(),
      
      helpText("Find followers that follow:"),
      
      textInput("text", label = NULL, value = "Enter one Twitter ID"),
      
      selectInput("select_type", "and", choices = c("follow", "does not follow"), width = '50%'),
      
      textInput("text2",label = NULL, value = "Enter one Twitter ID"),
      
      actionButton("get_list", "Get the initial List"),
      
      br(),
      
      helpText("Negative keywords - accounts with those keywords in bio will be excluded"),
      textInput("negative_keywords", label = NULL, value = NULL),
      
      verbatimTextOutput("value"),
      
      helpText("Positive keywords - accounts with those keywords in bio will be included"),
      textInput("positive_keywords", label = NULL, value = NULL),

      checkboxInput("inCheckbox", "Get the dates of latest interaction - max for 1500 results",
                     value = TRUE),
      
      
      actionButton("clean_list", "Clean the List"),
      
      downloadButton("download"),

      
      # create_payment_module_ui("pay_10"),
      # 
      # free_trial_banner_module_ui("trial_banner"),
      # 
      # verbatimTextOutput("polished_subscription"),
      # 
      # verbatimTextOutput("polished_user"),
      
      hr(),
      
      actionButton(
        "sign_out",
        "Sign Out",
        icon = icon("sign-out-alt")
        #,class = "pull-right"
      ),
      
      actionLink(
        "go_to_payments",
        "Payments",
        icon = icon("credit-card"),
        style = "display: inline-block; margin-top: 25px;",
        class = "pull-right"
      ),
      
      br()
      
    ),
    
    # Show the table 
    mainPanel(
      
      helpText("This is your rate limit"),
      tableOutput("ratelimit"),
      
      helpText("Number of twitter followers on your list"),
      textOutput("followerslist"),
      
      textOutput("negativity"),
      
      textOutput("positivity"),
      
      DT::dataTableOutput("view")
      
      
    )
  )
)


# Define server logic

server <- function(input, output, session) {

  output$value <- renderText({ as.character(input$negative_keywords) })
 
  observeEvent(input$go_to_payments, {
    polishedpayments::go_to_payments()
  })
  
  output$polished_subscription <- renderPrint({
    session$userData$stripe()
  })
  
  callModule(
    free_trial_banner_module,
    "trial_banner"
  )
  
  payment_return <- shiny::callModule(
    create_payment_module,
    "pay_10",
    amount = 1000,
    send_receipt_email = FALSE,
    description = "a $10 one time payment"
  )
  
  observeEvent(payment_return$payment_response(), {
    
    print(list(
      payment_response = payment_return$payment_response()
    ))
  })
  
  ###
  
  output$secure_content <- renderPrint({
    session$userData$user()
  })
  
  observeEvent(input$sign_out, {
    sign_out_from_shiny(session)
    session$reload()
  })
  
  
  observeEvent(input$connect,{
     
    auth <- rtweet_user(api_key = NULL, api_secret = NULL)
    auth_save(auth, "user")
    auth_as("user")

  })
  
  
  joinedFollowers <- eventReactive(input$get_list, {
    
    user_1 <- get_followers(input$text, n = 30000, 
                            retryonratelimit = TRUE)
    
    user_2 <- get_followers(input$text2, n = 30000, 
                            retryonratelimit = TRUE)
    if(input$select_type == "follow") {
      
      joinedFollowers <- inner_join(user_1, user_2, by="from_id")
    } else {
      
      joinedFollowers <- anti_join(user_1, user_2, by="from_id")
      
    }
    
    
    joinedFollowers
    
  }, ignoreNULL = FALSE)
  
  
  rt <- eventReactive(input$check_rate_limit, {
    
    rt <- as.data.frame(rate_limit()) %>% filter(resource %in% c("/users/lookup", "/followers/ids", "/statuses/user_timeline"))
  })
  
  
  output$ratelimit <- renderTable(
    
  rt()

  )
  
  output$followerslist <- renderText(nrow(joinedFollowers()))
  
  
  
  
  
  datasetInput <- eventReactive(input$clean_list, {
    
    joinedFollowers <- joinedFollowers()
    
    datasetInput <- lookup_users(joinedFollowers$from_id, verbose = FALSE)
    
    datasetInput <- datasetInput %>% select("name", "screen_name", "location" , "description", "url", "protected", "followers_count", "friends_count", "listed_count", "created_at", "favourites_count", "verified", "statuses_count", "profile_image_url_https", "default_profile", "default_profile_image")
    
    datasetInput %>% filter(protected==0) %>%
    # filter(!(grepl(isolate(as.character({ input$negative_keywords })), description))) %>%
    #   filter(description %in% c(input$positive_keywords)) %>%
      filter(statuses_count > 1)
    

    
    if(input$inCheckbox == TRUE){ 
    usernames <- c(datasetInput$screen_name)
    
    results <- data.frame()
    
    for (username in usernames) {
      # Use the search_tweets() function to search for the latest interaction
      # Replace "n" with the number of tweets you want to retrieve (the default is 20)
      latest_interaction <- get_timeline(username, n = 1)
      
      # Extract the date of the latest interaction from the data frame
      date <- latest_interaction$created_at[1]
      
      # Add a new row to the results data frame with the username and date of the latest interaction
      results <- rbind(results, data.frame(username = username, "latest interaction" = date))
    }
    
    datasetInput <- left_join(datasetInput, results, by = c("screen_name"="username"))
    
    } else {
      datasetInput
      }
    
  }, ignoreNULL = FALSE)
  
  ###
  

  
  observeEvent(input$negative_keyboard, {
    datasetInput <- datasetInput()
    
    datasetInput <- datasetInput %>% filter(grep(input$negative_keywords, description))
    
    datasetInput
  })
  
  ###
  output$view <- DT::renderDataTable({
    
    DT::datatable(dataset <- datasetInput(), options = list(lengthMenu = c(5, 30, 50), pageLength = 5))
    
  })

  ###
  output$download <- downloadHandler(
    filename = function() {
      paste("followers_list_output", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(datasetInput(), file, row.names = FALSE)
    }
  )
  
}







shinyApp(
  ui = payments_ui(ui) %>% secure_ui(
    ui,
    sign_in_page_ui = sign_in_ui_default(color = "#FFD700",
                                         company_name = "Rarog",
                                         logo_top =
                                           tags$h1("Rarog"),
                                         
    )),
  server = payments_server(server) %>% secure_server()
)

