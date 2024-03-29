shinyApp(
  ui = pageWithSidebar(
    headerPanel("Occupation and Degree App"),
    sidebarPanel(
      uiOutput("select_var1"), 
      uiOutput("select_var2"),
      uiOutput("select_var3"),
      uiOutput("select_var4"),
      uiOutput("select_var5")
    ),
    
    mainPanel(
      tableOutput("table")
    )
  ),
  
  server = function(input, output, session) {
    
    tab <- reactive({ 
      
      tib %>% 
        filter(var_one == input$var1) %>% 
        filter(var_two == input$var2) %>% 
        filter(var_three == input$var3) %>% 
        filter(var_four == input$var4) %>% 
        filter(var_five == input$var5)
      
    })
    
    output$select_var1 <- renderUI({
      
      selectizeInput('var1', 'Select variable 1', choices = c("select" = "", levels(tib$var_one)))
      
    })
    
    output$select_var2 <- renderUI({
      
      
      choice_var2 <- reactive({
        tib %>% 
          filter(var_one == input$var1) %>% 
          pull(var_two) %>% 
          as.character()
        
      })
      
      selectizeInput('var2', 'Select variable 2', choices = c("select" = "", choice_var2())) # <- put the reactive element here
      
    })
    
    output$select_var3 <- renderUI({
      
      choice_var3 <- reactive({
        tib %>% 
          filter(var_one == input$var1) %>% 
          filter(var_two == input$var2) %>% 
          pull(var_three) %>% 
          as.character()
        
      })
      
      selectizeInput('var3', 'Select variable 3', choices = c("select" = "", choice_var3()))
      
    })
    
    output$select_var4 <- renderUI({
      
      choice_var4 <- reactive({
        tib %>% 
          filter(var_one == input$var1) %>% 
          filter(var_two == input$var2) %>% 
          filter(var_three == input$var3) %>% 
          pull(var_four) %>% 
          as.character()
        
      })
      
      selectizeInput('var4', 'Select variable 4', choices = c("select" = "", choice_var4()))
      
    })
    
    output$select_var5 <- renderUI({
      
      choice_var5 <- reactive({
        tib %>% 
          filter(var_one == input$var1) %>% 
          filter(var_two == input$var2) %>% 
          filter(var_three == input$var3) %>% 
          filter(var_four == input$var4) %>% 
          pull(var_five) %>% 
          as.character()
        
      })  
      
      selectizeInput('var5', 'Select variable 5', choices = c("select" = "", choice_var5()))
      
    })
    
    output$table <- renderTable({ 
      
      tab()
      
    })
    
  },
  
  options = list(height = 500)
  
)