# Jr Version - Summary Version
# which one do users like more, to create scenarios, compare scenarios, or share scenarios?
# Jun 10, 2019
#
# App organization:
# UI - 3 inputs: 
#    selectInput - Degree Category, 
#    selectInput - Occupation Category, 
#    selectInput - School Category
# 
# Each input should filter from the other inputs
# Each input should filter the data table output
# Graph output should include average cost & salary for next 20 years?

library(shiny)
library(dplyr)
library(readxl)
library(DT)
library(readr)
library(tidyr)


# import master1
# master1 <- read.csv("https://www.dropbox.com/s/fgty42qwpkzudwz/master1.txt?dl=1")
# master1 <- read_csv("~/Dropbox/R-Studio/R_Training/Master1.zip")
# save(master1,file="data/master1.Rda")
# master2 <- load(file ="data/master1.Rda") # loading is not working yet.


# import cip_code
# cip_code <- read.delim("~/Dropbox/R-Studio/OccupationFromDegree/jrversion/data/cip_code.txt")
# names(cip_code) <- tolower(names(cip_code))

# import soc_code
# soc_code <- read.delim("~/Dropbox/R-Studio/OccupationFromDegree/jrversion/data/soc_code.txt")
# names(soc_code) <- tolower(names(soc_code))
#
# removed extra row in SOC table, which was NA for all variables
# soc_code <- soc_code[-1,]



# import socToCip
# SOC - CIP crosswalk source: https://nces.ed.gov/ipeds/cipcode/resources.aspx?y=55
# socToCip <- read_excel("jrversion/data/FINALSOCtoCIPcrosswalk_022811.xls", skip = 3)
#
# change variable names to lower case
# names(socToCip) <- tolower(names(socToCip))
#
# add soc prefix and cip prefix columns to socToCip
# socToCip$soc.cat <- substr(socToCip$soc2010code,1,2)
# socToCip$cip.cat <- substr(socToCip$`cip2010 code`,1,2)




# join socToCip and Cip category titles
 socToCipTitles <- left_join(socToCip, cip_code, by = "cip.cat")

# place cip.cat.title into socToCip table
# socToCip$cip.cat.title <- socToCip %>% left_join(cip, by = "cip.cat")


ui <- fluidPage(

    # Application title
    titlePanel("Which occupations require which degrees?"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("occupationCategory",
                        "Which occupation(s) category would you like to explore?",
                        # choices = occCatSelect()
                        choices = levels(as.factor(socToCipTitles$soc2010title)),
                        multiple = TRUE
                        )
            # selectInput("degreeCategory",
            #             "Which Degree would you like to obtain?",
            #             choices = unique(cip$cip_category)
            #             )
        ),

        # Show a data table
        mainPanel(
            HTML(paste0("Select an occupation (on the left) to see which degrees lead to your chosen occupation:")),
            DT::dataTableOutput(outputId = "table")

        )
    )
)

# Define server logic 
server <- function(input, output) {

    output$table <- renderDT({
        DT::datatable(
            socToCipTitles %>% 
            select(soc2010title, cip_category) %>% 
            filter(soc2010title %in% input$occupationCategory) %>% 
            group_by (cip_category),
            options = list(pageLength = 50)
        )
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
