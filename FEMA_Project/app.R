library(shiny)
library(tidyverse)
library(maps)
library(tmap)
library(sp)
library(rgeos)
library(sf)
library(shinythemes)
library(shinycssloaders)

#Read dataset
id <- "1S5mbXldWZ8TpMHhuJQFMtv-uqpZXQQrv"
disaster <- read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download", id))

id2 <- "1s-fCq97ttzvFDYfDofWD8kmBzj3AxgLP"
detail <- read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download", id2))


#data processing
#Get name by county.fips
county_fips <- county.fips

#Get state name + county name
disaster <- disaster %>%
  mutate(fips = fipsStateCode * 1000 + fipsCountyCode)
disaster <- left_join(disaster,county_fips, by = "fips")
disaster <- disaster %>% 
  select(c(26,24,2,4,5,11)) %>%
  mutate(ID = polyname) %>%
  separate(polyname,c("state","county"),sep = ",")
disaster <- na.omit(disaster)

#select states 
StatesInt <- state.name
StatesInt <- str_to_lower(StatesInt,locale = "en")

#Get county map
county <- st_as_sf(maps::map("county",StatesInt,plot=F,fill=T))

button_color_css <- "{
background: DodgerBlue;
font-size: 15px;
}"


# Define UI
ui <- fluidPage(

  #Navbar structure for UI
  navbarPage("FEMA Disasters", theme = shinytheme("lumen"),
            tabPanel("Disaster Map", fluid = TRUE, icon = icon("globe-americas"),
                     tags$style(button_color_css),
                     
                     # Sidebar layout with a input and output definitions
                     sidebarLayout(
                       sidebarPanel(
                         
                          titlePanel("Declared Hurricane Search"),
                          
                          fluidRow(column(6,
                                          
                                          #Select which year
                                          selectInput(inputId = "YearSelect",
                                                      label = "Year",
                                                      choices = sort(unique(disaster$year)),
                                                      selected = "2009",
                                                      width = '400px'
                                          ),
                                          
                                          uiOutput("StateOutput")
                                          ,
                                          
                                          uiOutput("DisasterNumber")
                                          
                                          )
                      
                                  )
                         
                         
                         
                         
                         
                                    ),#End of sidebarPanel
                       mainPanel(
                         fluidRow(
                              column(3, offset = 9,
                                     
                                     radioButtons(inputId = "VariableSelect",
                                                  label = "Display",
                                                  choices = c("Public Assistance Project",
                                                              "Project Number",
                                                              "Federal Share Obligated"),
                                                  selected = "Public Assistance Project")
                                
                                     )
                                  ),
                                    withSpinner(plotOutput(outputId = "apMap"))
                         
                                )
                       
                                  )
              
                    )
             
             
            )
    
)

# Define server logic
server <- function(input, output) {
  
  #Creaet selectInput to choose State
  output$StateOutput <- renderUI({
    #Get sub data by input
    disaster_sub <- disaster %>% 
      filter(year==input$YearSelect)
    
    choice <- sort(intersect(unique(disaster_sub$state),str_to_lower(state.name,locale = "en")))
    
    #Create another reactive input bar
    selectInput(inputId = "StateSelect",
                label = "State",
                choices = choice,
                width = '400px')
  })
  
  
  
  #Creaet selectInput to choose DisasterNumber
  output$DisasterNumber <- renderUI({
      #Get sub data by input
      disaster_sub <- disaster %>% 
                        filter(year==input$YearSelect,
                               state==input$StateSelect)
      choice <- sort(unique(disaster_sub$disasterNumber))
    
      #Create another reactive input bar
      selectInput(inputId = "DisasterNumberSelect",
                  label = "Disaster Number",
                  choices = choice,
                  width = '400px'
                 )
  })
  
  #Create Plot
  output$apMap <- renderPlot({
    #Get filtered data
    
    disaster_sub <- disaster %>% 
      filter(year==as.integer(input$YearSelect),
             state==input$StateSelect,
             disasterNumber==as.integer(input$DisasterNumberSelect),
             paProgramDeclared==1)
    
    detail_sub <- detail %>%
      filter(disasterNumber==as.integer(input$DisasterNumberSelect)) %>%
      mutate(ID = str_c(state,county,sep = ","))
    
    detail_sub2 <- detail_sub %>%
      group_by(ID) %>%
      summarise(total = sum(totalObligated))
    
    detail_sub <- left_join(detail_sub,count(detail_sub,ID),by = "ID")
    detail_sub <- left_join(detail_sub,detail_sub2,by = "ID")
    
    detail_sub <- detail_sub %>% 
      select(c(13,14,15)) %>%
      rename("Project Number" = "n","Federal Share Obligated" = "total") %>%
      mutate(ID = str_to_lower(ID,locale="en"))
    
    #Choose by condition
    county_sub <- subset(county,grepl(input$StateSelect,county$ID))
    
    #Combine
    county_sub <- left_join(county_sub,disaster_sub,by = "ID")
    
    county_sub <- left_join(county_sub,detail_sub,by = "ID")
    
    #Deal with PA
    county_sub[is.na(county_sub)] <- 0
    
    county_sub$`Designated Counties` <- ifelse(county_sub$paProgramDeclared==1,"Public Assistance","No Designation")
    
    #Get x,y coordinates
    county_sub <- cbind(county_sub, st_coordinates(st_centroid(county_sub)))
    
    #Get county name
    county_sub <- county_sub %>%
      separate(ID,c("state","county"),sep = ",")
    
    #Filter again
    county_sub <- county_sub %>%
                    filter(state==input$StateSelect)
    
    #Set color depends on result
    ##PA
    col <- c("white","lightgoldenrod1")
    list <- unique(county_sub$paProgramDeclared)
    if(length(list)==1 & list[1]==1){col <- c("lightgoldenrod1","white")}
    
    ##Number
    col2 <- "red"
    list2 <- unique(county_sub$Project.Number)
    if(length(list2)==1 & list2[1]==0){col2 <- "grey90"}
    
    ##Fund
    col3 <- "red"
    list3 <- unique(county_sub$Federal.Share.Obligated)
    if(length(list3)==1 & list3[1]==0){col3 <- "grey90"}
    
    #Plot
    ##Creaet title
    b <- unique(county_sub$disasterNumber)
    b <- b[str_length(b)>2]
    c <- unique(county_sub$declarationType)
    c <- c[str_length(c)>1]
    d <- str_sub(unique(county_sub$declarationDate),1,10)
    d <- d[str_length(d)>2]
    e <- unique(county_sub$state)
    e <- e[str_length(e)>2]
    title <- str_c("FEMA-",b,"-",c,", ",str_to_title(e)," Disaster Declaration as of ",d,sep = "")
    
    ##Creaet plot
    if(input$VariableSelect == "Public Assistance Project"){
    ggplot() +
      geom_sf(data = county_sub,aes(fill = Designated.Counties)) +
      scale_fill_manual(values = col) +
      ggtitle(title) +
      geom_text(data = county_sub, aes(X, Y, label = county),family="Times New Roman", size = 2, fontface = "bold") +
      theme(axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.x=element_blank(),
            axis.text.y=element_blank(),
            legend.title = element_text(size = 10),
            legend.text = element_text(size = 10),
            plot.title = element_text(hjust = 0.5, face = "bold", size = 10),
            text = element_text(family="Times New Roman")
      )
    }
    else if(input$VariableSelect == "Project Number"){
      ggplot() +
        geom_sf(data = county_sub,aes(fill = Project.Number)) +
        scale_fill_gradient(low="white", high=col2) +
        ggtitle(title) +
        geom_text(data = county_sub, aes(X, Y, label = county),family="Times New Roman", size = 2, fontface = "bold") +
        theme(axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              axis.text.x=element_blank(),
              axis.text.y=element_blank(),
              legend.title = element_text(size = 10),
              legend.text = element_text(size = 10),
              plot.title = element_text(hjust = 0.5, face = "bold", size = 10),
              text = element_text(family="Times New Roman")
        )
    }
    else if(input$VariableSelect == "Federal Share Obligated"){
      ggplot() +
        geom_sf(data = county_sub,aes(fill = Federal.Share.Obligated)) +
        scale_fill_gradient(low="white", high=col3) +
        ggtitle(title) +
        geom_text(data = county_sub, aes(X, Y, label = county),family="Times New Roman", size = 2, fontface = "bold") +
        theme(axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              axis.text.x=element_blank(),
              axis.text.y=element_blank(),
              legend.title = element_text(size = 10),
              legend.text = element_text(size = 10),
              plot.title = element_text(hjust = 0.5, face = "bold", size = 10),
              text = element_text(family="Times New Roman")
        )
      
    }
    
  },
  #Set Size
  height = 550,
  width = 700
  )
}

# Run the application 
shinyApp(ui = ui, server = server)
