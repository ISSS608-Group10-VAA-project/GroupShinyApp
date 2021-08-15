library(tidyverse)
library(tools)
library(ggraph)
#library(DT)
library(igraph)
library(tidygraph)
library(lubridate)
library(clock)
library(readr)
library(visNetwork)
#library(hms)
library(shiny)
library(shinythemes)



#add
library(shiny)
library(tidyverse)
library(tools)
library(thematic)
library(shinythemes)

#Jessica
library(shiny)
library(tidyverse)
library(shinythemes)
library(tidygraph)
library(ggraph)
library(DT)

#Julian
library(igraph)
library(tidygraph)
library(lubridate)
library(clock)
library(readr)
library(visNetwork)
library(tidyverse)
library(shiny)
library(DT)

#Suling
library(tidyverse)
library(visNetwork)
library(igraph)
library(shiny)
library(hms)
library(DT)
library(shinyTime)
library(shinydashboard)



#load dataset here

#Email data import
GASTechgraph <- read_rds("data/GASTechgraph.rds") %>% mutate(bc=centrality_betweenness())
employee <- read_rds("data/employee_background.rds")



#Spending-locations data import and preprocessing

cc_data <- read_csv("data/cc_data.csv")
cc_data$location <- iconv(cc_data$location, "UTF-8","ASCII",  sub="")

cc_data$timestamp <-  date_time_parse(cc_data$timestamp,
                                      zone = "",
                                      format = "%m/%d/%Y %H:%M")

cc_data$last4ccnum <- as.character(cc_data$last4ccnum)
cc_data$Day  = get_day(cc_data$timestamp)
cc_data$Hour  = get_hour(cc_data$timestamp)


##create node list(with two columns:id,label)
sources <- cc_data %>%
  distinct(last4ccnum) %>%
  rename(label = last4ccnum)
destinations <- cc_data %>%
  distinct(location) %>%
  rename(label = location)


cc_nodes <- full_join(sources, 
                      destinations, 
                      by = "label")

cc_nodes <- cc_nodes %>% 
  rowid_to_column("id")


cc_nodes$group= c(rep("Card Holder", 55), rep("Location", 34))
cc_nodes$title= paste(cc_nodes$label)

nodes_table<- select(cc_nodes,id,label,group)



##create edge list(with columns:from, to,day,weight)
edges <- cc_data %>%  
  group_by(last4ccnum, location,Day,Hour) %>%
  summarise(Weight = n()) %>% 
  ungroup()

cc_edges <- edges %>% 
  left_join(cc_nodes, 
            by = c("last4ccnum" = "label")) %>% 
  rename(from = id)

cc_edges <- cc_edges %>% 
  left_join(cc_nodes, 
            by = c("location" = "label")) %>% 
  #rename(to = id)
  mutate(to=id)

cc_edges <- select(cc_edges, from, to,location, Day,Hour,
                   Weight)

#cc_edges$title= paste('spending:',cc_edges$price)

##create graph object
cc_graph <- tbl_graph(nodes = cc_nodes, 
                      edges = cc_edges, 
                      directed = FALSE)

##allocate node type
V(cc_graph)$type <- bipartite_mapping(cc_graph)$type


#Message tab
nodes <- read.csv("data/nodes.csv")
edges <- read.csv("data/edges.csv")





ui <- fluidPage(
  
  theme=shinytheme("flatly"), 
  
  
  navbarPage(
    
    "VAST2021 Network Analysis",  
    
    #Email tab
    tabPanel("Email",  
             
             titlePanel("Email Network Visualisation"),
             
             # Sidebar with a slider input for number of bins 
             sidebarLayout(
               sidebarPanel(

                 sliderInput(inputId = "bc",
                             label = "Centrality Betweenness",
                             min = 0,
                             max = 300,
                             value = TRUE),
                 sliderInput(inputId = "Weight",
                             label = "Weight",
                             min = 0,
                             max = 30,
                             value = TRUE),
                 checkboxInput(inputId = "employee_background",
                               label = "Show employee records",
                               value = TRUE),
                 actionButton("goButton","Go!"),
                 width=3
               ),
               mainPanel(
                 plotOutput("GASTechgraph"),
                 DT::dataTableOutput(outputId = "employeeTab"),
                 width=9
               )
             )
    ),
    
    #spending locations TAB
    tabPanel("Spending locations", 
             
             titlePanel("Subject Correlation Analysis"),
             sidebarLayout( 
               sidebarPanel(    
                 selectInput('DAY','select day',
                             unique(cc_data$Day)),                 
                 selectInput('HOUR','select hour',
                             unique(cc_data$Hour)),
                 checkboxInput('nodes_checkbox','show nodes table',
                               TRUE),
                 checkboxInput('record_checkbox','show spending-location record',
                               FALSE)
               ),
               mainPanel(
                 visNetworkOutput("bipartite", width = '100%'),
                 dataTableOutput("nodes_table"),
                 dataTableOutput("cc_data")

                 
               )
             )
    ),
    
    #Text Messages TAB
    tabPanel("Text Messages", 
             
             titlePanel("Network graph of Text Messages"),
             sidebarLayout(
               sidebarPanel(
                 
                 selectizeInput(inputId = "edge",
                                label = "Select Data:",
                                choices = c("All" = "is_all",  
                                            "Only MicroBlog data" = "is_mbdata",
                                            "Has RTs" = "is_RT",
                                            "Has Mentions" = "is_mention",
                                            "Without Mentions" = "not_mention",
                                            "Has Links" = "is_link",
                                            "Has Hashtags" = "is_hashtag"),
                                selected = "is_mbdata"),
                 
                 selectizeInput(inputId = "group",
                                label = "Select Group:",
                                choices = c("All" = "is_all1",
                                            "News Group" = "is_news",
                                            "Official Channels" = "is_official",
                                            "CallCentre" = "is_ccdata",
                                            #"Spam" = "is_spam",
                                            "Interest Group" = "is_interest",
                                            "Others" = "is_others"),
                                selected = "is_news"),
                 
                 sliderInput(inputId = "time", 
                             label = "Select time range:",
                             min = as.POSIXct("2014-01-23 17:00:00"),
                             max = as.POSIXct("2014-01-23 21:34:45"),
                             value = c(as.POSIXct("2014-01-23 18:30:00"),
                                       as.POSIXct("2014-01-23 19:30:00")),
                             step = 300,
                             animate = TRUE
                 ), 
                 
                 checkboxInput(inputId = "datatable_all",
                               label = "Show all text messages in the current network",
                               value = TRUE),
                 
                 checkboxInput(inputId = "datatable",
                               label = "Show the filtered text messages of authors selected by long click on node.",
                               value = TRUE),
                 
                 #submitButton("Apply changes"),
                 width = 3
               ),
               
               mainPanel(
                 infoBoxOutput("id", width = 12) ,
                 h3(textOutput("text1")),
                 
                 visNetworkOutput("network", width = "100%", height = "850px"),
                 
                 h5(textOutput("text2")),
                 
                 DT::dataTableOutput(outputId = "all_messages"),
                 
                 h5(textOutput("text3")),
                 
                 DT::dataTableOutput(outputId = "messages"),
                 width = 9
                 
               )
             )
    )       
  )
)


  



server <- function(input, output) { 
  #Email tab
  dataset <- reactive(GASTechgraph[[input$var]])
  output$GASTechgraph <- renderPlot({
    
    g <- GASTechgraph %>%
      activate(nodes)%>%
      filter(bc>=input$bc)%>%
      ggraph(layout = "nicely") + 
      geom_edge_link(aes(width=Weight), 
                     alpha=0.2) +
      scale_edge_width(range = c(0.1, 5)) +
      geom_node_point(aes(colour = Name, 
                          size = bc))+
      ggtitle("Nodes & Edges of Email Network")
    g
    
  })
  
  output$employeeTab <- DT::renderDataTable({
    if(input$employee_background){
      DT::datatable(data = employee,
                    options= list(pageLength = 4),
                    rownames = FALSE)
    }
  })
  
  
  
  
  #Spending-location tab
  output$bipartite <- renderVisNetwork({
    #V(cc_graph)$type <- bipartite_mapping(cc_graph)$type
    cc_graph<- cc_graph %>%activate(edges)%>%filter(Day==input$DAY)
    cc_graph<- cc_graph %>%activate(edges)%>%filter(Hour==input$HOUR)
    data<-toVisNetworkData(cc_graph)
    #cc_edges<-filter(cc_edges,Day==input$DAY)
    visNetwork(data$nodes,data$edges,
               main = "Network of credit card spending locations", width = "100%")%>%
      visGroups(groupname = "Card Holder", color = "darkblue", shape = "star", 
                shadow = list(enabled = TRUE)) %>% 
      visGroups(groupname = "Location", color = "red", shape = "database") %>%
      
      visEdges(arrows = "to", 
               smooth = list(enabled = TRUE, 
                             type = "curvedCW")) %>%
      visIgraphLayout(layout = "layout_as_bipartite") %>%
      #waiting for prof's clinic
      visOptions(highlightNearest = list(enabled = T, degree = 1, hover = T),
                 nodesIdSelection = T
      ) %>%
      visLegend() %>%
      visLayout(randomSeed = 123)
    
    
  })
  
  output$cc_data <- renderDataTable({
    if(input$record_checkbox){
      datatable(cc_data,
                    options= list(pageLength = 5),
                    rownames = FALSE)
    }
  })  
 # output$cc_data <- renderDataTable(cc_data,options = list(pageLength = 5))    
  
  output$nodes_checkbox <- renderDataTable({
    if(input$nodes_checkbox){
      datatable(nodes_table,
                options= list(pageLength = 5),
                rownames = FALSE)
    }
  })    
  
  
  output$nodes_table <- renderDataTable(nodes_table,
                                        options = list(pageLength = 5),
                                        rownames = FALSE)    
  
  
  # Message Tab
  edgesInput <- reactive({
    edges %>% filter(get(input$edge) == 1) %>% filter(get(input$group) == 1) %>%  filter(datetime >= input$time[1] & datetime <= input$time[2])
  })
  
  output$text1 <- renderText({paste("From", strftime(input$time[1], "%T"), "to", strftime(input$time[2], "%T"))
  })
  
  output$text2 <- renderText({"This data table displays all text messages in the current network."
  })
  
  output$text3 <- renderText({"This data table displays only the filtered text messages of authors selected by long click on node."
  })
  
  
  observe({
    
    id = nodes %>%
      filter(get(input$group) == 1)%>%
      .$id%>%
      .[1]
    
    visNetworkProxy("network") %>%
      visFocus(id = id, scale = 4)
  })
  
  output$id <- renderInfoBox({
    infoBox(
      icon = icon("question-circle"),
      title = "How to explore the Network Graph:
      The nodes are authors of the text messages and the edges point to the mentioned recipients of the message from the source authors. ", 
      color = "blue",
      fill = TRUE
      
    )
  })
  
  output$network <- renderVisNetwork({
    
    groups.closed <- c("All", "News", "Official", "Spam", "Interest", "Others" , "CallCentre")
    
    myPalette <- c("#D1BCA8", "#DF536B", "#61D04F", "#2297E6", "#28E2E5", "#CD0BBC", "#F5C710")
    
    
    
    
    edges <- edgesInput()
    #nodes <- nodesInput()
    
    
    visNetwork(nodes, edges,  width = "100%")  %>%  visLayout(randomSeed = 123) %>%
      visEdges(arrows = "top", color = list(color = "#0085AF", highlight = "#C62F4B", smooth = FALSE)) %>%
      visNodes(labelHighlightBold = TRUE, font = list(size=30), shape="box", color = list(
        background = "#0085AF",
        border = "#013848",
        highlight = "#FF8000"
      )) %>% visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE, selectedBy = list(variable = "group", multiple = TRUE, highlight = TRUE )) %>%
      visIgraphLayout(layout = "layout_on_sphere") %>%
      visGroups(groupname = "All", color = "#D1BCA8") %>%
      visGroups(groupname = "News", color = "#DF536B") %>%
      visGroups(groupname = "Interest", color =  "#28E2E5") %>%
      visGroups(groupname = "Others", color = "#CD0BBC") %>%
      visGroups(groupname = "Official", color = "#61D04F") %>%
      visGroups(groupname = "Spam", color = "#2297E6") %>%
      visGroups(groupname = "CallCentre", color = "#F5C710") %>%
      visGroups(groupname = "NOT Mentioned", color = "white") %>%
      #visClusteringByGroup(groups = groups.closed , label = "Group : ", shape = "ellipse", color = myPalette) %>%
      visLegend()  %>% visPhysics(enabled = FALSE) %>% visInteraction(multiselect = TRUE) %>%
      visEvents(select = "function(data) {
                Shiny.onInputChange('current_nodes_selection', data.nodes);
                Shiny.onInputChange('current_edges_selection', data.edges);
                ;}")
    
    
  })
  
  output$all_messages <- DT::renderDataTable({
    edges <- edgesInput()
    
    if(input$datatable_all){
      
      subset_table <- edges %>% dplyr::select(time, author, title, mentions, hashtags, links, group)
      
      DT::datatable(data =subset_table,
                    options= list(pageLength = 5),
                    rownames = FALSE)
    }
  })
  
  output$messages <- DT::renderDataTable({
    edges <- edgesInput()
    
    if(input$datatable){
      
      subset_table <- edges %>% filter((to %in% input$current_nodes_selection)|(from %in% input$current_nodes_selection)) %>% dplyr::select(time, author, title, mentions, hashtags, links, group) 
      
      
      DT::datatable(data =subset_table,
                    options= list(pageLength = 5),
                    rownames = FALSE)
    }
  })
  
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)


