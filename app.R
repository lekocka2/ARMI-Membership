#load packages
library(shiny)
library(knitr)
library(tidyverse)
library(plotly)
library(data.table)

ui <- fluidPage(

    titlePanel("Plot Generator"),
    
    sidebarLayout(
        sidebarPanel(width=3,
            # Inputs
            fluidRow(column(6,
                            selectInput("fileSource","Source", c("NIH", "NSF", "Clinical Trials", "SBIR"), selected=NULL, width='120px'),
                            numericInput("cutoffYear","Cutoff year", value = 2010, width='120px'),
                            style='padding:4px'
                            ),
                     column(6,
                            numericInput("numPrint","Num to print", value=10, width='120px'),
                            textInput("mostRecent", "How recent", value = "Jun 2020", width='120px'),
                            style='padding:4px'
                            )
                     ),
            fluidRow(fileInput("upload","File Upload", accept=".csv", buttonLabel="Browse...", width='300px'))
        ),

        # Outputs
        mainPanel(
            uiOutput("fileInfo"), br(),
            tabsetPanel(type = "tabs",
                        
                        tabPanel("Num Awards", 
                                 plotlyOutput("numAwards"), style='padding-top:20px', br(),
                                 tableOutput("topResults1")
                        ),
                        
                        tabPanel("Amt Funding", 
                                 plotlyOutput("amtFunding"), style='padding-top:20px', br(),
                                 tableOutput("topResults2")
                        ),
                        tabPanel("Tissue Types", 
                                 plotlyOutput("tissueTypes"), style='padding-top:20px', br(),
                                 tableOutput("topResults3")
                        ),
                        tabPanel("Awards By Year",
                                 plotlyOutput("numByYear"), style='padding-top:20px'),
                        tabPanel("Funding By Year",
                                 plotlyOutput("fundByYear"), style='padding-top:20px')
            ) 
        )
    )
)

# Define server logic
server <- function(input, output){
    
    options(scipen=999) # no scientific notation
    RV <- reactiveValues() # initialize RV
    cutoff <- isolate(input$cutoffYear) # isolate input so it doesnt incorrectly change in main panel output
    
    observeEvent(input$upload, { # do all this cool stuff when the file is uploaded
        titleSnippet <- paste0(input$fileSource, ", ", toString(cutoff), " to ", input$mostRecent) #this shows up in the plot titles
        #read the file and save to 'data'
        inFile <- input$upload
        data <- read.csv(inFile$datapath, 1)
        
        #order data by organization name A-Z
        data <- data[order(data$Organization),]
        #clean data
        totalRows <- nrow(data)
        cleaned <- data
        cleaned$Organization <- gsub(",", "", cleaned$Organization) #removes commas
        cleaned$Organization <- gsub("Incorporated", "", cleaned$Organization) #removes Incorp
        cleaned$Organization <- gsub("Inc.", "", cleaned$Organization) #removes Inc.
        cleaned$Organization <- gsub("Inc", "", cleaned$Organization) #removes Inc
        cleaned$Organization <- gsub("Llc", "", cleaned$Organization) #removes Llc
        cleaned$Organization <- trimws(cleaned$Organization) #removes trailing spaces
        cleaned$Organization <- gsub("\\s+", " ", cleaned$Organization) #merges multiple white spaces into one
        cleaned_cutoff <- cleaned %>% filter(cleaned$FY >= input$cutoffYear)
        
        ##################################################
        ## File Info #####################################
        output$fileInfo <- renderUI({
            str0.5 <- "FILE INFO"
            str1 <- print(paste0("- ", totalRows, " total unique awards"))
            str2 <- print(paste0("- ", nrow(cleaned %>% filter(cleaned$FY == 0)), " did not capture a year"))
            str3 <- print(paste0("- ", nrow(cleaned_cutoff), " were awarded after the cutoff year"))
            str4 <- print(paste0("- ", nrow(cleaned_cutoff %>% filter(cleaned_cutoff$Funding == 0)), " showed $0 in funding"))
            
            HTML(paste(str0.5,'<br/>',str1,'<br/>',str2,'<br/>',str3,'<br/>',str4))
        })
        
        ##################################################
        ## Num Awards Tab ################################
        output$numAwards <- renderPlotly({
            filtered <- cleaned_cutoff %>% select(Organization, Tissue) #select rows you want to keep
            counts <- as.data.frame(table(filtered), stringsAsFactors = F) 
            #consolidate by tissue type and count frequency
            counts <- counts %>% filter(counts$Freq != 0) #removes frequencies of zero
            #remove tissue type column  #sum freq's per org
            countsOrder <- counts %>% select(!Tissue) %>% group_by(Organization) %>% summarize(Freq = sum(Freq))
            #order by Freq to establish customer factors
            countsOrder <- countsOrder[order(countsOrder$Freq, decreasing=F),]
            counts$Organization <- factor(counts$Organization, level=countsOrder$Organization)
            RV$countsOrder <- countsOrder
            
            #create stacked histogram plot showing breakdown of awards per organization and their tissue types
            bar <- plot_ly(y = ~counts$Organization, x = ~counts$Freq, type = 'bar', orientation = 'h', 
                            name = ~counts$Tissue, color = ~counts$Tissue) %>%
                layout(margin = list(l = 300), title=paste0("Tissue Type / Organization Breakdown (", titleSnippet, ")"), 
                       yaxis = list(title = 'Number of Awards'), xaxis = list(title = 'Organization Name'), barmode = 'stack')
        })
        #print results list for previous plot
        output$topResults1 <- renderTable({
            countsOrder <- RV$countsOrder
            countsOrder <- countsOrder %>% map_df(rev)
            printDF1 <- countsOrder[1:input$numPrint,1:2]
        }, caption="Most Awarded Organizations")
        
        ##################################################
        ## Amt Funding Tab ###############################
        output$amtFunding <- renderPlotly({
            #select pertinent columns
            fundData <- cleaned_cutoff %>% 
                select(Organization,Funding,Tissue) %>% 
                mutate(Funding=round(cleaned_cutoff$Funding, -3))
            #create factor levels
            lev <- fundData %>% group_by(Organization) %>% summarize(Funding = sum(Funding))
            lev <- lev[order(lev$Funding),]
            #assign factor levels
            fundData$Organization <- factor(fundData$Organization,levels = lev$Organization)
            
            #save df in reactive values
            RV$lev <- lev
            #plot
            bar1 <- plot_ly(fundData, x = ~Funding, y = ~Organization, type = 'bar',
                           name = ~Tissue, color = ~Tissue) %>%
                layout(margin = list(l = 300), title = paste0("Funding per Organization (", titleSnippet, ")"),
                       xaxis = list(title = "Funding ($)"),
                       yaxis = list(title = "Organization Name"), orientation = 'h', barmode='stack')
        })
        #print results list for previous plot
        output$topResults2 <- renderTable({
            lev <- RV$lev #get df out of RV
            lev <- lev[order(lev$Funding, decreasing = T),]
            printDF2 <- sapply(lev[1:input$numPrint,1:2], FUN=function(x) prettyNum(x, big.mark=","))
            printDF2[,2] <- paste("$", printDF2[,2])
            printDF2
        }, caption="Most Funded Organizations")
        
        ##################################################
        ## Tissue Types Tab ##############################
        output$tissueTypes <- renderPlotly({
            tissuesFunded <- cleaned_cutoff %>% select(Funding,Tissue) %>% 
                mutate(Funding = round(Funding,-3)) %>%
                group_by(Tissue) %>% 
                summarize(Funding = sum(Funding))
            #identify factor levels
            tissuesFunded$Tissue <- factor(tissuesFunded$Tissue, 
                                           levels = tissuesFunded$Tissue[order(tissuesFunded$Funding, decreasing=T)])
            RV$tissuesFunded <- tissuesFunded
            
            bar2 <- plot_ly(tissuesFunded, x = ~Tissue, y = ~Funding, type = 'bar') %>%
                layout(title = paste0("Funding per Tissue Type (", titleSnippet, ")"),
                       xaxis = list(title = "Tissue Type"),
                       yaxis = list(title = "Total Funding ($)"))
        })
        
        output$topResults3 <- renderTable({
            tissuesFunded <- RV$tissuesFunded
            tissuesFunded <- tissuesFunded[order(tissuesFunded$Funding, decreasing = T),]
            printDF3 <- sapply(tissuesFunded[1:5,1:2], FUN=function(x) prettyNum(x, big.mark=","))
            printDF3[,2] <- paste("$", printDF3[,2])
            printDF3
        }, caption="Most Funded Tissue Types")
        
        ##################################################
        ## Awards By Year Tab ############################
        output$numByYear <- renderPlotly({
            years_prefilt <- cleaned_cutoff %>% select(FY,Tissue) %>% filter(FY!=0)
            years <- as.data.frame(table(years_prefilt), stringsAsFactors = F)
            years <- years %>% filter(years$Freq!=0)
            
            bar3 <- plot_ly(x = ~years$FY, y = ~years$Freq, type = 'bar',
                             name = ~years$Tissue, color = ~years$Tissue) %>%
                layout(title=paste0("Number of Awards per Year (", titleSnippet, ")"), 
                       yaxis = list(tickprefix=" ", title = 'Number of Awards'), xaxis = list(title = 'Year'), barmode = 'stack')
        })
        
        ##################################################
        ## Funding By Year Tab ###########################
        output$fundByYear <- renderPlotly({
            years_cut <- cleaned_cutoff %>% select(FY,Funding,Tissue) %>% filter(FY!=0)
            years_cut$Funding <- round(years_cut$Funding, -3)
            
            bar4 <- plot_ly(x = ~years_cut$FY, y = ~years_cut$Funding, type = 'bar',
                            name = ~years_cut$Tissue, color = ~years_cut$Tissue) %>%
                layout(title=paste0("Amount Funding per Year (", titleSnippet, ")"), 
                       yaxis = list(tickprefix=" ", title = 'Funding ($)'), xaxis = list(title = 'Year', autotick=F), 
                       barmode = 'stack')
        })
        
    }) #end observe event
    
} #end server

# Run the application 
shinyApp(ui = ui, server = server)
