## app.R ##
#libraries used
library("shinydashboard")
library("dplyr")
library("plotly")
library("tidyverse")
library("lubridate")
library("DT")

#load mims data. 
mims <- read_csv("mims2019.csv") %>%
    mutate(date=dmy(date)) %>%
    mutate(ExamDate=dmy(ExamDate)) %>%
    mutate(Awarding=dmy(Awarding)) %>%
    arrange(Code, date)  

#get latest date in mims files - should be todays date is up to date mims file used
todays_date <- max(mims$date)

#create a list of paper codes from the mims data
paper_list <- mims %>%
    group_by(Code, level_code) %>%
    summarise(max = max(date)) %>%
    select(Code, level_code) %>%
    ungroup()

#load qual data
quals <- read_csv("quals2019.csv") %>%
    mutate(date=dmy(date)) %>%
    mutate(Awarding=dmy(Awarding)) %>%
    arrange(date) 

#create list of qual codes from qual data
qual_list <- quals %>%
    group_by(Qual, level_code) %>%
    summarise(max = max(date)) %>%
    select(Qual, level_code) %>%
    ungroup()

#read lookup file
qual_paper_lookup <-  read_csv("lookup.csv") %>%
    arrange(Code)


#shinydashboard app UI section
ui <- dashboardPage(
    #header
    dashboardHeader(title = "Marking Progress"),
    #side bar - parameters
    dashboardSidebar(
        radioButtons("level", "Level:",
                     c("GCSE" = 1,
                       "GCE" = 2,
                       "VQ" = 3), 
                     selected = 1) ,
        radioButtons("awarding_range", "Awarding in next:",
                     c("5 days" = 1,
                       "10 days" = 2,
                       "20 days" = 3,
                       "All" = 4),
                     selected = 4),
        selectInput("qual", label = "Specification:",
                    choices = c("All")),
        selectInput("paper", label = "Paper:",
                    choices = ""),
        radioButtons("ordering", "Order by:",
                     c("Total" = 1,
                       "Percentage" = 2),
                     selected = 1) 
        
    ),
    ## Body content
    dashboardBody(
        #first row - mark completion kpi for gcse, gce and vq
        fluidRow(
            tags$head(tags$style(HTML(".small-box {height: 80px}"))),
            tags$head(tags$style(HTML('.box {margin: 5px;}'))),
            valueBoxOutput("gcse_kpi", width = 4),
            valueBoxOutput("gce_kpi", width = 4),
            valueBoxOutput("vq_kpi", width = 4)
        ),
        #qual level table and plot
        fluidRow(
            tags$head(tags$style(HTML('.box {margin: 5px;}'))),
            box(title = "Qual Level Cash-In - Table", width = 6, height = "400px", status = "primary",  DT::dataTableOutput("qual_cash_in_table")),
            box(title = "Qual Level Cash-In - Plot", width = 6, height = "400px", status = "primary",  plotlyOutput("qual_cash_in_plot"))
        ),
        #paper level table and plot
        fluidRow(
            tags$head(tags$style(HTML('.box {margin: 5px;}'))),
            box(title = "Paper Level - Table", width = 6, height = "400px", status = "primary",  DT::dataTableOutput("paper_table")),
            box(title = "Paper Level - Plot", width = 6, height = "400px", status = "primary",  plotlyOutput("paper_plot"))
        )
    )
)

#server part
server <- function(input, output, session) {
    #side bar parameters load
    #qual drop down
    observe({
        #create filtered list from qual list where the level is the one chosen in Level 
        filtered <- 
            qual_list %>%
            filter(level_code==input$level)
        
        #update qual drop down box with list, with "All" option at the top
        updateSelectInput(
            session , "qual", 
            choices = c("All", filtered$Qual)
        )
    })
    
    #paper drop down
    observe({
        #create filtered list from paper lookup 
        #if qual drop down is "All", filter papers by chosen level
        filtered <- if(input$qual=="All") {
            qual_paper_lookup %>%
                filter(qual_level==input$level)
        } 
        #else filter papers to chosen paper
        else {
            qual_paper_lookup %>%
                filter(Qual==input$qual)    
        }
        
        #update paper drop down with filtered list
        updateSelectInput(
            session , "paper", 
            choices = filtered$Code
        )
    })
    
    #kpi section
    #create GCSE df from mims df where level is 1, add max_date, filter data so date=max_date, create marked and total sums
    #calculate percentage marked
    GCSE <- mims %>%
        filter(level_code==1)      %>%
        mutate(max_date = max(date)) %>%
        filter(date==max_date) %>%
        summarise(marked = sum(marked_actual), total = sum(max_number)) %>%
        mutate(percentage=100*marked/total) 
    
    #slice percentage complete
    GCSE_complete <- GCSE$percentage[1]
    
    #format to 1 dp
    rate1 <- formatC(GCSE_complete, digits = 1, format = "f")
    
    #create ValueBox with calculated value
    output$gcse_kpi <- renderValueBox({
                        valueBox(subtitle = "GCSE completed marking",
                                 icon = icon("area-chart") ,
                                 color = "light-blue",
                                 value = paste(rate1,"%"))
                    })
    
    
    #create GCE df from mims df where level is 2, add max_date, filter data so date=max_date, create marked and total sums
    #calculate percentage marked
    GCE <- mims %>%
        filter(level_code==2)      %>%
        mutate(max_date = max(date)) %>%
        filter(date==max_date) %>%
        summarise(marked = sum(marked_actual), total = sum(max_number)) %>%
        mutate(percentage=100*marked/total) 
    
    #slice percentage from df
    GCE_complete <- GCE$percentage[1]
    
    #format to 1 dp
    rate2 <- formatC(GCE_complete, digits = 1, format = "f")
    
    #create ValueBox output with calced value
    output$gce_kpi <- renderValueBox({
        valueBox(subtitle = "GCE completed marking",
                 icon = icon("area-chart") ,
                 color = "aqua",
                 value = paste(rate2,"%"))
    })
    
    
    #create VQ df from mims df where level is 3, add max_date, filter data so date=max_date, create marked and total sums
    #calculate percentage marked
    VQ <- mims %>%
        filter(level_code==3)      %>%
        mutate(max_date = max(date)) %>%
        filter(date==max_date) %>%
        summarise(marked = sum(marked_actual), total = sum(max_number)) %>%
        mutate(percentage=100*marked/total) 
    
    #slice percentage
    VQ_complete <- VQ$percentage[1]
    
    #format to 1 dp
    rate3 <- formatC(VQ_complete, digits = 1, format = "f")
    
    #create ValueBox output with calced value
    output$vq_kpi <- renderValueBox({
        valueBox(subtitle = "VQ (WBQ & L3Applied) completed marking",
                 icon = icon("area-chart") ,
                 color = "teal",
                 value = paste(rate3,"%"))
    })
    
    #qual table
    observe({
            
            #derive "to" date based on awarding range input
            awarding_to_date <- switch(input$awarding_range,
                                    "1" = todays_date + 5,
                                    "2" = todays_date + 10,
                                    "3" = todays_date + 20)
            
            #filter quals on awarding range
            #if "All" there is no "to" date in the filter
            filt_quals <- if(input$awarding_range < 4) {
                                quals %>%
                                    filter(level_code==input$level) %>%
                                    filter(Awarding > todays_date) %>%
                                    filter(Awarding < awarding_to_date)
                            } else {quals %>%
                                        filter(level_code==input$level) %>%
                                        filter(Awarding > todays_date)}
            
            #depending on Order By option
            #create the output df
            if(input$ordering == 1) {
                #if total diff then group by Qual and calc total dif, subset the df to the columns to display
                qual_table <- filt_quals %>%
                                group_by(Qual) %>%
                                mutate(max_date = max(date)) %>%
                                filter(level_code==input$level)      %>%
                                filter(date==max_date) %>%
                                ungroup() %>%
                                arrange(pred_dif_tot) %>% 
                                mutate(tot_dif=Centre-Mark_In) %>%
                                select(Qual, Centre, Mark_In,  pred_dif_tot, tot_dif)
                
                #create a list of the column names to be used in the datatable
                col_rename <- list("Spec", "Entered", "Marks in", "target dif", "total dif")
            } else {
                #if %age then group by qual and calc tot_dif %, subset the df to the columns to display
                qual_table <- filt_quals %>%
                    group_by(Qual) %>%
                    mutate(max_date = max(date)) %>%
                    filter(level_code==input$level)      %>%
                    filter(date==max_date) %>%
                    ungroup() %>%
                    mutate(tot_dif_pc=100-percent_Marked_IPA) %>%
                    arrange(pred_dif_pc)  %>%
                    select(Qual, Centre, percent_Marked_IPA, pred_dif_pc, tot_dif_pc) 

                #create a list of the column names to be used in the datatable   
                col_rename <- list("Spec", "Entered", "% marked (IPA)", "target dif%", "total dif%")
            }
            
            ##qual cash in table
            #render data table
            output$qual_cash_in_table <- DT::renderDataTable({
                #pass in col rename list
                if (input$ordering == 1) {
                    datatable(qual_table,
                              colnames = col_rename,
                              options = list(pageLength = 20, scrollY = "225px") )    
                } else {
                    #if %age then round the % columns to 1 dp
                    datatable(qual_table,
                              colnames = col_rename,                              
                              options = list(pageLength = 20, scrollY = "225px") ) %>%
                                formatRound(c('pred_dif_pc', 'percent_Marked_IPA', 'tot_dif_pc'), 1)    
                }
                
                
            })
    })
    
    #qual plot
    observe({
        #render the qual plot
        output$qual_cash_in_plot <- renderPlotly({
            #plot is only displayed if qual drop down is NOT "All"
            if(input$qual!="All"){
                #create filtered df to selected qual
                qual_plot_data <- quals %>%
                    filter(Qual==input$qual) %>%
                    arrange(date)  
                
                #calculated fields 
                max <- qual_plot_data$Centre[1]
                Awarding <- qual_plot_data$Awarding[1]
                qual_code <- qual_plot_data$Qual[1]
            
            
                #plot filtered data
                pq <- qual_plot_data %>% 
                        plot_ly(x= ~date, y= ~Mark_In, type = "scatter", mode = "lines+markers", name="Marked") %>%
                        add_trace(y = ~marked_prediction, name="if linear", mode = "Lines") %>%
                        add_lines(x=c(Awarding, Awarding), y=c(0, max), name = "Award Date") %>%
                        layout(yaxis = list(title = "Candidates Marked"), xaxis = list(title = "Date"))
            }
            
        })
    })
    
    #paper table
    observe({
        #if qual drop down is "All", get all the codes from the lookup
        if(input$qual=="All"){
            filt_code_list <- qual_paper_lookup %>%
                select(Code)
        }
        #else filter lookup by the chosen qual
        else{
            filt_code_list <-  qual_paper_lookup %>%
                filter(Qual==input$qual) %>%
                select(Code)
        }
        
        #derive the "to" awarding date
        awarding_to_date <- switch(input$awarding_range,
                                   "1" = todays_date + 5,
                                   "2" = todays_date + 10,
                                   "3" = todays_date + 20)
        
        #filter mims data on awarding range
        filt_papers <- if(input$awarding_range < 4) {
            mims %>%
                filter(level_code==input$level) %>%
                filter(Awarding > todays_date) %>%
                filter(Awarding < awarding_to_date)
        } 
        #if the awarding range is "All" there is no "to" date in the filter
        else {mims %>%
                filter(level_code==input$level) %>%
                filter(Awarding > todays_date)}
        
        
        #left join mims to code lookup
        filt_papers <- left_join(filt_code_list, filt_papers)
        
        #if Order By is total dif create a df grouping on Code, adding calced fields and selecting only columns to be displayed
        if(input$ordering == 1) {
            paper_table <- filt_papers %>%
                                group_by(Code) %>%
                                mutate(max_date = max(date)) %>%
                                filter(level_code==input$level)      %>%
                                filter(date==max_date) %>%
                                ungroup() %>%
                                arrange(pred_dif_tot) %>%
                                select(Code, SubjectPaperName, Awarding, Type, max_number,  marked_actual, pred_dif_tot)
            
            
            #create a list of the column names to be used in the dashboard
            col_rename <- list("Spec", "Paper", "Award date", "Type", "Entries", "Marks in", "Target Dif")
        } 
        #if Order By is %age calc percentages, grouping by Code
        else {
            paper_table <- filt_papers %>%
                                group_by(Code) %>%
                                mutate(max_date = max(date)) %>%
                                filter(level_code==input$level)      %>%
                                filter(date==max_date) %>%
                                ungroup() %>%
                                mutate(marked_pc=100*(marked_actual/max_number)) %>%
                                arrange(pred_dif_pc)  %>%
                                select(Code, SubjectPaperName, Awarding, Type, max_number,  marked_pc, pred_dif_pc)
            
            #create a list of column names to be used in the dashboard
            col_rename <- list("Spec", "Paper", "Award date", "Type", "Entries", "Marks in %", "Target Dif %")
        }
        
        ##paper table
        #render the datatables
        output$paper_table <- DT::renderDataTable({
            #if the order by is %age then format the % columns to 1 dp
            if (input$ordering == 1) {
                datatable(paper_table,
                          colnames = col_rename,                          
                          options = list(pageLength = 20, scrollY = "225px") )    
            } else {
                datatable(paper_table,
                          colnames = col_rename,                          
                          options = list(pageLength = 20, scrollY = "225px") ) %>%
                    formatRound(c('pred_dif_pc', 'marked_pc'), 1)    
                
            }
            
        })
    })
    
    #paper plot
    observe({
        #render the paper plot
        output$paper_plot <- renderPlotly({
            
            #filter the mims df to the paper chosen in the paper drop down and the level in the Level radio option
            paper_plot_data <- mims %>%
                                filter(Code==input$paper) %>%
                                filter(level_code==input$level)
                
            #create calculated items
            max <- paper_plot_data$max_number[1]
            Awarding <- paper_plot_data$Awarding[1]
            paper_name <- paper_plot_data$SubjectPaperName[1]
            paper_code <- paper_plot_data$Code[1]
                
                
            #create plot
            pp <- paper_plot_data %>% 
                    plot_ly(x= ~date, y= ~marked_actual, type = "scatter", mode = "lines+markers", name="marked") %>%
                    add_trace(y = ~marked_prediction, name="if linear", mode = "lines") %>%
                    add_lines(x=c(Awarding, Awarding), y=c(0, max), name = "award date") %>%
                    layout(yaxis = list(title = "Candidates Marked"), yaxis = list(title = "Date"), title=paste(paper_name, paper_code))
            
            
        })
    })
}

shinyApp(ui, server)
