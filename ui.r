library(shiny)
library(shinyBS)
library(shinydashboard)
#options(error=NULL)
library(shinyjs)
library(plotly)
library(dplyr)
library(arules)
library(markovchain)


######### DO NOT DELETE ###############

#### Code that filters and manipulates the data to be saved to workspace that is pulled in by the app code
# setwd("C:\\users\\breric\\OneDrive - Hewlett Packard Enterprise\\Sales Analytics Projects\\Partner Segmentation\\Modeling\\Quarterly_Level_R_Code")
setwd("C:/Users/zhaoch/Desktop/Project/R Shiny/Markovchain Partner")
#setwd("C:\\Users\\teterq\\OneDrive - Hewlett Packard Enterprise\\partener_rev_seg")

# all_region_dat <- read.csv("All_Partner_Merged_11-17.csv")
# 
# sellout.filter <- all_region_dat[, c(2:4,6, 8)] %>% group_by(PPID, Current.Product.Segment, Customer.Sub.Region, Partnername) %>% summarize(sum.ppid = sum(Ref.Net.Sellout.for.Quota.Perf.CLC) )
# 
# pos_sellout_filter <- sellout.filter[which(sellout.filter$sum.ppid > 0),]
# 
# merge.filter <- merge(pos_sellout_filter, all_region_dat)
# 
# seg_part <- filter(merge.filter, ((PPID!="") & (Partnername!="")  & ((Current.Product.Segment=='HPN') | (Current.Product.Segment=='HPSD') | (Current.Product.Segment=='HP Servers')) ))
# 
# #seg_part <- subset(seg_part, ((PPID!='.') & (combinedScore!='.') & (Ref.Net.Sellout.for.Quota.Perf.CLC > 0) & ((Current.Product.Segment=='HPN') | (Current.Product.Segment=='HPSD') | (Current.Product.Segment=='HP Servers'))))
# 
# # need to filter out na for partnername, groupby ppid - take rowsum of ref.net.sellout - delete partners with rowsum <= 0
# #why is group_by in below function across all those columns
# 
# 
# 
# seg_part <- seg_part %>% group_by(Fiscal.Quarter, Partnername, Current.Product.Segment, Customer.Sub.Region, PPID, combinedScore,RFM_Comb, Prior_combinedScore, Prior_RFM_Comb, Customer.Program.Name, May.Oct..2016.Combined.Score , May.Oct..2016.RFM.Segment , Dec15.Apr16.Combined.Score, Dec15.Apr16.RFM.Segment) %>% summarise(sellout = sum(Ref.Net.Sellout.for.Quota.Perf.CLC), quota = sum(Sales.Comp.Net.CLC))
# 
# getmode <- function(v) {
#   uniqv <- unique(v)
#   uniqv[which.max(tabulate(match(v, uniqv)))]
# }
# 
# 
# account.names <- unique(sapply(seg_part$Partnername, as.character))
# 
# #retrieve all unique rows
# unique.df <- pos_sellout_filter[, 2:4]
# 
# rm(list=setdiff(ls(), c("getmode", "seg_part", "account.names", "unique.df")))
# save.image("~/R-Partner_Revenue_Forecasting/all_partner_data.RData")
# 
# # 

################################################################################
########################### Load the required data #############################


load("all_partner_data.RData", .GlobalEnv)



#########################################################################################################
#######################################                  ################################################
####################################### BEGIN UI SECTION ################################################
#######################################                  ################################################
#########################################################################################################


shinyUI(
  dashboardPage( 
    
    
    dashboardHeader(title = "HPE Partner Revenue Forecast Demo", titleWidth = '450px'),
    dashboardSidebar( width= 150,
                      
                      sidebarMenu(
                        
                        menuItem("RevForecast", tabName= "tabDemo", icon=icon("dashboard")),
                        
                        menuItem("Coming Soon...", tabName= "tab2", icon = icon("spinner", lib = "font-awesome")),
                        
                        
                        menuItem("Signature",
                                 menuSubItem("HPE"),
                                 menuSubItem("#QTpro"))
                        
                      )),
    
    dashboardBody(
      
      tabItems(
        
        tabItem(tabName= "tabDemo",
                
                fluidPage(
                  shinyjs::useShinyjs(),
                  
                  column(4,
                         
                         box(title= strong("Inputs"), status = "primary", width =12,
                             
                             sliderInput("bins",
                                         "Number of Bins:",
                                         min = 2,
                                         max = 35,
                                         value = c(3,10)),
                          
                             sliderInput("simulations", "Simulation:", 
                                         min=0, max=5000, value=500),
                             
                             hr(),
                             
                             
                             selectInput("partner",
                                         label = ("Partner Name:"),
                                         choices = account.names),
                             
                             br(),
                             
                             uiOutput("ProSeg"),
                            
                             br(),
                             
                             uiOutput("SubRegN"),
                           
                             br(),
                       
                             selectizeInput("next_state", label = ("Next State:"), choices = c(1:5), options = list(create= TRUE), selected = 4),
                             
                             br(),
                             
                             actionButton("run", label = "Run"),
                             
                             hr()
                             
                             )),
                  
                  
                  column(8,
                         
                         fluidRow(  
                           
                           #######################################
                           ## REVENUE PLOTS
                           
                           box(title= "Markov Chain Predictions", status= "success", solidHeader = TRUE, width=12,
                               
                               plotlyOutput("outputplot")
                               
                               
                           )), #big box #row
                         
                         
                         fluidRow(  
                           tabBox(id = "tabBoX",
                                  # Title can include an icon
                                  title = tagList(shiny::icon("gear"), "Info"), width = 12,
                                  
                                  
                                  
                                  ######################################
                                  ## TABPANEL 1
                                  
                                  tabPanel("Predictions vs. Actuals",
                                           fluidPage(
                                             uiOutput("comp"),
                                             uiOutput("out_err1"),
                                             tableOutput("pred"))), #tabpanel
                                  
                                  ###################################### 
                                  ## TABPANEL 2
                                  
                                  tabPanel("RFM Scores", 
                                           
                                           fluidPage(
                                             
                                             uiOutput("RFMcomp"),
                                             tableOutput("rfm_out"))), #fluidpage #tabpanel
                                  
                                  ######################################
                                  ## TABPANEL 3
                                  
                                  tabPanel("Optimal Bin Selection",
                                           fluidPage(
                                             textOutput("bin")
                                             )) #fluidpage #tabpanel
                               
                           ))  #tabbox #row
                         
                         ))), #column #fluidpage #tabitem 
       
         tabItem(tabName= "tab2",
                 fluidPage(
                   
                   HTML('<strong> COMING SOON ... <strong>')
                   
                 ) #fluidpage
        
        ))) #tabitem #tabitems #dasboardbody
                         
                             
  )) #shinyUi #dashboard page








#######################

#QTpro
#QTpro
##HPE
##HPE