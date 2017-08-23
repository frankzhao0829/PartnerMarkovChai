library(shiny)
library(shinyBS)
library(shinydashboard)
#options(error=NULL)
library(shinyjs)
library(plotly)
library(dplyr)
library(shinythemes)

library(arules)

library(markovchain)


# getmode <<- function(v) {
#   uniqv <- unique(v)
#   uniqv[which.max(tabulate(match(v, uniqv)))]
# }

shinyServer(function(input, output, session) {
  
  
  ######################################################################################################
  ######################################################################################################
  

  
  #this section responds to account selection
  #pulls information about selected account
  
  output$ProSeg <- renderUI(
    selectInput("ps",
                label = ("Product Segments:"),
                choices = as.character(unique(unique.df$Current.Product.Segment[which(unique.df$Partnername == input$partner)])))
  ) 
   
 
  output$SubRegN <- renderUI(
      selectInput("sub_region",
                  label = ("Sub-Region:"),
                  choices = as.character(unique(unique.df$Customer.Sub.Region[which(unique.df$Partnername == input$partner & unique.df$Current.Product.Segment == input$ps)])))
    )  

  
  #use this to create an event on these values changing so that when plot is finished generated we can enable run button
  val <<- reactiveValues(Values=0)
  
  #trigger event when run button is clicked
  observeEvent(input$run,{
  
    #disable selction boxes until graph finishes rendering
    shinyjs::disable("partner")
    shinyjs::disable("ps")
    shinyjs::disable("sub_region")
    shinyjs::disable("next_state")
    shinyjs::disable("run")
    
    #create progress statement while plot renders
    progress <<- Progress$new(session)
    
    progress$set(message = 'Generating Plot & Tables',
                 detail = 'This may take a while...')
    
    val$Values <- isolate(val$Values)+1
    
    
  min_value <- as.numeric(input$bins[1])
max_value <- as.numeric(input$bins[2])  
iterations <- c(min_value:max_value)
sellout <- seg_part$sellout[((seg_part$Current.Product.Segment==input$ps) & (seg_part$Partnername==input$partner) & (seg_part$Customer.Sub.Region==input$sub_region))]
quota <- seg_part$quota[((seg_part$Current.Product.Segment==input$ps) & (seg_part$Partnername==input$partner) & (seg_part$Customer.Sub.Region==input$sub_region))]
seg_month <- seg_part$Fiscal.Quarter[((seg_part$Current.Product.Segment==input$ps) & (seg_part$Partnername==input$partner) & (seg_part$Customer.Sub.Region==input$sub_region))]
# ADDDED BELOW 10-10-16
rfm_val <- seg_part$combinedScore[(seg_part$Partnername==input$partner)][1]
rfm_cmb <- seg_part$RFM_Comb[(seg_part$Partnername==input$partner)][1]
prior_rfm_val <- seg_part$Prior_combinedScore[(seg_part$Partnername==input$partner)][1]
prior_rfm_cmb <- seg_part$Prior_RFM_Comb[(seg_part$Partnername==input$partner)][1]
dec_apr_cmb <- seg_part$Dec15.Apr16.Combined.Score[(seg_part$Partnername==input$partner)][1]
dec_apr_seg <- seg_part$Dec15.Apr16.RFM.Segment[(seg_part$Partnername==input$partner)][1]
may_oct_cmb <- seg_part$May.Oct..2016.Combined.Score[(seg_part$Partnername==input$partner)][1]
may_oct_seg <- seg_part$May.Oct..2016.RFM.Segment[(seg_part$Partnername==input$partner)][1]


t <- as.numeric(input$next_state)

# ==============================================================
# Throw warning for partners without at least 6 quarters of data
# ==============================================================
if (length(sellout) < 6){
 
  output$out_err1 <- renderUI({
    tags$h3("Warning:  Insufficient Data for Chosen Partner/Product/Sub-Region combination.  Please choose a different combination")
    #HTML('<strong> Warning:  Insufficient Data for Chosen Partner/Product/Sub-Region combination <strong>')
  })
  
  output$outputplot <- renderPlotly({
    p <- plotly_empty()
  })
  
  output$comp <- renderUI({
    HTML('')
  })
  
  output$pred <- renderText({
    character()
  })
  
  output$act <- renderText({
    character()
  })
 
  output$RFMcomp <- renderUI({
    HTML('')
  })
} 

else { 
  output$out_err1 <- renderUI({
    HTML(' ')
  })
sellout_train <- sellout[1:(length(sellout)-3)]
sellout_test <- tail(sellout, n = 3)
#current_state <- tail(sellout_train, n=1)
next_predctions <- array(0, dim = c(length(iterations),as.numeric(input$simulations),t))
next_state_result <- matrix(0,nrow = as.numeric(input$simulations), ncol = t)
sum_np <- matrix(0,nrow = length(iterations), ncol = t)
res <- matrix(0,nrow = length(iterations), ncol = t)
for (n in iterations){
  
  if(length(unique(sellout_train)) > 1){
  categories <- discretize(sellout_train, method = "interval", categories = n, labels = c(1:n))
  cutpoints <- discretize(sellout_train, method = "interval", categories = n, onlycuts = TRUE)
  }
  
  if(length(unique(sellout_train)) == 1){
    categories <- discretize(sellout_train, method = "interval", categories = 1, labels = c(1))
    cutpoints <- discretize(sellout_train, method = "interval", categories = 1, onlycuts = TRUE)
  }
  
  cat_values <- vector()
  for (m in 1:n){
    cat_values <- append(cat_values, mean(cutpoints[m:(m+1)]))
  }
  #add the mode of all the training categories to avoid brand new occuring state with unknown next transition issue which will show not enough positive probabilities
  #one row of a transition matrix is zero i.e. 6 means this is an absorbing  state
  categories <- append(categories, getmode(categories))
  #print (categories)
  markovchain_model <- markovchainFit(as.character(categories))
  
  for (k in 1 : as.numeric(input$simulations)){
    next_state_result[k,] <- markovchainSequence(t, markovchain_model$estimate, t0 = categories[length(categories)])
    next_predctions[(n-iterations[1]+1),k,] <- cat_values[as.numeric(next_state_result[k,])]
    }
 
  if (t > 1){
  res[(n-iterations[1]+1),] <- abs(colMeans(next_predctions[(n-iterations[1]+1),,]) - sellout_test[1:t])
  sum_np[(n-iterations[1]+1),] <- colMeans(next_predctions[(n-iterations[1]+1),,])
  }
  else {
    res[(n-iterations[1]+1),] <- abs(mean(next_predctions[(n-iterations[1]+1),,]) - sellout_test[1:t])
    sum_np[(n-iterations[1]+1),] <- mean(next_predctions[(n-iterations[1]+1),,])
  }
}

prediction <- vector()
prediction_lower <- vector()
prediction_upper <- vector()

if (t > 3) {
  
  rowsums.res <- rowSums(res[,1:3])

  index_min <- which.min(rowsums.res)
  index_min <- c(rep(index_min, 3))

  # Use the iterations that have the least error in the testing sample for the future predictions
  index_min <- append(index_min, rep(getmode(index_min),t-3))
  
} else {
  rowsums.res <- rowSums(res)
  index_min <- which.min(rowsums.res)
  index_min <- c(rep(index_min, t))
}


# Update month label if user choose more than 3 Next State
if (t > 3) {
  seg_month <- as.character(seg_month)
  
  i=1
  while (i+3 <= t) {
    lastqtr <- seg_month[length(seg_month)]
    
    if (as.numeric(substring(lastqtr,nchar(lastqtr))) == 4) {
      nextqtr <- lastqtr 
      nextyr <- as.numeric(substr(nextqtr, 3, 4)) + 1
      
      substr(nextqtr, 3, 4) <- as.character(nextyr)
      substring(nextqtr, nchar(nextqtr)) <- as.character(1)
      
      seg_month[length(seg_month) + 1] <- nextqtr
      
    } else {
      nextqtr <- lastqtr 
      nextqtrn <- as.character(as.numeric(substring(nextqtr,nchar(nextqtr))) + 1)
      substring(nextqtr,nchar(nextqtr)) <- nextqtrn
      
      seg_month[length(seg_month) + 1] <- nextqtr
    }
    i <- i + 1
  }
  
  seg_month <- factor(seg_month)
}


for (s in 1:t){
  prediction <- append(prediction,sum_np[index_min[s],s])
   #if all the simulation points are constant, the t.test will show errors because the model will always 
  #predict a same status that's why we add the mode of all the training categories as the extra state
  if (sd(next_predctions[index_min[s],,s]) == 0){
    prediction_lower <- append(prediction_lower, prediction[s])
    prediction_upper <- append(prediction_upper, prediction[s])
  }
  else {
    conf_interv <- t.test(next_predctions[index_min[s],,s],alternative="two.sided",mu = 0,conf.level=0.95)$conf.int
    prediction_lower <- append(prediction_lower, conf_interv[1])
    prediction_upper <- append(prediction_upper, conf_interv[2])
  }
}
prediction <- round(prediction, 0)

if (t > 3) {
  data_length <- c(1:(length(sellout_train) + 3))
  
  pred_length <- tail(data_length, 3)
  
  i=1
  while (i+3 <= t) {
    pred_length[3+i] <- pred_length[3+i-1] + 1
    i <- i+1
  }
  
} else {
  data_length <- c(1:(length(sellout_train) + t))
  pred_length <- tail(data_length, n = t)
  
}

total_length <- unique(c(data_length, pred_length))

#render plot
output$outputplot <- renderPlotly({
  
  p <- plot_ly(x = data_length, y = sellout[1:length(data_length)], type = "bar", marker = list(color = "#008000"), name = "Sellout Actual")
  p <- add_lines(p, x = data_length, y = quota[1:length(data_length)], line = list(color = "orange" ), marker = list(color = "orange"), name = "Quota")
  p <- add_lines(p, x = pred_length, y = prediction_lower,line = list(color = "#33FFEE"),  marker = list(color = "#33FFEE"), name = "Sellout Lower")
  p <- add_lines(p, x = pred_length, y = prediction, line = list(color = "#33B6FF"),marker = list(color = "#33B6FF"), name = "Sellout Prediction")
  p <- add_lines(p, x = pred_length, y = prediction_upper,line = list(color = "#3337FF"), marker = list(color = "#3337FF"), name = "Sellout Upper")
  p <- layout(p, title = paste("Partner", isolate({input$partner}), "with Product Segment", isolate({input$ps}), "in next", t, "states", sep=" "), 
              xaxis = list(title = "", tickvals = 1:length(total_length), ticktext = seg_month[1:length(total_length)], tickfont = list(family = "serif", size = 10)), 
              yaxis = list(title = "Dollar Amount $"), 
              annotations = list(x = pred_length[1], y = prediction[1], text="Forecast", showarrow = T))
  })

output$comp <- renderUI({
  HTML('<strong> Prediction and Actual comparison <strong>')
})

#render prediction table
output$pred <- renderTable({
  prediction <- paste("$", format(prediction, big.mark=","), sep="")
  
  sellout_test <- sellout_test[1:t]
  sellout_test <- round(sellout_test, 0)
  sellout_test <- paste("$", format(sellout_test , big.mark=","), sep="")
  
  dat3 <- rbind(prediction, sellout_test)
  row.names(dat3) <- c("Predictions", "Actuals")
  colnames(dat3) <- as.character(seg_month[pred_length])
  
  dat3
})
 
output$RFMcomp <- renderUI({
  HTML('<strong> Comparison with RFM scores from SQL Server<strong>')
})

#render RFM table
output$rfm_out <- renderTable({

  rfm_table <- matrix(data = NA, nrow = 2, ncol = 4, dimnames = list(c("Total RFM Score", "RFM Segment"), c("Original Prior Period", "Original Current Period", "Dec15 - Apr16", "May16 - Oct16")))
  rfm_table[1,1] <-  paste(prior_rfm_val)
  rfm_table[2,1] <-  paste(prior_rfm_cmb)
  rfm_table[1,2] <-  paste(rfm_val)
  rfm_table[2,2] <-  paste(rfm_cmb)
  rfm_table[1,3] <-  paste(dec_apr_cmb)
  rfm_table[2,3] <-  paste(dec_apr_seg)
  rfm_table[1,4] <-  paste(may_oct_cmb)
  rfm_table[2,4] <-  paste(may_oct_seg)
  rfm_table
  
}, rownames = TRUE)
# , align = 'c'

#render optimized bin selection
 output$bin <- renderText({
   paste("Selected Bin:", paste(index_min[1]))
 })

}

#this observe function is triggered when plot and tables finish rendering to enable selections and close progress message
observe({
  val$Values
  
  #progress$set(message = 'Generating Plot & Tables',
   #            detail = 'This may take a while...')
  
  shinyjs::enable("partner")
  shinyjs::enable("ps")
  shinyjs::enable("sub_region")
  shinyjs::enable("next_state")
  shinyjs::enable("run")
  
 suppressWarnings(progress$close())
  
  })


})
}) 








#######################

#QTpro
#QTpro
##HPE
##HPE
