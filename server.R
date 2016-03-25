library(googleCharts)
library(ggplot2)
library(googleVis)
library("reshape2")
library(memoise)
source("helper.R")

#options(shiny.trace=TRUE)

shinyServer(function(input, output, session) {
  
  chart_data <- prepare_dataframe_google()
  
  foodCategoryProducer <- reactive({
    return (append(c("All.food"), input$foodCategoryProducer))
  })
    
  foodCategory <- reactive({
    c = if (is.null(input$foodCategory)) c("All.food") else input$foodCategory
    return (c)
  })
  
  output$plot_pie <- renderGvis({
    Pie <- gvisPieChart(category_percent,
                        options=list(
                          title="Food Categories weighted by their relative importance or share of consumer expenditures.",
                          width="100%",
                          height="300px"
                        )
                        )
  })
  
  output$plot_category_percent_bar <- renderGvis({
    
    Column <- gvisColumnChart(category_percent,
                              options=list(
                                title="Food Categories weighted by their relative importance or share of consumer expenditures.",
                                vAxis="{title:'Percent'}",
                                hAxis="{title:'Food Category'}",
                                width="100%",
                                height="350px",
                                legend= "{position: 'none'}"
                              )
    )
    
  })
  
  
  food_price_bar_data <- reactive({
    
    filter(food.price, year %in% c(input$leftYear, input$rightYear) );

  })
  
  output$plot_food_price_bar <- renderGvis({
    
    Column <- gvisColumnChart(food_price_bar_data(),
                              options=list(
                                vAxis="{title:'Percent Change'}",
                                hAxis="{title:'Year'}",
                                width="100%",
                                height="350px"
                              )
                              )
    
  })
  
  #object for food prices line
  output$plot_food_price <- renderGvis({
    
    Line <- gvisLineChart(food.price, xvar="year", yvar=foodCategory(),
                          options=list(
                            title="Food Price Changes",
                            width="100%",
                            height="300px",
                            backgroundColor="#D3D3D3",                          
                            vAxis="{title:'Percent Change'}",
                            hAxis="{title:'Year', showTextEvery:5}"
                          )
                          )
    
    return (Line)
    
  })
  
  
  output$plot_food_correlation_ui <- renderUI({
    
    #cor_zoom_width = cor_zoom_width * input$zoom
    #cor_zoom_height = cor_zoom_height * input$zoom
    
    #plotOutput("plot", width = cor_zoom_width * input$zoom, height = cor_zoom_height * input$zoom)
    
    plotOutput("plot_food_correlation",
               width = cor_zoom_width * input$zoom, 
               height = cor_zoom_height * input$zoom
    )
  })
  
#   output$plot_food_correlation <- renderPlot({
#     
#     plot(food.category.cor)
#     
#   })
  
  # food price vs producer prie
  output$plot_food_vs_producer <- renderGvis({
    
    Line <- gvisLineChart(food_producer_join, xvar="year", yvar=foodCategoryProducer(),
                          options=list(
                            title="Consumer Food Price Changes vs Producer Price Changes",
                            width="100%",
                            height="300px",
                            backgroundColor="#D3D3D3",                          
                            vAxis="{title:'Percent Change'}",
                            hAxis="{title:'Year', showTextEvery:5}"
                          )
    )
    
  })
  
  # food price vs producer prie
  output$plot_food_vs_all <- renderGvis({
    
    food.and.all = inner_join(food.price, rename(all.price, All.item = percent.change), by = c("year"))
    
    Line <- gvisLineChart(food.and.all, xvar="year", yvar=c('All.item','All.food'),
                          options=list(
                            title="Price Change in All-Items and in Food",
                            backgroundColor="#D3D3D3",                          
                            vAxis="{title:'Percent Change'}",
                            hAxis="{title:'Year', showTextEvery:5}"
                          )
                          )
  })
  
  
  output$summary <- renderPrint({
    
  })
  
   output$table <- DT::renderDataTable({
     DT::datatable(food.price)
   })
   
   output$plot_corr <- renderPlot({
     
     cormat <- round(cor(food.category.cor),2)
     
     cormat <- reorder_cormat(cormat)
     upper_tri <- get_upper_tri(cormat)
     
     # Melt the correlation matrix
     melted_cormat <- melt(upper_tri, na.rm = TRUE)
     
     plot_c = ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
       geom_tile(color = "white")+
       scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                            midpoint = 0, limit = c(-1,1), space = "Lab", 
                            name="Correlation") +
       theme_minimal()+ 
       theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                        size = 12, hjust = 1))
     #coord_fixed()
     
     plot_c = plot_c + 
       geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
       theme(
         axis.title.x = element_blank(),
         axis.title.y = element_blank(),
         panel.grid.major = element_blank(),
         panel.border = element_blank(),
         panel.background = element_blank(),
         axis.ticks = element_blank(),
         legend.justification = c(1, 0),
         legend.position = c(0.6, 0.7),
         legend.direction = "horizontal")+
       guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                                    title.position = "top", title.hjust = 0.5))
     
     return (plot_c)
   })
   
  
})