
source('script_analysis.R')
Wtempstats_df
length(unique(Wtempstats_df$state_site)) # 224

## UI
# Use a fluid Bootstrap layout
ui <- fluidPage(    
  
  # Give the page a title
  titlePanel("Water Temperature at Nationwide USGS Gages"),
  
  # Generate a row with a sidebar
  sidebarLayout(      
    
    # Define the sidebar with one input
    sidebarPanel('Pick a Site to Plot (listed alphabetically by state)',
      selectInput("site", "State : Site", 
                  choices=unique(Wtempstats_df$state_site))
    ),
    
    # Create a spot for the barplot
    mainPanel(
      plotOutput("warm_stripe_plot")  
    )
    
  )
)

## Server

# Define a server for the Shiny app
server <- function(input, output) {
  
  # Fill in the spot we created for a plot
  output$warm_stripe_plot <- renderPlot({
    
    Wtempstats_df %>% 
      
      filter(state_site == input$site) %>% 
      
      ggplot(aes(x=year, y=1, fill=mean)) +
      geom_bar(color=NA, width=1, stat="identity") + 
      scale_fill_distiller(type= "div", palette="RdBu") +  
      scale_x_continuous(expand=c(0,0)) +
      scale_y_continuous(expand=c(0,0)) +
      ggtitle(paste(input$site)) +
      labs(fill="Mean Yearly Temperature (C)", x='Year') +
      theme_minimal() + 
      theme(legend.position = "bottom", axis.title.y = element_blank(),
            axis.ticks.y = element_blank(), axis.text.y = element_blank(), 
            plot.title = element_text(hjust=0.5), legend.key.width = unit(3,"line"),
            panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank())
  })
}

shinyApp(ui, server)
