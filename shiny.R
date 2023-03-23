library(shiny)
library(shinyWidgets)
library(plotly)
library(ggseg3d)
library(ggsegBrodmann)
library(ggsegEconomo)
library(ggsegDesterieux)
library(dplyr)
library(tidyr)

library(tictoc)
library(profvis)
# Read individual trials
df<-read.csv("finaldata_ggseg3d.csv")

brain_models_list <- list(brodmann = brodmann_3d,dk = dk_3d,economo=economo_3d,desterieux=desterieux_3d)

# 

## Functions

#Add transparency to brain model
mesh_transparency <- function(brain_model,chosen_structures) {
  
  modified_mesh = get(brain_model, brain_models_list)%>% 
    unnest(cols = ggseg_3d)%>% 
    filter(annot %in% chosen_structures) %>% 
    mutate(colour = paste(colour,'FC',sep="")) 
  
  return(modified_mesh)
}

get_ggseg3d_plot <- function(df_filtered,plot_title,show_mesh,brain_model,chosen_structures) {
  if (show_mesh){
    
    scene = list(camera = list(eye = list(x=-1.5, y=-3, z = -20)))
    transparent_mesh= mesh_transparency(brain_model=brain_model,chosen_structures = chosen_structures)
    ggsegplot = ggseg3d(.data=transparent_mesh, atlas=brodmann_3d)    %>% 
      add_glassbrain() %>% 
      layout(title = list(text = plot_title, y = .95, xref = "plot"), scene = scene) %>% 
      pan_camera("right medial")%>%  #“left lateral”, “left medial”, “right lateral”, “right medial”
      add_trace(type="scatter3d",x = df_filtered$x,y = df_filtered$y,z = df_filtered$z,
                color=df_filtered$value,mode = "markers", name = "Measured value",size=3,
                #marker = list(color = df_filtered$value,size = 3),
                showlegend = F)
    
    
    
    return(ggsegplot)
  }
  else{
    ggsegplot = plot_ly()   %>% 
      add_glassbrain() %>% 
      layout(title =plot_title) %>% 
      pan_camera("right medial")%>%  #“left lateral”, “left medial”, “right lateral”, “right medial”
      add_trace(type="scatter3d",x = df_filtered$x,y = df_filtered$y,z = df_filtered$z,
                color=df_filtered$value,mode = "markers", name = "Measured value",size=3,
                #marker = list(color = df_filtered$value,size = 3),
                showlegend = F)
    
    
    
    return(ggsegplot)
  }
  

  
}



# Time difference of 1.000327 mins

### Shiny

ui <- fluidPage(
  headerPanel('Brain visualization'),
  sidebarPanel(
    # Input: Select a file ----
    fileInput("coordinates", "Choose CSV File with vertex coordinates",
              multiple = FALSE,
              accept = c("text/csv",
                         "text/comma-separated-values,text/plain",
                         ".csv")),
    switchInput(inputId = "booleanShowMesh", value = TRUE,label="Brain mesh",width='75%',size="mini"),
    
    selectInput('brain_model','Brain model', names(brain_models_list)),
    pickerInput("brain_structure", "Brain structures",choices = NULL,multiple = TRUE),
    
    selectInput('sub','Subject', unique(df$Subject)),
    selectInput('freq','Frequency', unique(df$frequency)),
    sliderInput("time", "Time:",
                min = 0, max = 382,
                value = 0, step = 2)

    ),
  mainPanel(
      splitLayout(cellWidths = c("50%","50%"),plotlyOutput('plot1'), plotlyOutput('plot2'))
    
    # plotlyOutput('plot1'),
    # plotlyOutput('plot2')
    
  )
)

server <- function(input, output,session) {
  
  # Read data from uploaded file
  read_coordinates <- reactive({
    file <- input$coordinates
    if(is.null(file)) {
      return(NULL)
    }
    read.csv(file$datapath)
  })
  
  
  # Filter the dataset based on the user's color selection
  filter_subject_frequency <- reactive({
    df %>% filter(Subject==input$sub & side=="l" & frequency==input$freq)
    
  })
  
  filter_time <- reactive({
    df_filtered = filter_subject_frequency()
    df_filtered %>% filter(t==input$time)
    
  })%>%
    bindCache(input$sub,input$freq,input$time)
  

  output$plot1 <- renderPlotly({
    
    coordinates <- read_coordinates()

    df_filtered <- filter_time()
    
    if (!is.null(coordinates)){
      df_filtered_no_coords <-subset(df_filtered, select=-c(x,y,z))
      
      df_filtered<- inner_join(df_filtered_no_coords,coordinates , by = "vertex")    
      
    }
    
    
    #toc()
    
    #tic("execution time filter 2")
    df_filtered %>% filter(Tone==0)
    #toc()
    
    #tic("execution time 3d plot")
    fig <-  get_ggseg3d_plot(df_filtered, "\n before tone",input$booleanShowMesh,input$brain_model,input$brain_structure)
    #toc()
    fig
  })
  
  output$plot2 <- renderPlotly({
    
    coordinates <- read_coordinates()
    
    df_filtered <- filter_time()
    
    if (!is.null(coordinates)){
      df_filtered_no_coords <-subset(df_filtered, select=-c(x,y,z))
      
      df_filtered<- inner_join(df_filtered_no_coords,coordinates , by = "vertex")    
      
    }
    
    df_filtered %>% filter(Tone==1)
    
    fig <-  get_ggseg3d_plot(df_filtered, "\n after tone",input$booleanShowMesh,input$brain_model,input$brain_structure)
    fig
  })
  
  #Brain structure dropdown
  
  
  observeEvent(input$brain_model, {
    chosen_model = get(input$brain_model, brain_models_list)
    
    model_structures <- chosen_model %>% 
      unnest(cols = ggseg_3d) %>% pull(annot)
    
    updatePickerInput(session, "brain_structure", choices = model_structures,selected = model_structures)
    
  })
  

  

}


shinyApp(ui,server)

