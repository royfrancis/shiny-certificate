## certificate
## R shinyapp to generate workshop certificates
## 2020 Roy Mathew Francis

source("functions.R")

# UI ---------------------------------------------------------------------------

ui <- fluidPage(theme=shinytheme("flatly"),
  fixedRow(
      column(12,style="margin:15px;",
             fluidRow(style="margin-bottom:10px;",
               span(tags$img(src='nbis.png',style="height:18px;"),style="vertical-align:top;display:inline-block;"),
               span(tags$h4("•",style="margin:0px;margin-left:6px;margin-right:6px;"),style="vertical-align:top;display:inline-block;"),
               span(tags$h4(strong("Workshop Certificate"),style="margin:0px;"),style="vertical-align:middle;display:inline-block;")
             ),
    fixedRow(
    column(3,style="max-width:450px;background:#ebedef;padding-top:15px;padding-bottom:15px;border-radius:4px;",
     div(
       style="padding-top:5px;padding-bottom:10px",
      div(style="margin-bottom:5px;",strong("Name(s)")),
      shinyAce::aceEditor("in_names","John Doe",mode="text",theme="textmate",readOnly=FALSE,height="100px",fontSize=12),
      helpText("Add one name per row.",style="display:inline;")
     ),
      div(style="margin-bottom:5px;",strong("Main text")),
      shinyAce::aceEditor("in_txt",txt_default,mode="text",theme="textmate",readOnly=FALSE,height="200px",fontSize=12),
      #textAreaInput("in_txt","Main text",value=txt_default,height="150px",resize="vertical"),
      div(style="margin-bottom:5px;",helpText("Limited markdown formatting is supported. Do not modify/remove <<name>>.",style="display:inline;")),
      sliderInput("in_pos_x","X position",min=0.01,max=0.90,value=0.08,step=0.01),
      sliderInput("in_pos_y","Y position",min=0.01,max=0.90,value=0.72,step=0.01),
      downloadButton("btn_download","Download"),
      tags$hr(),
      helpText(paste0("2020 | Roy Francis | Version: ",fn_version()))
    ),
    column(9,style="max-width:450px;min-width:400px;padding-top:15px;padding-bottom:15px;border-radius:4px;",
      sliderInput("in_scale","Image preview scale",min=1,max=5,step=0.2,value=2.2),
      fluidRow(
        column(12,
               style="padding-top:5px;padding-bottom:10px",
                      icon("info-circle",class="fa-lg"),
                      helpText("Scale only controls preview below and does not affect download. Preview only shows output for the first name. Download generates all names.",style="display:inline;")
               )
        ),
      tags$br(),
      imageOutput("out_plot")
      #verbatimTextOutput("out_display")
    )
    )
  )
)
)

# SERVER -----------------------------------------------------------------------

server <- function(input, output, session) {

  ## get temporary directory
  store <- reactiveValues(epath=tempdir())
  
  ## FN: fn_params ------------------------------------------------------------
  ## function to get plot params

  fn_params <- reactive({

    #validate(fn_validate(input$in_text_name))
    #validate(fn_validate(input$in_text_title))
    #validate(fn_validate(input$in_text_dept))
    #validate(fn_validate(input$in_text_email))
    #validate(fn_validate(input$in_text_phone))
    #validate(fn_validate(input$in_image_profile))
    
    # if values are available, use them, else use defaults
    if(is.null(input$in_names)){names <- "John Doe"}else{names <- unlist(strsplit(input$in_names,"\n"))}
    if(is.null(input$in_txt)){txt <- txt_default}else{txt <- input$in_txt}
    if(is.null(input$in_pos_x)){pos_x <- 0.08}else{pos_x <- input$in_pos_x}
    if(is.null(input$in_pos_y)){pos_y <- 0.72}else{pos_y <- input$in_pos_y}
    bg <- readPNG("./www/bg.png")
    logo_right <- readPNG("./www/nbis.png")
    
    return(list(names=names,txt=txt,pos_x=pos_x,pos_y=pos_y,bg=bg,logo_right=logo_right))
  })

  ## OUT: out_plot ------------------------------------------------------------
  ## plots figure

  output$out_plot <- renderImage({
    
    progress1 <- shiny::Progress$new()
    progress1$set(message="Capturing settings...", value=10)
    
    p <- fn_params()
    
    progress1$set(message="Generating figure...", value=40)
    
    sapply(p$names[1],make_certificate,txt=p$txt,img_bg=p$bg,pos_x=p$pos_x,pos_y=p$pos_y,logo_right=p$logo_right,
           format_export="png",path=store$epath)
    fname <- list.files(path=store$epath,pattern="png",full.names=TRUE)[1]
    
    progress1$set(message="Completed.", value=100)
    progress1$close()
    
    return(list(src=fname,contentType="image/png",
                height=round(297*input$in_scale,0),
                width=round(210*input$in_scale,0),
                alt="certificate"))
  },deleteFile=TRUE)
  
  ## OUT: out_display -------------------------------------------------------
  ## prints general variables for debugging

  output$out_display <- renderPrint({
    cat(paste0(
      "name: ",input$in_names,"\n",
      "txt: ",input$in_txt,"\n",
      "pos_x: ",input$in_pos_x,"\n",
      "pos_y: ",input$in_pos_y,"\n",
    ))
  })

  ## FN: fn_download -----------------------------------------------------------
  ## function to download a zipped file with images

  fn_download <- function(){
    
    #progress$set(message="Capturing settings...", value=12)
    p <- fn_params()
    
    #progress$set(message="Generating PDFs...", value=40)
    sapply(p$names,make_certificate,txt=p$txt,img_bg=p$bg,pos_x=p$pos_x,pos_y=p$pos_y,
           logo_right=p$logo_right,path=store$epath)
    
    #progress$set(message="Discarding old files...", value=68)
    epathn <- file.path(store$epath,"certificates.zip")
    if(exists(epathn)) unlink(epathn)
    
    #progress$set(message="Zipping plots...", value=78)
    zip(epathn,files=list.files(path=store$epath,pattern="pdf",full.names=TRUE))
    unlink(list.files(path=store$epath,pattern="pdf",full.names=TRUE))
  }

  ## DHL: btn_download ---------------------------------------------------------
  ## download handler for downloading zipped file

  output$btn_download <- downloadHandler(
    filename="certificates.zip",
    content=function(file) {
      
      progress <- shiny::Progress$new()
      progress$set(message="Generating PDFs...", value=45)
      fn_download()
      
      progress$set(message="Downloading zipped file...", value=90)
      file.copy(file.path(store$epath,"certificates.zip"),file,overwrite=T)
      
      progress$set(message="Completed.", value=100)
      progress$close()
    }
  )
}

shinyApp(ui=ui, server=server)
