## certificate
## R shinyapp to generate workshop certificates
## 2020 Roy Mathew Francis

source("functions.R")

# UI ---------------------------------------------------------------------------

ui <- fluidPage(theme=shinytheme("flatly"),
                tags$head(tags$link(rel="stylesheet",type="text/css",href="styles.css")),
                fixedRow(
                  column(12,style="margin:15px;",
                         fluidRow(style="margin-bottom:10px;",
                                  span(tags$img(src='nbis.png',style="height:18px;"),style="vertical-align:top;display:inline-block;"),
                                  span(tags$h4("•",style="margin:0px;margin-left:6px;margin-right:6px;"),style="vertical-align:top;display:inline-block;"),
                                  span(tags$h4(strong("Workshop Certificate"),style="margin:0px;"),style="vertical-align:middle;display:inline-block;")
                         ),
                         fixedRow(
                           column(3,class="box-left",
                                  div(class="box-block",
                                    textAreaInput("in_names","",value="John Doe",resize="vertical",width="100%"),
                                    shinyBS::bsTooltip(id="in_names",title="Participant name(s). Add one name per row.",placement="top",trigger="hover"),
                                  ),
                                  fluidRow(
                                    column(12,
                                           textAreaInput("in_txt","",value=txt_default,resize="vertical",width="100%",height="250px"),
                                           shinyBS::bsTooltip(id="in_txt",title="Main text. Supports limited markdown formatting. Do not remove &lt;&lt;name&gt;&gt;.",placement="top",trigger="hover"),
                                    )
                                  ),
                                  fluidRow(
                                    column(4,class="no-pad-right",
                                           numericInput("in_offset_x","",min=0,max=1.0,value=0.12,step=0.01),
                                           shinyBS::bsTooltip(id="in_offset_x",title="Horizontal content position. Value between 0 and 1.",placement="top",trigger="hover"),
                                    ),
                                    column(4,class="no-pad-right no-pad-left",
                                           numericInput("in_offset_y","",min=0,max=1.0,value=0.7,step=0.01),
                                           shinyBS::bsTooltip(id="in_offset_y",title="Vertical main text position. Value between 0 and 1.",placement="top",trigger="hover"),
                                    ),
                                    column(4,class="no-pad-left",
                                           numericInput("in_text_size_main","",min=3.0,max=6.0,value=4.4,step=0.1),
                                           shinyBS::bsTooltip(id="in_text_size_main",title="Font size of main text. Value between 3.0 and 6.0.",placement="top",trigger="hover"),
                                    )
                                  ),
                                  fileInput("in_im_sign","",multiple=FALSE,width="100%",placeholder="Upload signature"),
                                  fluidRow(
                                    column(12,
                                           HTML('<div class="help-note"><i class="fas fa-info-circle"></i> For signature, use a PNG image with transparent background.</div>'),
                                    )
                                  ),
                                  fluidRow(
                                    column(6,class="no-pad-right",
                                           numericInput("in_im_sign_width","",min=0.01,max=0.90,value=0.28,step=0.01),
                                           shinyBS::bsTooltip(id="in_im_sign_width",title="Signature size. Value between 0.01 & 0.9.",placement="top",trigger="hover"),
                                    ),
                                    column(6,class="no-pad-left",
                                           numericInput("in_im_sign_offset_y","",min=0,max=1.0,value=0.67,step=0.01),
                                           shinyBS::bsTooltip(id="in_im_sign_offset_y",title="Signature position. Value between 0 & 1.",placement="top",trigger="hover"),
                                    )
                                  ),
                                  div(style="margin-top:20px;margin-bottom:25px;",downloadButton("btn_download","Download")),
                                  div(style="font-size:0.9em;",paste0(format(Sys.time(),'%Y'),' • Roy Francis • Version: ',fn_version()))
                           ),
                           column(9,
                                  #sliderInput("in_scale","Image preview scale",min=1,max=5,step=0.2,value=2.2),
                                  #fluidRow(
                                  #  column(12,style="padding-top:5px;padding-bottom:10px",
                                  #         HTML('<div class="help-note"><i class="fas fa-info-circle"></i>  Scale controls preview below and does not affect download. Preview only shows output for the first name. Download generates all names.</div>')
                                  #        )
                                  #),
                                  div(class="img-output",
                                    imageOutput("out_plot",width="auto",height="auto")
                                  )
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
    
    fn_validate_im <- function(x){
      if(!is.null(x)){
        y <- tolower(sub("^.+[.]","",basename(x$datapath)))
        if(!y %in% c("jpg","png","jpeg","gif")) return("Image must be one of JPG/JPEG, PNG or GIF formats.")
        if((x$size/1024/1024)>1) return("Image must be less than 1MB in size.")
      }
    }
    
    validate(fn_validate_im(input$in_im_sign))
    
    # if values are available, use them, else use defaults
    if(is.null(input$in_names)){names <- "John Doe"}else{names <- unlist(strsplit(input$in_names,"\n"))}
    if(is.null(input$in_txt)){txt <- txt_default}else{txt <- input$in_txt}
    if(is.null(input$in_offset_x)){offset_x <- 0.12}else{offset_x <- input$in_offset_x}
    if(is.null(input$in_offset_y)){offset_y <- 0.70}else{offset_y <- input$in_offset_y}
    if(is.null(input$in_text_size_main)){text_size_main <- 4.4}else{text_size_main <- input$in_text_size_main}
    if(is.null(input$in_im_sign_width)){im_sign_width<- 0.28}else{im_sign_width <- input$in_im_sign_width}
    if(is.null(input$in_im_sign_offset_y)){im_sign_offset_y <- 0.67}else{im_sign_offset_y <- input$in_im_sign_offset_y}
    
    if(is.null(input$in_im_sign)) {
      im_sign <- NULL
    }else{
      magick::image_write(
        magick::image_convert(magick::image_read(input$in_im_sign$datapath),format="png"),
        path=file.path(store$epath,"image.png"),format="png")
      im_sign <- png::readPNG(file.path(store$epath,"image.png"))
    }
    
    bg <- readPNG("./www/bg.png")
    logo_right <- readPNG("./www/nbis.png")
    
    return(list(names=names,txt=txt,offset_x=offset_x,offset_y=offset_y,text_size_main=text_size_main,
                im_sign=im_sign,im_sign_width=im_sign_width,im_sign_offset_y=im_sign_offset_y,
                bg=bg,logo_right=logo_right))
  })

  ## OUT: out_plot ------------------------------------------------------------
  ## plots figure

  output$out_plot <- renderImage({
    
    progress1 <- shiny::Progress$new()
    progress1$set(message="Capturing settings...", value=10)
    
    p <- fn_params()
    
    progress1$set(message="Generating figure...", value=40)
    
    sapply(p$names[1],make_certificate,txt=p$txt,im_bg=p$bg,pos_x=p$offset_x,pos_y=p$offset_y,text_size_main=p$text_size_main,
           im_sign=p$im_sign,im_sign_width=p$im_sign_width,im_sign_offset_y=p$im_sign_offset_y,logo_right=p$logo_right,
           format_export="png",path=store$epath)
    fname <- list.files(path=store$epath,pattern="png",full.names=TRUE)[1]
    
    progress1$set(message="Completed.", value=100)
    progress1$close()
    
    scaling <- 2.6
    return(list(src=fname,contentType="image/png",
                height=round(297*scaling,0),
                width=round(210*scaling,0),
                alt="certificate"))
  },deleteFile=TRUE)
  
  ## OUT: out_display -------------------------------------------------------
  ## prints general variables for debugging

  output$out_display <- renderPrint({
    cat(paste0(
      "name: ",input$in_names,"\n",
      "txt: ",input$in_txt,"\n",
      "offset_x: ",input$in_offset_x,"\n",
      "offset_y: ",input$in_offset_y,"\n",
    ))
  })

  ## FN: fn_download -----------------------------------------------------------
  ## function to download a zipped file with images

  fn_download <- function(){
    
    p <- fn_params()
    
    sapply(p$names,make_certificate,txt=p$txt,im_bg=p$bg,pos_x=p$offset_x,pos_y=p$offset_y,text_size_main=p$text_size_main,
           im_sign=p$im_sign,im_sign_width=p$im_sign_width,im_sign_offset_y=p$im_sign_offset_y,logo_right=p$logo_right,
           format_export="pdf",path=store$epath)
    
    epathn <- file.path(store$epath,"certificates.zip")
    if(exists(epathn)) unlink(epathn)
    
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
