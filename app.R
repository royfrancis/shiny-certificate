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
                           column(3,style="max-width:450px;background:#ebedef;padding-top:15px;padding-bottom:15px;border-radius:4px;",
                                  div(
                                    style="padding-top:5px;padding-bottom:10px",
                                    div(style="margin-bottom:5px;",strong("Name(s)")),
                                    shinyAce::aceEditor("in_names","John Doe",mode="text",theme="textmate",readOnly=FALSE,height="100px",fontSize=12),
                                    fluidRow(
                                      column(12,
                                             HTML('<div class="help-note"><i class="fas fa-info-circle"></i>  Add one name per row.</div>'),
                                      )
                                    )
                                  ),
                                  fluidRow(
                                    column(12,style="margin-bottom:10px;",
                                           div(style="margin-bottom:5px;",strong("Main text")),
                                           shinyAce::aceEditor("in_txt",txt_default,mode="text",theme="textmate",readOnly=FALSE,height="200px",fontSize=12),
                                           HTML('<div class="help-note"><i class="fas fa-info-circle"></i>  Supports limited markdown formatting. Do not remove &lt;&lt;name&gt;&gt;.</div>'),
                                    )
                                  ),
                                  fluidRow(style="margin-bottom:15px;",
                                    column(6,style=list("padding-right: 3px;"),
                                           numericInput("in_offset_x","X position",min=0.01,max=0.90,value=0.08,step=0.01)
                                    ),
                                    column(6,style=list("padding-left: 3px;"),
                                           numericInput("in_offset_y","Y position",min=0.01,max=0.90,value=0.72,step=0.01)
                                    )
                                  ),
                                  fileInput("in_img_sign","Upload Signature",multiple=FALSE,width="100%"),
                                  fluidRow(
                                    column(12,
                                           HTML('<div class="help-note"><i class="fas fa-info-circle"></i>  Use a PNG image with transparent background.</div>'),
                                    )
                                  ),
                                  fluidRow(style="margin-bottom:15px;margin-top:10px;",
                                    column(6,style=list("padding-right: 3px;"),
                                           numericInput("in_img_sign_width","Signature width",min=0.01,max=0.90,value=0.40,step=0.01)
                                    ),
                                    column(6,style=list("padding-left: 3px;"),
                                           numericInput("in_img_sign_offset_y","Signature Y position",min=0.01,max=0.90,value=0.70,step=0.01)
                                    )
                                  ),
                                  div(style="margin-top:20px;margin-bottom:20px;",downloadButton("btn_download","Download")),
                                  helpText(paste0(format(Sys.time(),"%Y")," • Roy Francis • Version: ",fn_version()))
                           ),
                           column(9,style="max-width:450px;min-width:400px;padding-top:15px;padding-bottom:15px;border-radius:4px;",
                                  sliderInput("in_scale","Image preview scale",min=1,max=5,step=0.2,value=2.2),
                                  fluidRow(
                                    column(12,style="padding-top:5px;padding-bottom:10px",
                                           HTML('<div class="help-note"><i class="fas fa-info-circle"></i>  Scale controls preview below and does not affect download. Preview only shows output for the first name. Download generates all names.</div>')
                                          )
                                  ),
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

    #validate(fn_validate(input$in_text_name))
    #validate(fn_validate(input$in_text_title))
    #validate(fn_validate(input$in_text_dept))
    #validate(fn_validate(input$in_text_email))
    #validate(fn_validate(input$in_text_phone))
    
    fn_validate_img <- function(x){
      if(!is.null(x)){
        y <- tolower(sub("^.+[.]","",basename(x$datapath)))
        if(!y %in% c("jpg","png","jpeg","gif")) return("Signature must be an image in one of JPG/JPEG, PNG or GIF formats.")
        if((x$size/1024/1024)>1) return("Signature image must be less than 1MB in size.")
      }
    }
    
    validate(fn_validate_img(input$in_img_sign))
    
    # if values are available, use them, else use defaults
    if(is.null(input$in_names)){names <- "John Doe"}else{names <- unlist(strsplit(input$in_names,"\n"))}
    if(is.null(input$in_txt)){txt <- txt_default}else{txt <- input$in_txt}
    if(is.null(input$in_offset_x)){offset_x <- 0.08}else{offset_x <- input$in_offset_x}
    if(is.null(input$in_offset_y)){offset_y <- 0.72}else{offset_y <- input$in_offset_y}
    if(is.null(input$in_img_sign_width)){img_sign_width<- 0.40}else{img_sign_width <- input$in_img_sign_width}
    if(is.null(input$in_img_sign_offset_y)){img_sign_offset_y <- 0.70}else{img_sign_offset_y <- input$in_img_sign_offset_y}
    
    if(is.null(input$in_img_sign)) {
      img_sign <- NULL
    }else{
      magick::image_write(
        magick::image_convert(magick::image_read(input$in_img_sign$datapath,tolower(sub("^.+[.]","",basename(input$in_img_sign$datapath))))),
        path=file.path(store$epath,"image.png"),format="png")
      img_sign <- png::readPNG(file.path(store$epath,"image.png"))
    }
    
    bg <- readPNG("./www/bg.png")
    logo_right <- readPNG("./www/nbis.png")
    
    return(list(names=names,txt=txt,offset_x=offset_x,offset_y=offset_y,
                img_sign=img_sign,img_sign_width=img_sign_width,img_sign_offset_y=img_sign_offset_y,
                bg=bg,logo_right=logo_right))
  })

  ## OUT: out_plot ------------------------------------------------------------
  ## plots figure

  output$out_plot <- renderImage({
    
    progress1 <- shiny::Progress$new()
    progress1$set(message="Capturing settings...", value=10)
    
    p <- fn_params()
    
    progress1$set(message="Generating figure...", value=40)
    
    sapply(p$names[1],make_certificate,txt=p$txt,img_bg=p$bg,pos_x=p$offset_x,pos_y=p$offset_y,
           img_sign=p$img_sign,img_sign_width=p$img_sign_width,img_sign_offset_y=p$img_sign_offset_y,logo_right=p$logo_right,
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
      "offset_x: ",input$in_offset_x,"\n",
      "offset_y: ",input$in_offset_y,"\n",
    ))
  })

  ## FN: fn_download -----------------------------------------------------------
  ## function to download a zipped file with images

  fn_download <- function(){
    
    p <- fn_params()
    
    sapply(p$names,make_certificate,txt=p$txt,img_bg=p$bg,pos_x=p$offset_x,pos_y=p$offset_y,
           img_sign=p$img_sign,img_sign_width=p$img_sign_width,img_sign_offset_y=p$img_sign_offset_y,logo_right=p$logo_right,
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
