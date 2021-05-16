# shiny-certificate
# functions

library(Cairo)
library(curl)
library(ggplot2)
library(ggtext)
library(magick)
library(png)
library(shiny)
library(shinyBS)
library(shinythemes)
library(showtext)

if(!"gfont" %in% sysfonts::font_families()) font_add_google("Lato","gfont")
showtext_opts(dpi=300)

fn_dir <- function(session)
{
  wd <- file.path(tempdir(check=TRUE),session$token)
  if(!dir.exists(wd)) dir.create(wd)
  cat(paste0("Working directory: ",wd,"\n"))
  return(wd)
}

# fn_version
fn_version <- function() {
  return("v1.1.4")
}

# validation
fn_validate <- function(input,message1,message2,message3)
{

  if(missing(message1)) message1 <- "Input is missing."
  gcheck <- length(grep("Argument \\'\\w+\\' missing",message1))
  if(gcheck == 1)
  {
    m1 <- sub("Argument ","",message1)
    m1 <- sub(" missing.","",m1)
  }

  if (all(is.null(input))) {
    if(missing(message1)) message1 <- "Input is missing."
    print(message1)
  } else if (is.numeric(input) | is.list(input)) {
    if(all(is.na(input)))
    {
      if(missing(message2))
      {
        if(gcheck==1) message2 <- paste0("Argument ",m1," is NA.",sep="")
        if(gcheck!=1) message2 <- "Input is NA."
      }
      print(message2)
    }
  } else if (is.character(input)) {
    if(all(nchar(input) == 0))
    {
      if(missing(message3))
      {
        if(gcheck==1) message3 <- paste0("Argument ",m1," is empty.",sep="")
        if(gcheck!=1) message3 <- "Input is empty."
      }
      print(message3)
    }
  } else {
    NULL
  }
}

fn_validate_numeric <- function(input) {
  if(is.na(as.numeric(input))) print("Input is not a numeric.")
}

# im_dims_right ------------------------------------------------------------

#' @title im_dims_right
#' @description Computes image dimensions for a right aligned image
#' @param im An image object from readPNG()
#' @param im_width Final image width in plot units
#' @param im_offset_x Image distance from right x edge
#' @param im_offset_y Image distance from top y edge
#' @param canvas_height Canvas height in any units
#' @param canvas_width Canvas width in any units
#'
im_dims_right <- function(im,im_width,im_offset_x,im_offset_y,canvas_height,canvas_width) {

  w_scaler <- canvas_width/canvas_height
  im_height <- ((im_width*nrow(im))/ncol(im))*w_scaler
  im_x2 <- 1-im_offset_x
  im_x1 <- im_x2-im_width
  im_y2 <- 1-im_offset_y
  im_y1 <- round(im_y2-im_height,3)

  return(list(xmin=im_x1,xmax=im_x2,ymin=im_y1,ymax=im_y2))
}

# im_dims_left ------------------------------------------------------------

#' @title im_dims_left
#' @description Computes image dimensions for a left aligned image
#' @param im An image object from readPNG()
#' @param im_width Final image width in plot units
#' @param im_offset_x Image distance from left x edge
#' @param im_offset_y Image distance from top y edge
#' @param canvas_height Canvas height in any units
#' @param canvas_width Canvas width in any units
#' 
im_dims_left <- function(im,im_width,im_offset_x,im_offset_y,canvas_height,canvas_width) {
  
  w_scaler <- canvas_width/canvas_height
  im_height <- ((im_width*nrow(im))/ncol(im))*w_scaler
  im_x1 <- im_offset_x
  im_x2 <- im_x1+im_width
  im_y2 <- 1-im_offset_y
  im_y1 <- round(im_y2-im_height,3)
  
  return(list(xmin=im_x1,xmax=im_x2,ymin=im_y1,ymax=im_y2))
}

# make_certificate ------------------------------------------------------------

#' @title make_certificate
#' @description Generates a PDF certificate
#' @param name [character] Name of the student
#' @param txt [character] A text string
#' @param pos_x [numeric] X-axis position of all elements
#' @param pos_y [numeric] Y-axis position of main text
#' @param text_size_main [numeric] Size of main text.
#' @param im_bg [array] Background image, output from readPNG().
#' @param logo_right [array] Right logo, output from readPNG().
#' @param logo_right_width [numeric] Width of the logo in plot units
#' @param logo_right_offset_x [numeric] Logo distance from x edge
#' @param logo_right_offset_y [numeric] Logo distance from y edge
#' @param im_sign [array] Signature png image, output from readPNG().
#' @param im_sign_width [numeric] Width of the signature in plot units
#' @param im_sign_offset_y [numeric] Position of signature on y axis
#' @param height [numeric] Height of output document
#' @param width [numeric] Width of output document
#' @param format_export [character] A character indicating export format. "jpeg","png" or "pdf".
#' @param path_export [character] Export path
#' @return Does not return any value. Exports an A4 sized PDF.
#'
make_certificate <- function(name="John Doe",txt,pos_x=0.12,pos_y=0.70,text_size_main=4.4,im_bg=NULL,
                             logo_right=NULL,logo_right_width=0.11,logo_right_offset_x=0.14,logo_right_offset_y=0.086,
                             im_sign=NULL,im_sign_width=0.28,im_sign_offset_y=0.67,
                             width=210,height=297,format_export="pdf",path_export=".") {

  txt <- gsub("\n","<br>",txt)
  txt <- sub("<<name>>",paste0("<span style='font-size:24pt;'>",name,"</span>"),txt)
  txt <- gsub("- ","• ",txt)

  dfr <- data.frame(label=txt,x=pos_x,y=pos_y)
  pos_y_upper <- 0.83
  p <- ggplot()

  if(!is.null(im_bg)) p <- p+annotation_raster(im_bg,xmin=0,xmax=1,ymin=0,ymax=1)
  if(!is.null(logo_right)) {
    dims_logo_right <- im_dims_right(logo_right,logo_right_width,logo_right_offset_x,logo_right_offset_y,height,width)
    p <- p + annotation_raster(logo_right,xmin=dims_logo_right$xmin,xmax=dims_logo_right$xmax,
                                          ymin=dims_logo_right$ymin,ymax=dims_logo_right$ymax)
  }

  # add signature
  if(!is.null(im_sign)) {
    im_sign_offset_x <- pos_x
    dims_im_sign <- im_dims_left(im_sign,im_sign_width,im_sign_offset_x,im_sign_offset_y,height,width)
    p <- p + annotation_raster(im_sign,xmin=dims_im_sign$xmin,xmax=dims_im_sign$xmax,
                               ymin=dims_im_sign$ymin,ymax=dims_im_sign$ymax)
  }
  
   p <- p+
    ggtext::geom_richtext(data=dfr,aes(x,y,label=label),hjust=0,vjust=1,size=text_size_main,family="gfont",colour="grey10",
                  fill=NA,label.color=NA,label.padding=grid::unit(rep(0,4),"pt"),lineheight=1.45)+
    geom_text(data=data.frame(label="Certificate",x=pos_x,y=pos_y_upper),
              aes(x,y,label=label),hjust=0,size=13,family="gfont",fontface="bold",colour="white")+
    geom_text(data=data.frame(label="NBIS • TRAINING",x=pos_x,y=pos_y_upper+0.06),
              aes(x,y,label=label),hjust=0,size=6,family="gfont",colour="white")+
    geom_text(data=data.frame(label="www.nbis.se",x=pos_x,y=pos_y_upper+0.08),
              aes(x,y,label=label),hjust=0,size=4,family="gfont",colour="white")+
    ggtext::geom_richtext(data=data.frame(label="This is a certificate of participation. Participants are not evaluated.<br>
    **National Bioinformatics Infrastructure Sweden (NBIS)** is a distributed national research infrastructure supported<br>
    by the Swedish Research Council, Science for Life Laboratory, Knut and Alice Wallenberg Foundation and all major<br>
    Swedish universities in providing state-of-the-art bioinformatics to the Swedish life science research community.",x=pos_x,y=0.10),
               aes(x,y,label=label),hjust=0,size=3,family="gfont",colour="grey10",lineheight=1.3,fill=NA,label.color=NA,label.padding=grid::unit(rep(0,4),"pt"))+
    coord_cartesian(xlim=c(0,1),ylim=c(0,1))+
    scale_x_continuous(expand=c(0,0))+
    scale_y_continuous(expand=c(0,0))+
    labs(x="",y="")+
    theme(legend.position="none",
          axis.text=element_blank(),
          axis.title=element_blank(),
          panel.grid=element_blank(),
          panel.spacing=unit(0,"lines"),
          strip.background=element_blank(),
          strip.text=element_blank(),
          axis.ticks=element_blank(),
          panel.background=element_blank(),
          plot.background=element_blank(),
          plot.margin=margin(0,0,0,0),
          axis.ticks.length=unit(0,"pt"))

  message(paste0("Exported ",file.path(path_export,paste0("certificate-",tolower(gsub(" ","-",name)),".",format_export)),"."))

  if(format_export=="pdf") {
    grDevices::cairo_pdf(file=file.path(path_export,paste0("certificate-",tolower(gsub(" ","-",name)),".pdf")),
                         height=round(height/25.4,2),width=round(width/25.4,2))
    showtext::showtext_begin()
    print(p)
    showtext::showtext_end()
    dev.off()
  }

  if(format_export=="png") {
    grDevices::png(file=file.path(path_export,paste0("certificate-",tolower(gsub(" ","-",name)),".png")),
                   width=width,height=height,units="mm",res=300,type="cairo")
    showtext::showtext_begin()
    print(p)
    showtext::showtext_end()
    dev.off()
  }

  if(format_export=="jpeg") {
    grDevices::jpeg(file=file.path(path_export,paste0("certificate-",tolower(gsub(" ","-",name)),".jpeg")),
                   width=width,height=height,units="mm",res=300,quality=98,type="cairo")
    showtext::showtext_begin()
    print(p)
    showtext::showtext_end()
    dev.off()
  }
}

txt_default <- c(
"**<<name>>**

has participated in the NBIS workshop **Advanced Analysis of Data**
held in **Uppsala** during **11-15 Nov, 2020**.

The workshop consisted of 40 hours of lectures and computer exercises.
This included the following topics:

- Introduction to data & data analysis
- Datatypes and data structures
- Literate programming using data
- Loops, conditional statements, functions and variable scope
- Importing and exporting data
- Visualisation of data
- Introduction to tidy data
- Overview of data package anatomy





Course Leader | **John Doe, PhD**
Associate Professor in Bioinformatics
NBIS | Uppsala University"
)
