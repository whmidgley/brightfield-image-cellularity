if (Sys.info()["user"] == "william.midgley") {
  setwd("~/projects/brightfield-image-cellularity")
} else {
  #setwd("Add working directory here and remove #. Also remove stop("...")")
  stop("Add working directory")
}

rm(list = ls())

if(!is.null(names(sessionInfo()$otherPkgs))) {
	suppressWarnings(
		invisible(
			lapply(
				paste0("package:",
					names(sessionInfo()$otherPkgs)),
					detach,
					character.only = TRUE,
					unload = TRUE
					)
			)
	)
}

pkgs <- c(
	"tidyverse",
	"beepr",
	"EBImage",
	"RBioFormats",
	"readr",
	"stringr",
	"shiny",
	"shinyjs",
	"shinyWidgets",
	"shinyalert"
	)

for (pkg in pkgs) {
	suppressWarnings(
		suppressPackageStartupMessages(
			library(pkg, character.only = TRUE)
			)
		)
}

options(repr.plot.width = 15, repr.plot.height = 20)

# Functions =====================================================================


check.dim <- function(lif){
	twodim <- if(dim(lif)[3] == 2) TRUE else FALSE

	return(twodim)
}
 
extract.image <- function(lif_name) {
	lif <- read.image(lif_name)
	if(is.null(dim(lif))) {
		twodims <- sapply(lif, FUN = check.dim)
		twodim_images <- lif[twodims]
	} else {
		twodim_images <- lif
	}
	return(twodim_images)
}

check.writeable <- function(input_file) {
	if(file.exists(input_file)){
		try_empty <- suppressWarnings(try(read.csv(input_file)))
		if(!(summary(try_empty)[2] == "try-error")) {
			try_cellularities <- read.csv(input_file)
			try_cellularities <- suppressWarnings(try(write.csv(try_cellularities, input_file, row.names = FALSE), silent = TRUE))
			if(!is.null(try_cellularities)) {
				shinyalert(paste0(input_file, " is open"), "please close in order to write over it.\n
					If you want to save the last run, please make a copy by another name", type = "error")
				return(TRUE)
			} else {return(FALSE)}
		} else {return(FALSE)}
	} else {return(FALSE)}
}

check.writeable.grid <- function(input_file) {
	if(file.exists(input_file)){
		try_empty <- suppressWarnings(try(read.csv(input_file, header = FALSE)))
		if(!(summary(try_empty)[2] == "try-error")) {
			try_cellularities <- read.csv(input_file, header = FALSE)
			try_cellularities <- suppressWarnings(try(write.table(try_cellularities, input_file, row.names = FALSE, col.names = FALSE, sep = ","), silent = TRUE))
			if(!is.null(try_cellularities)) {
				shinyalert(paste0(input_file, " is open"), "please close in order to write over it.\n
					If you want to save the last run, please make a copy by another name", type = "error")
				return(TRUE)
			} else {return(FALSE)}
		} else {return(FALSE)}
	} else {return(FALSE)}
}


# Define UI ----
ui <- fluidPage(

	useShinyjs(),

  # App title ----
  titlePanel("Calculate cellularities"),
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    # Sidebar panel for inputs ----
    sidebarPanel(
    	h2("Variables"),

      # Input: Slider for the level of blur applied ----
      fluidRow(
      	column(10, style = "justify-content:center;",
      	sliderInput(inputId = "blur",
                  label = "Level of blur (proportion of image):",
                  min = 0.001,
                  max = 0.005,
                  value = 0.003)
      	),
      	column(1, style = "justify-content:center; padding-top:40px;",
      actionButton("reset_blur", "Reset", style="padding:4px; font-size:90%")
      	)
      ),
      fluidRow(
      column(10, style = "justify-content:center;",
      sliderInput(inputId = "brightness_mean",
                  label = "Brighten the edge detected image so that the mean brightness is this:",
                  min = 0.1,
                  max = 0.5,
                  value = 0.3)
      ),
      column(1, style = "justify-content:center; padding-top:40px;",
      actionButton("reset_bm", "Reset", style = "padding:4px; font-size:90%")
      	)
      ),
      fluidRow(
      column(10, style = "justify-content:center;",
      sliderInput(inputId = "cut_off",
                  label = "Brightness cut-off for acellular vs cellular area:",
                  min = 0.02,
                  max = 0.15,
                  value = 0.08)
      ),
      column(1, style = "justify-content:center; padding-top:40px;",
      actionButton("reset_co", "Reset", style = "padding:4px; font-size:90%")
      	)
      ),
      fluidRow(
      column(10, style = "justify-content:center;",
      sliderInput(inputId = "error_factor",
                  label = "Proportion of perimeter of segmented area by which cellularity is reduced (Increase alongside blur):",
                  min = 0.75,
                  max = 2.50,
                  value = 1.65)
      ),
      column(1, style = "justify-content:center; padding-top:57px;",
      actionButton("reset_ef", "Reset", style = "padding:4px; font-size:90%")
      	)
      ),
      fluidRow(
      column(10, style = "justify-content:center;",
      sliderInput(inputId = "flag_thresh",
                  label = "Threshold for flagging images with segments with lots of edges (%):",
                  min = 0,
                  max = 30,
                  value = 15)
      ),
      column(1, style = "justify-content:center; padding-top:40px;",
      actionButton("reset_ft", "Reset", style = "padding:4px; font-size:90%")
      	)
      ),
      fluidRow(
      column(5,
      numericInput(inputId = "grid_no",
      			  label = "Number each side of image is divided into for grid",
      			  value = 4)
      ),
      column(7, style = "padding-top:50px;",
      prettySwitch(inputId = "grid_output",
      			  label = "Output cellularities by grid?",
      			  value = FALSE
      			  )
      )
      ),
      htmlOutput("prettySwitchUI"),
      selectInput("desired_output_format", "Output format:",
	  	  c("tif", "png", "jpeg"),
	  	  selected = "tif"
	  ),
	  fluidRow(
	  column(1, style = "padding-top:10px;",
      actionButton("reset_all", "Reset all")
      ),
      column(5, offset = 6,
      actionButton("run", "Run algorithm", style = "font-size:200%; background-color:#428bca; color:white;")
      )
      ),
      fluidRow(
      column(1,
      actionButton("see_images", "See images")
      ),
      column(5, offset = 6,
      actionButton("run_selected", "Run algorithm on selected image", style = "background-color:#428bca; color:white;")
      )
      ),
  ),
    # Main panel for displaying outputs ----
    mainPanel(
    fluidRow(
    	span(htmlOutput("name"), style = "font-size:200%;"),
    	column(5, offset = 1,
    	plotOutput("original"),
    	),
    	column(5,
    	plotOutput("normalised")
    	)
    ),
    fluidRow(
    	column(1,
    	htmlOutput("prev_image"),
    	),
    	column(4, offset = 1,
    	textOutput("original_label")
    	),
    	column(4, offset = 1,
    	textOutput("normalised_label")
    	),
    	column(1, style = "padding-right:10px;",
    	htmlOutput("next_image")
    	),
    ),
    fluidRow(
    	column(5, offset = 1,
    	plotOutput("segmented")
    	),
    	column(5,
    	plotOutput("overlay")
    	)
    ),
    fluidRow(
    	column(4, offset = 2,
    	textOutput("segmented_label")
    	),
    	column(4, offset = 1,
    	textOutput("overlay_label")
    	)
    ),
    fluidRow(
    	column(6,
    	htmlOutput("choose_image")
    	),
    	column(5,
    	htmlOutput("show_cellularity")
    	)
    	)
    )
    )
)



# ==========================================================================
# Server
# ==========================================================================


# Define server logic required to draw a histogram ----
server <- function(input, output, session) {

rv <- reactiveValues()
rv$image_no <- 1
rv$done <- 0

  observeEvent(input$reset_blur,{
    updateSliderInput(session,"blur", value = 0.003)
  })
  observeEvent(input$reset_bm,{
    updateSliderInput(session,"brightness_mean", value = 0.3)
  })
  observeEvent(input$reset_co,{
    updateSliderInput(session,"cut_off", value = 0.08)
  })
  observeEvent(input$reset_ef,{
    updateSliderInput(session,"error_factor", value = 1.65)
  })
  observeEvent(input$reset_ft,{
    updateSliderInput(session,"flag_thresh", value = 15)
  })
  observeEvent(input$grid_no %% 1 != 0 | input$grid_no == "e",{
    updateNumericInput(session,"grid_no", value = round(input$grid_no))
  })
  observeEvent(input$grid_output,{
  	if(!input$grid_output) {
  		disable("grid_no")
  	} else {
  		enable("grid_no")
  	}
  	output$prettySwitchUI <- renderUI({
  		if(input$grid_output) {
  		prettySwitch(inputId = "change_grid_no",
      			  label = "Only get new grid cellularities?\n(This will use the last run's variables)",
      			  value = FALSE
      			  )
  	}
  })
  })
  observeEvent(input$reset_all,{
    updateSliderInput(session, "blur", value = 0.003)
    updateSliderInput(session, "brightness_mean", value = 0.3)
    updateSliderInput(session, "cut_off", value = 0.08)
    updateSliderInput(session, "error_factor", value = 1.65)
    updateSliderInput(session,"flag_thresh", value = 15)
    updateNumericInput(session, "grid_no", value = 4)
    updateSwitchInput(session, "grid_output", value = FALSE)
    updateSwitchInput(session, "change_grid_no", value = FALSE)
    updateSelectInput(session, "desired_output_format", selected = "tif")
  })



    output$name <- renderText({
    	if(rv$done > 0) paste0("<B>Image name: </B>", rv$image_names[rv$image_no])
    	})
  	output$original <- renderPlot({
  		if(rv$done > 0) {
  		if(rv$input_format == "lif") {
  		lif_dirs <- list.files(path = "input-images", pattern = "lif$", recursive = TRUE, full.names = TRUE) 

  		for(i in 1:length(lif_dirs)){
			lif <- extract.image(lif_dirs[i])
			if(is.null(dim(lif))) {
				lif_length <- length(lif)
			} else {
				lif_length <- 1
			}
			if(lif_length*i >= rv$image_no) {
				if(lif_length == 1) {
					image_frame <- lif
				} else {
					image_frame <- lif[[rv$image_no - lif_length*(i-1)]]
				}
				image <- array(dim = c(dim(image_frame)[c(1:2)], 3))
				image[,,1] <- image_frame[,,2]
				image[,,2] <- image_frame[,,2]
				image[,,3] <- image_frame[,,2]
				m_bf <- Image(image, colormode = "Color")
			}
		}
		} else {
		rv$images <- list.files(path = "input-images", pattern = rv$input_format, recursive = TRUE, full.names = TRUE) 
		m_bf <- suppressWarnings(readImage(paste0(rv$images[rv$image_no])))
	}
	plot(m_bf)
	}
	})
  	output$normalised <- renderPlot({
  		if(rv$done > 0) {
  			readImage(paste0("normalised-images/", rv$image_names[rv$image_no], " normalised.", input$desired_output_format)) %>%
  			plot()
  		}
  	})
  	output$segmented <- renderPlot({
  		if(rv$done > 0) {
  			readImage(paste0("segmented-images/", rv$image_names[rv$image_no], " segmented.", input$desired_output_format)) %>%
  			plot()
  		}
  	})
  	output$overlay <- renderPlot({
  		if(rv$done > 0) {
  			readImage(paste0("overlay-images/", rv$image_names[rv$image_no], " overlay.", input$desired_output_format)) %>%
  			plot()
  		}
  	})
    output$show_cellularity <- renderText({
    	if(rv$done > 0) {
				if(file.exists("cellularities.csv")) {
					try_empty <- suppressWarnings(try(read.csv("cellularities.csv")))
					if(!(summary(try_empty)[2] == "try-error")) {
						cells <- read.csv("cellularities.csv")
						paste0("<B>Cellularity: </B>", round(cells[rv$image_no, 2], 1), "%")
					}
				}
    	}
    	})

output$original_label <- renderText(if(rv$done > 0) "Original image")
output$normalised_label <- renderText(if(rv$done > 0) "Normalised image")
output$segmented_label <- renderText(if(rv$done > 0) "Segmented image")
output$overlay_label <- renderText(if(rv$done > 0) "Segmented areas overlay original image")

output$prev_image <- renderUI({
  		if(rv$done > 0) actionButton("prevButtonUI", "Previous Image")
})
output$next_image <- renderUI({
  		if(rv$done > 0) actionButton("nextButtonUI", "Next Image")
})

output$choose_image <- renderUI({
  		if(rv$done > 0) {
  		selectInput("chosenImageUI", "Image:",
	  	  rv$image_names,
	  	  selected = (if(rv$done == 1) {
	  	  	rv$image_names[1]
	  	  } else {rv$image_names[rv$image_no]})
	  )
  	}
})


observeEvent(input$prevButtonUI, {
	if(rv$image_no > 1) {
		rv$image_no <- rv$image_no - 1
	} else if(length(rv$image_names) == 1 ){
		rv$image_no <- 1
	} else {
		rv$image_no <- length(rv$image_names)
	}
	updateSelectInput(session, "chosenImageUI", selected = rv$image_names[rv$image_no])
})

observeEvent(input$nextButtonUI, {
	if(rv$image_no < length(rv$image_names)) {
		rv$image_no <- rv$image_no + 1
	} else if(length(rv$image_names) == 1){
		rv$image_no <- 1
	} else {
		rv$image_no <- 1
	}
	updateSelectInput(session, "chosenImageUI", selected = rv$image_names[rv$image_no])
})

observeEvent(input$chosenImageUI, {
	if(rv$done > 0) {
	rv$image_no <- c(1:length(rv$image_names))[input$chosenImageUI == rv$image_names]
}
})

observeEvent(input$see_images, {

auto_lif_detect <- FALSE



# this is only relevant if you're dealing with images and not .lif files
# put TRUE if you're dealing with snapshots from leica
# put FALSE if you're dealing with image formats that aren't taken from leica image snapshots
leica_snapshot_flg <- TRUE

vector.OR <- function(vector) {
	sum(vector) > 0 & length(vector) != 0
}
vector.XOR <- function(vector) {
	sum(vector) == 1 & length(vector) != 0
}
vector.AND <- function(vector) {
	sum(vector) == length(vector)
}

filetype.test <- function(file, extension) {
	str_detect(file, paste0(".", extension, "$")) %>% vector.OR()
}

image_file_contents <- list.files(path = "input-images", recursive = TRUE, full.names = TRUE)

if(auto_lif_detect) {
	# input format can be lif or tif
	input_format <- "lif"
} else {
	image_file_contents <- list.files(path = "input-images", recursive = TRUE, full.names = TRUE)

	compatible_types <- c("lif", "tif", "tiff", "png", "jpeg", "jpg")

	if(length(image_file_contents) == 0) {
		shinyalert("File input-images is empty", "Please add images or lifs", type = "error")
		return(NULL)
	}
	if(!sapply(X = compatible_types, FUN = filetype.test, file = image_file_contents) %>% vector.XOR()) {
		shinyalert("Multiple file formats detected", "Please remove unwanted file format", type = "error")
		return(NULL)
	}
	if(filetype.test(image_file_contents, "tif") && !str_detect(image_file_contents, ".(tiff)|(jpeg)|(jpg)|(png)|(lif)$") %>% vector.AND()) {
		showNotification("Tifs detected. Switching to tif mode\n")
		input_format <- "tif"
	}
	if(filetype.test(image_file_contents, "tiff") && !str_detect(image_file_contents, ".(tif)|(jpeg)|(jpg)|(png)|(lif)$") %>% vector.AND()) {
		showNotification("Tiffs detected. Switching to tiff mode\n")
		input_format <- "tiff"
	}
	if(filetype.test(image_file_contents, "png") && !str_detect(image_file_contents, ".(tif)|(tiff)|(jpeg)|(jpg)|(lif)$") %>% vector.AND()) {
		showNotification("Pngs detected. Switching to png mode\n")
		input_format <- "png"
	}
	if(filetype.test(image_file_contents, "jpeg") && !str_detect(image_file_contents, ".(tif)|(tiff)|(jpg)|(png)|(lif)$") %>% vector.AND()) {
		showNotification("Jpegs detected. Switching to jpeg mode\n")
		input_format <- "jpeg"
	}
	if(filetype.test(image_file_contents, "jpg") && !str_detect(image_file_contents, ".(tif)|(tiff)|(jpeg)|(png)|(lif)$") %>% vector.AND()) {
		showNotification("Jpgs detected. Switching to jpg mode\n")
		input_format <- "jpg"
	}
	if(filetype.test(image_file_contents, "lif") && !str_detect(image_file_contents, ".(tif)|(tiff)|(jpeg)|(jpg)|(png)$") %>% vector.AND()) {
		showNotification("Lifs detected. Switching to lif mode\n")
		input_format <- "lif"
	}
	if(!filetype.test(image_file_contents, "(tif)|(tiff)|(jpeg)|(jpg)|(png)|(lif)")) {
		shinyalert("Files detected are neither lifs nor images. Please add images or lifs",
			"Supported file formats are .lif, .tif, .tiff, .png, and .jpeg", type = "error")
		return(NULL)
	} else if((!filetype.test(image_file_contents, "(tif)|(tiff)|(jpeg)|(jpg)|(png)|(lif)"))) {
		shinyalert("Other files detected which are neither lifs nor images", type = "warning")
	}
}


rv$input_format <- input_format


if(input_format == "lif") {

lif_dirs <- list.files(path = "input-images", pattern = "lif$", recursive = TRUE, full.names = TRUE) 


image_names <- c()
lif_lengths <- c()
for(i in c(1:length(lif_dirs))) {
	lif <- extract.image(lif_dirs[i])
	if(is.null(dim(lif))) {
		lif_length <- length(lif)
	} else {
		lif_length <- 1
	}
	lif_lengths[i] <- lif_length
	for(j in c(1:lif_length)) {
		if(lif_length == 1) {
			image_frame <- lif
		} else {
			image_frame <- lif[[j]]
		}
		image <- array(dim = c(dim(image_frame)[c(1:2)], 3))
		image[,,1] <- image_frame[,,2]
		image[,,2] <- image_frame[,,2]
		image[,,3] <- image_frame[,,2]
		image <- Image(image, colormode = "Color")
		if(i == 1) {
			row_num <- j
		} else {
			row_num <- sum(lif_lengths[1:(i-1)]) + j
		}
		image_names[row_num] <- c(paste0(sub('.+/(.+)', '\\1', lif_dirs[i] %>% str_replace(".lif", "")), " Image ", j))
		assign(paste0("image_", image_names[row_num] %>% str_replace_all(" ", "_")), image)
	}
}
} else {

images <- list.files(path = "input-images", pattern = input_format, recursive = TRUE, full.names = TRUE) 

rv$images <- images

cellularities <- data.frame(matrix(nrow=length(images), ncol=2))
colnames(cellularities) <- c("image_name", "cellularity")

if(leica_snapshot_flg) {
image_names <- sub('.+/(.+)', '\\1', images) %>% str_replace("Effectene.lif_", "") %>% str_replace(paste0("Snapshot1.", input_format), "") %>% str_replace(".lif_", " ")
} else {
	image_names <- sub('.+/(.+)', '\\1', images) %>% str_replace("Effectene.lif_", "")
}
}

rv$image_names <- image_names
rv$done <- rv$done + 1
})




observeEvent(input$run, {
	withProgress(
      message = "Analysing images...", value = 0, {
# ==========================================================================
# Variables
# ==========================================================================

# I recomend between 0.0015 and 0.0035
blur <- input$blur
# I recomend between 0.1 and 0.5
brightness_mean <- input$brightness_mean
# I recomend between 0.05 and 0.1
cut_off <- input$cut_off
# I recomend between 0.75 and 1.75
error_factor <- input$error_factor
# Do you want the cellularity to be outputted as a grid?
grid_output <- input$grid_output
# whatever you want (provided it's below ~600)
grid_no <- input$grid_no
# If you just want to change grid_no on the same variables as a previous run change this to TRUE
# N.B. it will still do a full run if you haven't run it before
change_grid_no <- input$change_grid_no
# I recomend between 10 and 20
flag_thresh <- input$flag_thresh
desired_output_format <- input$desired_output_format


# set defaults if variables are undefined
if(!exists("blur")) blur <- 0.003
if(!exists("brightness_mean")) brightness_mean <- 0.3
if(!exists("cut_off")) cut_off <- 0.08
if(!exists("error_factor")) error_factor <- 1.65
if(!exists("grid_no")) grid_no <- 4
if(!exists("flag_thresh")) flag_thresh <- 15
if(!exists("grid_output")) grid_output <- FALSE
if(!exists("change_grid_no")) change_grid_no <- FALSE
if(!exists("desired_output_format")) desired_output_format <- "tif"


if(is.null(blur)) blur <- 0.003
if(is.null(brightness_mean)) brightness_mean <- 0.3
if(is.null(cut_off)) cut_off <- 0.08
if(is.null(error_factor)) error_factor <- 1.65
if(is.null(grid_no)) grid_no <- 4
if(is.null(flag_thresh)) flag_thresh <- 15
if(is.null(grid_output)) grid_output <- FALSE
if(is.null(change_grid_no)) change_grid_no <- FALSE
if(is.null(desired_output_format)) desired_output_format <- "tif"
if(!grid_output & change_grid_no) shinyalert("grid_output is FALSE but change_grid_no is TRUE.", "Ignoring change_grid_no and doing a full run", type = "warning")

save(blur, file = "blur.rdata")
save(brightness_mean, file = "brightness_mean.rdata")
save(cut_off, file = "cut_off.rdata")
save(error_factor, file = "error_factor.rdata")
save(grid_output, file = "grid_output.rdata")
save(grid_no, file = "grid_no.rdata")
save(change_grid_no, file = "change_grid_no.rdata")
save(flag_thresh, file = "flag_thresh.rdata")
save(desired_output_format, file = "desired_output_format.rdata")

cat("blur is ", blur, "\n")
cat("brightness_mean is ", brightness_mean, "\n")
cat("error_factor is ", error_factor, "\n")
cat("grid_output is ", grid_output, "\n")
cat("grid_no is ", grid_no, "\n")
cat("change_grid_no is ", change_grid_no, "\n")
cat("flag_thresh is ", flag_thresh, "\n")
cat("desired_output_format is ", desired_output_format, "\n")

# ==========================================================================
# Check folders exist
# ==========================================================================


if(!dir.exists("input-images")) dir.create("input-images")
if(!dir.exists("grid-cellularities") & grid_output) dir.create("grid-cellularities")
if(!dir.exists("normalised-images")) dir.create("normalised-images")
if(!dir.exists("overlay-images")) dir.create("overlay-images")
if(!dir.exists("segmented-images")) dir.create("segmented-images")


# ==========================================================================
# File formats
# ==========================================================================


# enter your desired output image format. Supported formats are tif, tiff, png and jpeg
if(!desired_output_format %in% c("tif", "tiff", "png", "jpeg", "jpg")) stop("Output format not supported.\nSupported formats are tif, tiff, png and jpeg")

# Change to TRUE if you want to manually chage the input format
auto_lif_detect <- FALSE



# this is only relevant if you're dealing with images and not .lif files
# put TRUE if you're dealing with snapshots from leica
# put FALSE if you're dealing with image formats that aren't taken from leica image snapshots
leica_snapshot_flg <- TRUE

vector.OR <- function(vector) {
	sum(vector) > 0 & length(vector) != 0
}
vector.XOR <- function(vector) {
	sum(vector) == 1 & length(vector) != 0
}
vector.AND <- function(vector) {
	sum(vector) == length(vector)
}

filetype.test <- function(file, extension) {
	str_detect(file, paste0(".", extension, "$")) %>% vector.OR()
}

image_file_contents <- list.files(path = "input-images", recursive = TRUE, full.names = TRUE)

if(auto_lif_detect) {
	# input format can be lif or tif
	input_format <- "lif"
} else {
	image_file_contents <- list.files(path = "input-images", recursive = TRUE, full.names = TRUE)

	compatible_types <- c("lif", "tif", "tiff", "png", "jpeg", "jpg")

	if(length(image_file_contents) == 0) {
		shinyalert("File input-images is empty", "Please add images or lifs", type = "error")
		return(NULL)
	}
	if(!sapply(X = compatible_types, FUN = filetype.test, file = image_file_contents) %>% vector.XOR()) {
		shinyalert("Multiple file formats detected", "Please remove unwanted file format", type = "error")
		return(NULL)
	}
	if(filetype.test(image_file_contents, "tif") && !str_detect(image_file_contents, ".(tiff)|(jpeg)|(jpg)|(png)|(lif)$") %>% vector.AND()) {
		showNotification("Tifs detected. Switching to tif mode\n")
		input_format <- "tif"
	}
	if(filetype.test(image_file_contents, "tiff") && !str_detect(image_file_contents, ".(tif)|(jpeg)|(jpg)|(png)|(lif)$") %>% vector.AND()) {
		showNotification("Tiffs detected. Switching to tiff mode\n")
		input_format <- "tiff"
	}
	if(filetype.test(image_file_contents, "png") && !str_detect(image_file_contents, ".(tif)|(tiff)|(jpeg)|(jpg)|(lif)$") %>% vector.AND()) {
		showNotification("Pngs detected. Switching to png mode\n")
		input_format <- "png"
	}
	if(filetype.test(image_file_contents, "jpeg") && !str_detect(image_file_contents, ".(tif)|(tiff)|(jpg)|(png)|(lif)$") %>% vector.AND()) {
		showNotification("Jpegs detected. Switching to jpeg mode\n")
		input_format <- "jpeg"
	}
	if(filetype.test(image_file_contents, "jpg") && !str_detect(image_file_contents, ".(tif)|(tiff)|(jpeg)|(png)|(lif)$") %>% vector.AND()) {
		showNotification("Jpgs detected. Switching to jpg mode\n")
		input_format <- "jpg"
	}
	if(filetype.test(image_file_contents, "lif") && !str_detect(image_file_contents, ".(tif)|(tiff)|(jpeg)|(jpg)|(png)$") %>% vector.AND()) {
		showNotification("Lifs detected. Switching to lif mode\n")
		input_format <- "lif"
	}
	if(!filetype.test(image_file_contents, "(tif)|(tiff)|(jpeg)|(jpg)|(png)|(lif)")) {
		shinyalert("Files detected are neither lifs nor images. Please add images or lifs",
			"Supported file formats are .lif, .tif, .tiff, .png, and .jpeg", type = "error")
		return(NULL)
	} else if((!filetype.test(image_file_contents, "(tif)|(tiff)|(jpeg)|(jpg)|(png)|(lif)"))) {
		shinyalert("Other files detected which are neither lifs nor images", type = "warning")
	}
}

rv$input_format <- input_format

# ==========================================================================
# Load images
# ==========================================================================

if(input_format == "lif") {

lif_dirs <- list.files(path = "input-images", pattern = "lif$", recursive = TRUE, full.names = TRUE) 

image_names <- c()
lif_lengths <- c()
for(i in c(1:length(lif_dirs))) {
	lif <- extract.image(lif_dirs[i])
	if(is.null(dim(lif))) {
		lif_length <- length(lif)
	} else {
		lif_length <- 1
	}
	lif_lengths[i] <- lif_length
	for(j in c(1:lif_length)) {
		if(lif_length == 1) {
			image_frame <- lif
		} else {
			image_frame <- lif[[j]]
		}
		image <- array(dim = c(dim(image_frame)[c(1:2)], 3))
		image[,,1] <- image_frame[,,2]
		image[,,2] <- image_frame[,,2]
		image[,,3] <- image_frame[,,2]
		image <- Image(image, colormode = "Color")
		if(i == 1) {
			row_num <- j
		} else {
			row_num <- sum(lif_lengths[1:(i-1)]) + j
		}
		image_names[row_num] <- c(paste0(sub('.+/(.+)', '\\1', lif_dirs[i] %>% str_replace(".lif", "")), " Image ", j))
		assign(paste0("image_", image_names[row_num] %>% str_replace_all(" ", "_")), image)
	}
}
} else {

images <- list.files(path = "input-images", pattern = input_format, recursive = TRUE, full.names = TRUE) 

rv$images <- images

cellularities <- data.frame(matrix(nrow=length(images), ncol=2))
colnames(cellularities) <- c("image_name", "cellularity")

if(leica_snapshot_flg) {
image_names <- sub('.+/(.+)', '\\1', images) %>% str_replace("Effectene.lif_", "") %>% str_replace(paste0("Snapshot1.", input_format), "") %>% str_replace(".lif_", " ")
} else {
	image_names <- sub('.+/(.+)', '\\1', images) %>% str_replace("Effectene.lif_", "")
}
}

rv$image_names <- image_names

# ==========================================================================
# Check output file isn't open
# ==========================================================================

unwriteable <- check.writeable("cellularities.csv")
if(unwriteable) return(NULL)
rm(unwriteable)

if(grid_output) {
	unwriteable <- invisible(sapply(paste0("grid-cellularities/", image_names, " ", grid_no, "x", grid_no, " grid.csv"), FUN = check.writeable.grid))
	if(unwriteable %>% vector.OR()) return(NULL)
	rm(unwriteable)
}

auto_cellularities <- data.frame(matrix(ncol = 3, nrow = length(image_names)))
colnames(auto_cellularities) <- c("name", "cellularity", "high_compensation_flag")

save(image_names, file = "image_names.rdata")

for (j in 1:length(image_names)) {
incProgress(1/length(image_names), message = paste0(image_names[j]))
	cat("Image",j,"=================\n")

 output$image_name <- renderText({ 
   paste0(image_names[j])
 })

	if(input_format == "lif") {
		m_bf <- suppressWarnings(get(paste0("image_", image_names[j] %>% str_replace_all(" ", "_"))))
		} else {
		m_bf <- suppressWarnings(readImage(paste0(images[j])))
	}
	if(length(dim(m_bf)) == 2) {
		make_colour <- array(dim = c(dim(m_bf), "3"))
		make_colour[,,1] <- m_bf
		make_colour[,,2] <- m_bf
		make_colour[,,3] <- m_bf
		m_bf <- make_colour
	}

	save(m_bf, file = "m_bf.rdata")
	save(j, file = "j.rdata")

if(!(change_grid_no & grid_output)) {
if(file.exists(paste0("normalised-images/", image_names[j], " normalised.", desired_output_format))) {
	pre_normalised <- readImage(paste0("normalised-images/", image_names[j], " normalised.", desired_output_format))
	if(!((dim(pre_normalised) == dim(m_bf)) %>% vector.AND())) source("01a_remove_gradient.r")
	rm("pre_normalised")
} else {source("01a_remove_gradient.r")}

source("01b_detect_edges.r")
source("01c_cut_off.r")
}
if(grid_output) {
source("01d_by_grid.r")
}
if(!(change_grid_no & grid_output)) {
auto_cellularities[j,] <- c(image_names[j], print(computer_cellularity), case_when((1-prop_background_edge)*error_factor*100 > flag_thresh ~ "CHECK OUTLINE",
																									TRUE ~ "image normal"))
}
file.remove("m_bf.rdata")
file.remove("j.rdata")
}

write.csv(auto_cellularities, "cellularities.csv", row.names = FALSE)

file.remove("image_names.rdata")
file.remove("blur.rdata")
file.remove("brightness_mean.rdata")
file.remove("cut_off.rdata")
file.remove("error_factor.rdata")
file.remove("grid_output.rdata")
file.remove("grid_no.rdata")
file.remove("change_grid_no.rdata")
file.remove("flag_thresh.rdata")
file.remove("desired_output_format.rdata")
beep() 

 output$done <- renderText({ 
   paste0("Done!")
 })
 rv$done <- rv$done + 1 
})
})


observeEvent(input$run_selected, {
	withProgress(
      message = "Analysing image...", value = 0, {
# ==========================================================================
# Variables
# ==========================================================================

# I recomend between 0.0015 and 0.0035
blur <- input$blur
# I recomend between 0.1 and 0.5
brightness_mean <- input$brightness_mean
# I recomend between 0.05 and 0.1
cut_off <- input$cut_off
# I recomend between 0.75 and 1.75
error_factor <- input$error_factor
# Do you want the cellularity to be outputted as a grid?
grid_output <- input$grid_output
# whatever you want (provided it's below ~600)
grid_no <- input$grid_no
# If you just want to change grid_no on the same variables as a previous run change this to TRUE
# N.B. it will still do a full run if you haven't run it before
change_grid_no <- input$change_grid_no
# I recomend between 10 and 20
flag_thresh <- input$flag_thresh
desired_output_format <- input$desired_output_format


# set defaults if variables are undefined
if(!exists("blur")) blur <- 0.003
if(!exists("brightness_mean")) brightness_mean <- 0.3
if(!exists("cut_off")) cut_off <- 0.08
if(!exists("error_factor")) error_factor <- 1.65
if(!exists("grid_no")) grid_no <- 4
if(!exists("flag_thresh")) flag_thresh <- 15
if(!exists("grid_output")) grid_output <- FALSE
if(!exists("change_grid_no")) change_grid_no <- FALSE
if(!exists("desired_output_format")) desired_output_format <- "tif"


if(is.null(blur)) blur <- 0.003
if(is.null(brightness_mean)) brightness_mean <- 0.3
if(is.null(cut_off)) cut_off <- 0.08
if(is.null(error_factor)) error_factor <- 1.65
if(is.null(grid_no)) grid_no <- 4
if(is.null(flag_thresh)) flag_thresh <- 15
if(is.null(grid_output)) grid_output <- FALSE
if(is.null(change_grid_no)) change_grid_no <- FALSE
if(is.null(desired_output_format)) desired_output_format <- "tif"
if(!grid_output & change_grid_no) shinyalert("grid_output is FALSE but change_grid_no is TRUE.", "Ignoring change_grid_no and doing a full run", type = "warning")

save(blur, file = "blur.rdata")
save(brightness_mean, file = "brightness_mean.rdata")
save(cut_off, file = "cut_off.rdata")
save(error_factor, file = "error_factor.rdata")
save(grid_output, file = "grid_output.rdata")
save(grid_no, file = "grid_no.rdata")
save(change_grid_no, file = "change_grid_no.rdata")
save(flag_thresh, file = "flag_thresh.rdata")
save(desired_output_format, file = "desired_output_format.rdata")

cat("blur is ", blur, "\n")
cat("brightness_mean is ", brightness_mean, "\n")
cat("error_factor is ", error_factor, "\n")
cat("grid_output is ", grid_output, "\n")
cat("grid_no is ", grid_no, "\n")
cat("change_grid_no is ", change_grid_no, "\n")
cat("flag_thresh is ", flag_thresh, "\n")
cat("desired_output_format is ", desired_output_format, "\n")

# ==========================================================================
# Check folders exist
# ==========================================================================


if(!dir.exists("input-images")) dir.create("input-images")
if(!dir.exists("grid-cellularities") & grid_output) dir.create("grid-cellularities")
if(!dir.exists("normalised-images")) dir.create("normalised-images")
if(!dir.exists("overlay-images")) dir.create("overlay-images")
if(!dir.exists("segmented-images")) dir.create("segmented-images")


# ==========================================================================
# File formats
# ==========================================================================


# enter your desired output image format. Supported formats are tif, tiff, png and jpeg
if(!desired_output_format %in% c("tif", "tiff", "png", "jpeg", "jpg")) stop("Output format not supported.\nSupported formats are tif, tiff, png and jpeg")

# Change to TRUE if you want to manually chage the input format
auto_lif_detect <- FALSE



# this is only relevant if you're dealing with images and not .lif files
# put TRUE if you're dealing with snapshots from leica
# put FALSE if you're dealing with image formats that aren't taken from leica image snapshots
leica_snapshot_flg <- TRUE

vector.OR <- function(vector) {
	sum(vector) > 0 & length(vector) != 0
}
vector.XOR <- function(vector) {
	sum(vector) == 1 & length(vector) != 0
}
vector.AND <- function(vector) {
	sum(vector) == length(vector)
}

filetype.test <- function(file, extension) {
	str_detect(file, paste0(".", extension, "$")) %>% vector.OR()
}

image_file_contents <- list.files(path = "input-images", recursive = TRUE, full.names = TRUE)

if(length(image_file_contents) == 0) {
	shinyalert("File input-images is empty", "Please add images or lifs", type = "error")
	return(NULL)
}
if(str_detect(image_file_contents, ".tif$") %>% vector.OR() && !str_detect(image_file_contents, ".(tiff)|(jpeg)|(jpg)|(png)|(lif)$") %>% vector.AND()) {
	showNotification("Tifs detected. Switching to tif mode\n")
	input_format <- "tif"
}
if(str_detect(image_file_contents, ".tiff$") %>% vector.OR() && !str_detect(image_file_contents, ".(tif)|(jpeg)|(jpg)|(png)|(lif)$") %>% vector.AND()) {
	showNotification("Tiffs detected. Switching to tiff mode\n")
	input_format <- "tiff"
}
if(str_detect(image_file_contents, ".png$") %>% vector.OR() && !str_detect(image_file_contents, ".(tif)|(tiff)|(jpeg)|(jpg)|(lif)$") %>% vector.AND()) {
	showNotification("Pngs detected. Switching to png mode\n")
	input_format <- "png"
}
if(str_detect(image_file_contents, ".jpeg$") %>% vector.OR() && !str_detect(image_file_contents, ".(tif)|(tiff)|(png)|(jpg)|(lif)$") %>% vector.AND()) {
	showNotification("Jpegs detected. Switching to jpeg mode\n")
	input_format <- "jpeg"
}
if(str_detect(image_file_contents, ".jpg$") %>% vector.OR() && !str_detect(image_file_contents, ".(tif)|(tiff)|(png)|(jpeg)|(lif)$") %>% vector.AND()) {
	showNotification("Jpgs detected. Switching to jpg mode\n")
	input_format <- "jpg"
}
if(str_detect(image_file_contents, ".lif$") %>% vector.OR() && !str_detect(image_file_contents, ".(tif)|(tiff)|(jpeg)|(jpg)|(png)$") %>% vector.AND()) {
	showNotification("Lifs detected. Switching to lif mode\n")
	input_format <- "lif"
}
if(str_detect(image_file_contents, ".lif$") %>% vector.OR() && str_detect(image_file_contents, ".(tif)|(tiff)|(jpeg)|(jpg)|(png)$") %>% vector.OR()) {
	shinyalert("Multiple file formats detected", "Please remove unwanted file format", type = "error")
	return(NULL)
}
if(!str_detect(image_file_contents, ".(tif)|(tiff)|(jpg)|(jpeg)|(png)|(lif)$") %>% vector.OR()) {
	shinyalert("Files detected are neither lifs nor images. Please add images or lifs",
		"Supported file formats are .lif, .tif, .tiff, .png, and .jpeg", type = "error")
	return(NULL)
} else if((!str_detect(image_file_contents, ".(tif)|(tiff)|(jpg)|(jpeg)|(png)|(lif)$")) %>% vector.OR()) {
	shinyalert("Other files detected which are neither lifs nor images", type = "warning")
}

rv$input_format <- input_format

# ==========================================================================
# Load images
# ==========================================================================

if(input_format == "lif") {

lif_dirs <- list.files(path = "input-images", pattern = "lif$", recursive = TRUE, full.names = TRUE) 


image_names <- c()
lif_lengths <- c()
for(i in c(1:length(lif_dirs))) {
	lif <- extract.image(lif_dirs[i])
	if(is.null(dim(lif))) {
		lif_length <- length(lif)
	} else {
		lif_length <- 1
	}
	lif_lengths[i] <- lif_length
	for(j in c(1:lif_length)) {
		if(lif_length == 1) {
			image_frame <- lif
		} else {
			image_frame <- lif[[j]]
		}
		image <- array(dim = c(dim(image_frame)[c(1:2)], 3))
		image[,,1] <- image_frame[,,2]
		image[,,2] <- image_frame[,,2]
		image[,,3] <- image_frame[,,2]
		image <- Image(image, colormode = "Color")
		if(i == 1) {
			row_num <- j
		} else {
			row_num <- sum(lif_lengths[1:(i-1)]) + j
		}
		image_names[row_num] <- c(paste0(sub('.+/(.+)', '\\1', lif_dirs[i] %>% str_replace(".lif", "")), " Image ", j))
		assign(paste0("image_", image_names[row_num] %>% str_replace_all(" ", "_")), image)
	}
}
} else {

images <- list.files(path = "input-images", pattern = input_format, recursive = TRUE, full.names = TRUE) 

rv$images <- images

cellularities <- data.frame(matrix(nrow=length(images), ncol=2))
colnames(cellularities) <- c("image_name", "cellularity")

if(leica_snapshot_flg) {
	image_names <- sub('.+/(.+)', '\\1', images) %>% str_replace("Effectene.lif_", "") %>% str_replace(paste0("Snapshot1.", input_format), "") %>% str_replace(".lif_", " ")
} else {
	image_names <- sub('.+/(.+)', '\\1', images) %>% str_replace("Effectene.lif_", "")
}
}

rv$image_names <- image_names

# ==========================================================================
# Check output file isn't open
# ==========================================================================


unwriteable <- check.writeable("cellularities.csv")
if(unwriteable) return(NULL)
rm(unwriteable)

if(grid_output) invisible(sapply(paste0("grid-cellularities/", image_names, " ", grid_no, "x", grid_no, " grid.csv"), FUN = check.writeable.grid))


if(file.exists("cellularities.csv")) {
	auto_cellularities <- read.csv("cellularities.csv")
	} else {
	auto_cellularities <- data.frame(matrix(ncol = 3, nrow = length(image_names)))
}
colnames(auto_cellularities) <- c("name", "cellularity", "high_compensation_flag")

save(image_names, file = "image_names.rdata")


j <- rv$image_no

	cat("Image",j,"=================\n")

 output$image_name <- renderText({ 
   paste0(image_names[j])
 })

	if(input_format == "lif") {
		m_bf <- suppressWarnings(get(paste0("image_", image_names[j] %>% str_replace_all(" ", "_"))))
		} else {
		m_bf <- suppressWarnings(readImage(paste0(images[j])))
	}

	if(length(dim(m_bf)) == 2) {
		make_colour <- array(dim = c(dim(m_bf), "3"))
		make_colour[,,1] <- m_bf
		make_colour[,,2] <- m_bf
		make_colour[,,3] <- m_bf
		m_bf <- make_colour
	}

	save(m_bf, file = "m_bf.rdata")
	save(j, file = "j.rdata")

if(!(change_grid_no & grid_output)) {
if(file.exists(paste0("normalised-images/", image_names[j], " normalised.", desired_output_format))) {
	pre_normalised <- readImage(paste0("normalised-images/", image_names[j], " normalised.", desired_output_format))
	if(!((dim(pre_normalised) == dim(m_bf)) %>% vector.AND())) source("01a_remove_gradient.r")
	rm("pre_normalised")
} else {source("01a_remove_gradient.r")}

source("01b_detect_edges.r")
source("01c_cut_off.r")
}
if(grid_output) {
source("01d_by_grid.r")
}
if(!(change_grid_no & grid_output)) {
auto_cellularities[j,] <- c(image_names[j], print(computer_cellularity), case_when((1-prop_background_edge)*error_factor*100 > flag_thresh ~ "CHECK OUTLINE",
																									TRUE ~ "image normal"))
}
file.remove("m_bf.rdata")
file.remove("j.rdata")

write.csv(auto_cellularities, "cellularities.csv", row.names = FALSE)

file.remove("image_names.rdata")
file.remove("blur.rdata")
file.remove("brightness_mean.rdata")
file.remove("cut_off.rdata")
file.remove("error_factor.rdata")
file.remove("grid_output.rdata")
file.remove("grid_no.rdata")
file.remove("change_grid_no.rdata")
file.remove("flag_thresh.rdata")
file.remove("desired_output_format.rdata")
beep() 

 rv$done <- rv$done + 1 
})
})
}

shinyApp(ui = ui, server = server)