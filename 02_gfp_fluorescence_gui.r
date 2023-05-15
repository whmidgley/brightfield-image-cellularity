
source("scripts/r_clear_and_load.r")
library(shiny)
library(shinyjs)
library(shinyWidgets)
library(shinyalert)

# Define UI ----
ui <- fluidPage(

	useShinyjs(),

  # App title ----
  titlePanel("Calculate GFP stats"),
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    # Sidebar panel for inputs ----
    sidebarPanel(
    	style = "height: 90vh; overflow-y: auto;",
    	h2("Variables"),

      # Inputs ----
      # Brighten ----
      fluidRow(
      column(10, style = "justify-content:center;",
      numericInput(inputId = "brighten",
                  label = "Value to saturate to to brighten (0-1):",
                  value = 0.04,
                  step = 0.01)
      ),
      column(1, style = "justify-content:center;",
      actionButton("reset_brighten", "Reset", style = "padding:4px; font-size:90%")
      	)
      ),
      # Blur ----
      fluidRow(
      	column(10, style = "justify-content:center;",
      	sliderInput(inputId = "blur",
                  label = "Level of blur:",
                  min = 0,
                  max = 5,
                  value = 1,
                  step = 0.1
                  )
      	),
      	column(1, style = "justify-content:center; padding-top:40px;",
      actionButton("reset_blur", "Reset", style="padding:4px; font-size:90%")
      	)
      ),
      # Cutoff ----
      fluidRow(
      column(10, style = "justify-content:center;",
      sliderInput(inputId = "cut_off",
                  label = "Brightness cut-off for fluorescence:",
                  min = 0,
                  max = 1,
                  value = 0.45)
      ),
      column(1, style = "justify-content:center; padding-top:40px;",
      actionButton("reset_co", "Reset", style = "padding:4px; font-size:90%")
      	)
      ),
      # Min cell size ----
      fluidRow(
      column(10, style = "justify-content:center;",
      sliderInput(inputId = "min_cell_size",
                  label = "Minimum size of cell detected:",
                  min = 0,
                  max = 1,
                  value = 0.13)
      ),
      column(1, style = "justify-content:center; padding-top:57px;",
      actionButton("reset_min_cell_size", "Reset", style = "padding:4px; font-size:90%")
      	)
      ),
      fluidRow(
      column(10, style = "justify-content:center;",
      prettySwitch(inputId = "cell_output",
                  label = "Give results on a cellular level?",
                  value = TRUE)
      ),
      column(1, style = "justify-content:center; padding-top:57px;",
      actionButton("reset_cell_output", "Reset", style = "padding:4px; font-size:90%")
      	)
      ),
      # Eccentricity stuff ----
      fluidRow(
      column(5,
      	sliderInput(inputId = "min_cell_size_eccen",
      					  label = "Max size this is applied to",
      					  min = 0,
      					  max = 5,
      					  value = 0.3,
      					  step = 0.1)
      	),
			column(7, style = "padding-top:40px;",
      prettySwitch(inputId = "remove_if_eccentric",
      			  label = "Remove fluorescing areas if they're eccentric?",
      			  value = TRUE
      			  )
      	)
				),
      	fluidRow(
      	column(10,
      		sliderInput(inputId = "max_eccen",
      				  label = "Max eccentricity",
      				  min = 0.5,
      				  max = 1,
      				  value = 0.85)
      	),
      column(1, style = "justify-content:center; padding-top:40px;",
      actionButton("reset_eccen", "Reset", style = "padding:4px; font-size:90%")
      	)      
      ),
      # Grid ----
      fluidRow(
      column(5,
      numericInput(inputId = "grid_no",
      			  label = "Number each side of image is divided into for grid",
      			  value = 4)
      ),
      column(7, style = "padding-top:50px;",
      prettySwitch(inputId = "grid_output",
      			  label = "Output by grid?",
      			  value = FALSE
      			  )
      )
      ),
      # Output format ----
      htmlOutput("prettySwitchUI"),
      selectInput("desired_output_format", "Output format:",
	  	  c("tif", "png", "jpeg"),
	  	  selected = "tif"
	  ),
      # Stuff at bottom ----
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
# Main panel ====================================================================
    mainPanel(
    	# Tabs ----
    tabsetPanel(
    	type = "tabs",
    	tabPanel("Overview",
    	style = "height: 90vh; overflow-y: auto;",
    	# Name ----
    fluidRow(
    	span(htmlOutput("name"), style = "font-size:200%;"),
    	column(5, offset = 1,
    	plotOutput("bf"),
    	),
    	column(5,
    	plotOutput("gfp")
    	)
    ),
    # Choose image buttons ----
    fluidRow(
    	column(1,
    	htmlOutput("prev_image"),
    	),
    	column(4, offset = 1,
    	textOutput("bf_label")
    	),
    	column(4, offset = 1,
    	textOutput("gfp_label")
    	),
    	column(1, style = "padding-right:10px;",
    	htmlOutput("next_image")
    	),
    ),
    fluidRow(
    	column(5, offset = 1,
    	plotOutput("brightened")
    	),
    	column(5,
    	plotOutput("overlay")
    	)
    ),
    fluidRow(
    	column(4, offset = 2,
    	textOutput("brightened_label")
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
    ),
    tabPanel("Display Overlay",
    	style = "height: 90vh; overflow-y: auto;",
    	fillPage(displayOutput("overlay_display", height = "800px"))
    	),
    tabPanel("GFP Stats",
    fluidRow(
    	column(4, style = "padding-top:5px;",
    	htmlOutput("show_perc_fluro")
    	),
    	column(4, style = "padding-top:5px;",
    	htmlOutput("show_mean_brightness")
    	),
    	column(4, style = "padding-top:5px;",
    	htmlOutput("show_sd_brightness")
    	)   	
    	),
    fluidRow(
    	plotOutput("overall_dist")
    	),
    fluidRow(
    	column(6,
    	plotOutput("grid_overlay")
    	),
    	column(6,
    	plotOutput("grid_dist")
    	)
    	)
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
rv$load_images <- 0
rv$start_run <- 0
rv$start_run_selected <- 0

  observeEvent(input$reset_brighten,{
    updateSliderInput(session,"brighten", value = 0.04)
  })
  observeEvent(input$reset_blur,{
    updateSliderInput(session,"blur", value = 1)
  })
  observeEvent(input$reset_co,{
    updateSliderInput(session,"cut_off", value = 0.45)
  })
  observeEvent(input$reset_min_cell_size,{
    updateSliderInput(session,"min_cell_size", value = 0.13)
  })
   observeEvent(input$reset_cell_output,{
    updatePrettySwitch(session,"cell_output", value = TRUE)
  })
  observeEvent(input$reset_eccen,{
    updateSliderInput(session,"min_cell_size_eccen", value = 0.3)
    updateSliderInput(session,"max_eccen", value = 0.85)
    updatePrettySwitch(session,"remove_if_eccentric", value = TRUE)
  })
  observeEvent(input$remove_if_eccentric,{
    if(!input$remove_if_eccentric) {
  		disable("min_cell_size_eccen")
  		disable("max_eccen")
  	} else {
  		enable("min_cell_size_eccen")
  		enable("max_eccen")
  	}
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
		updateSliderInput(session,"brighten", value = 0.04)
		updateSliderInput(session,"blur", value = 1)
		updateSliderInput(session,"cut_off", value = 0.45)
		updateSliderInput(session,"min_cell_size", value = 0.13)
		updatePrettySwitch(session,"cell_output", value = TRUE)
    updateSliderInput(session,"min_cell_size_eccen", value = 0.3)
    updateSliderInput(session,"max_eccen", value = 0.85)
    updatePrettySwitch(session,"remove_if_eccentric", value = TRUE)
    updateNumericInput(session, "grid_no", value = 4)
    updateSwitchInput(session, "grid_output", value = FALSE)
    updateSwitchInput(session, "change_grid_no", value = FALSE)
    updateSelectInput(session, "desired_output_format", selected = "tif")
  })

# Load images ===================================================================

observeEvent(rv$load_images, {
if(rv$load_images > 0) {
	print("Loading images...")
	# Create folder ----
	if(!dir.exists("input")) dir.create("input")
	# Detect input format ----
	image_file_contents <- list.files(path = "input", recursive = TRUE, full.names = TRUE)

	compatible_types <- c("lif", "tif", "tiff", "png", "jpeg", "jpg")

	if(length(image_file_contents) == 0) {
		shinyalert("File input is empty", "Please add images or lifs", type = "error")
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

	# Load ----
	showNotification("Loading images...")
	if(input_format == "lif") {

		lif_dirs <- list.files(path = "input", pattern = "lif$", recursive = TRUE, full.names = TRUE) 
		
		
		gfp_names <- c()
		bf_names <- c()
		lif_lengths <- c()
		for(i in c(1:length(lif_dirs))) {
			lif <- extract.bf(lif_dirs[i])
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
				if(i == 1) {
					row_num <- j
				} else {
					row_num <- sum(lif_lengths[1:(i-1)]) + j
				}
				bf  <- array(dim = c(dim(image_frame)[c(1:2)], 3))
				gfp <- array(dim = c(dim(image_frame)[c(1:2)], 3))
				bf[,,1] <- image_frame[,,2]
				bf[,,2] <- image_frame[,,2]
				bf[,,3] <- image_frame[,,2]
				gfp[,,1] <- image_frame[,,1]
				gfp[,,2] <- image_frame[,,1]
				gfp[,,3] <- image_frame[,,1]
				bf  <- Image(bf,  colormode = "Color")
				gfp <- Image(gfp, colormode = "Color")
				bf_names[row_num]  <- c(paste0(sub('.+/(.+)', '\\1', lif_dirs[i] %>% str_replace(".lif", "")), " BF ",  j))
				gfp_names[row_num] <- c(paste0(sub('.+/(.+)', '\\1', lif_dirs[i] %>% str_replace(".lif", "")), " GFP ", j))
				rv[[paste0("image_", bf_names[row_num]  %>% str_replace_all(" ", "_"))]] <- bf
				rv[[paste0("image_", gfp_names[row_num] %>% str_replace_all(" ", "_"))]] <- gfp
			}
		}
		rv$bf_names <- bf_names
		rv$gfp_names <- gfp_names
		rv$image_names <- bf_names %>% str_replace("BF", "Image")
	} else {
		images <- list.files(path = "input", pattern = input_format, recursive = TRUE, full.names = TRUE) 
		
		cellularities <- data.frame(matrix(nrow=length(images), ncol=2))
		colnames(cellularities) <- c("image_name", "cellularity")
		
		if(leica_snapshot_flg) {
			image_names <- sub('.+/(.+)', '\\1', images) %>% str_replace("Effectene.lif_", "") %>% str_replace(paste0("Snapshot1.", input_format), "") %>% str_replace(".lif_", " ")
		} else {
			image_names <- sub('.+/(.+)', '\\1', images) %>% str_replace("Effectene.lif_", "")
		}
		
		assign(paste0(input_image_type, "s"), images)
		assign(paste0(input_image_type, "_names"), image_names)
	}
	for(i in rv$bf_names) {
	if(paste0(i, " segmented.", input$desired_output_format) %in% list.files("output/bf-analysis/bf-segmented")) {
		rv[[paste0("image_", i %>% str_replace_all("Image", "BF") %>% str_replace_all(" ", "_"), "_segmented")]] <-
			readImage(paste0("output/bf-analysis/bf-segmented/", i, " segmented.", input$desired_output_format))
	} else {
		bf_copy <- i
		i <- i %>% str_replace_all("BF", "Image")
		if(paste0(i, " segmented.", input$desired_output_format) %in% list.files("output/bf-analysis/bf-segmented")) {
			rv[[paste0("image_", i %>% str_replace_all("Image", "BF") %>% str_replace_all(" ", "_"), "_segmented")]] <-
			readImage(paste0("output/bf-analysis/bf-segmented/", i, " segmented.", input$desired_output_format))
		} else {
			shinyalert("Cellular area has not been determined",
			"Please run cellularity script on these images", type = "error")
			return(NULL)
		}
		i <- bf_copy
	}
	}
	showNotification("Images loaded")
	print("images loaded")
}
})

# ===============================================================================





# Show outputs ==================================================================

# Name
output$name <- renderText({
	if(rv$done > 0) paste0("<B>Image name: </B>", rv$image_names[rv$image_no])
})
# BF image
output$bf <- renderPlot({
	if(rv$done > 0) {
		if(rv$load_images > 0) {
			print("plotting bf...")
			plot(rv[[paste0("image_", rv$bf_names[rv$image_no]  %>% str_replace_all(" ", "_"))]])
		} else {
			rv$load_images <- rv$load_images + 1
		}
	}
})
# GFP image
output$gfp <- renderPlot({
	if(rv$done > 0 & rv$load_images > 0) {
		print("plotting gfp...")
		rv$gfp <- rv[[paste0("image_", rv$gfp_names[rv$image_no]  %>% str_replace_all(" ", "_"))]]
		plot(rv$gfp)
	}
})
# Brightened image
output$brightened <- renderPlot({
	if(rv$done > 0 & rv$load_images > 0) {
		print("plotting brightened...")
		gfp_cut_top <- rv[[paste0("image_", rv$gfp_names[rv$image_no]  %>% str_replace_all(" ", "_"))]]
		gfp_cut_top[gfp_cut_top > input$brighten] <- input$brighten
		rv$gfp_cut_top <- gfp_cut_top*(1/input$brighten)
		plot(rv$gfp_cut_top)
	}
})

output$overlay <- renderPlot({
	if(rv$done > 0 & rv$load_images > 0) {
		req(rv$gfp_cut_top)
		print("plotting overlay...")

		bf_segmented <- rv[[paste0("image_", rv$image_no %>% str_replace_all("Image", "BF") %>% str_replace_all(" ", "_"), "_segmented")]]
		rv$bf_segmented <- bf_segmented

		gfp_b <- gblur(rv$gfp_cut_top, (input$blur/1024)*nrow(rv$gfp_cut_top))
		gfp_t <- gfp_b
		gfp_t[gfp_t > input$cut_off] <- 1
		gfp_t[gfp_t <= input$cut_off] <- 0
		gfp_t[bf_segmented == 0] <- 0

		gfp_bw <- bwlabel(gfp_t[,,1])
		gfp_cfs <- computeFeatures.shape(gfp_bw)
		sels <- which(gfp_cfs[,"s.area"] < nrow(rv$gfp_cut_top)^2*(input$min_cell_size/1024))
		gfp_rms <- rmObjects(gfp_bw, sels) 
			
		if(input$remove_if_eccentric) {
		gfp_bw <- bwlabel(gfp_rms)
		gfp_cfs <- computeFeatures.shape(gfp_bw)
		gfp_cfm <- computeFeatures.moment(gfp_bw)
		sels <- which(gfp_cfs[,"s.area"] < nrow(rv$gfp_cut_top)^2*(input$min_cell_size_eccen/1024) & gfp_cfm[,"m.eccentricity"] > (input$max_eccen/1024))
		gfp_rms <- rmObjects(gfp_bw, sels) 
		}
		gfp_overlay <- rv$gfp_cut_top
		gfp_overlay[,,1] <- 0
		gfp_overlay[,,3] <- rv[[paste0("image_", rv$bf_names[rv$image_no]  %>% str_replace_all(" ", "_"))]][,,3]
		gfp_overlay[,,1] <- gfp_rms
		gfp_rms[gfp_rms >= 1] <- 1
		gfp_rms[gfp_rms < 1] <- 0
		rv$gfp_rms <- gfp_rms
		rv$gfp_overlay <- gfp_overlay
		plot(gfp_overlay)
	}
})

output$overlay_display <- renderDisplay({
	req(rv$gfp_overlay)
	print("rendering display")
	display(rv$gfp_overlay, method = "browser")
})


output$show_cellularity <- renderText({
	if(rv$done > 0) {
		if(file.exists("output/bf-analysis/cellularities.csv")) {
			try_empty <- suppressWarnings(try(read.csv("output/bf-analysis/cellularities.csv")))
			if(!(summary(try_empty)[2] == "try-error")) {
				cells <- read.csv("output/bf-analysis/cellularities.csv")
				paste0("<B>Cellularity: </B>", round(cells[rv$image_no, 2], 1), "%")
			}
		}
	}
})

observeEvent(rv$done, {
	if(rv$done > 0) {
		if(file.exists("output/gfp-analysis/overall_stats.csv")) {
			try_empty <- suppressWarnings(try(read.csv("output/gfp-analysis/overall_stats.csv")))
			if(!(summary(try_empty)[2] == "try-error")) {
				rv$stats <- read.csv("output/gfp-analysis/overall_stats.csv")
			}
		}
	}
})

output$show_perc_fluro <- renderText({
	paste0("<B>Percentage cellular area fluorescing: </B>", round(rv$stats[rv$image_no, 2], 2), "%")
})

output$show_mean_brightness <- renderText({
	paste0("<B>Mean brightness of fluorescing cells: </B>", round(rv$stats[rv$image_no, 4], 3))
})

output$show_sd_brightness <- renderText({
	paste0("<B>SD brightness of fluorescing cells: </B>", round(rv$stats[rv$image_no, 4], 3))
})

output$bf_label <- renderText(if(rv$done > 0) "Brightfield image")
output$gfp_label <- renderText(if(rv$done > 0) "GFP image")
output$brightened_label <- renderText(if(rv$done > 0) "Brightened image")
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

output$overall_dist <- renderPlot({
	req(rv$gfp_rms)
	print("plotting hist")
	gfp <- rv$gfp[,,1]
	gfp[rv$gfp_rms == 1] %>% data.frame(brightness = .) %>%
	ggplot(aes(x = brightness)) +
		geom_bar() +
		labs(title = "Brightness distribution of fluorescing area 0-1")
})


output$grid_dist <- renderPlot({
	req(rv$gfp_rms)
	req(rv$gfp)
	print("plotting grid hist")
	gfp <- rv$gfp[,,1]
	grid_size <- nrow(rv$gfp_rms) / input$grid_no
	
	gfp_split <- matsplitter(gfp, grid_size, grid_size)
	gfp_seg_split <- matsplitter(rv$gfp_rms, grid_size, grid_size)

	hist_data <- matrix(nrow = 256*input$grid_no^2, ncol = 4) %>% as.data.frame()
 	colnames(hist_data) <- c("x", "y", "brightness", "count")
 	hist_data[,1:3] <- expand.grid(c(1:input$grid_no), c(1:input$grid_no), seq(0,1, by = 1/255))


	for (roi in 1:input$grid_no^2) {
		counts <- table(gfp_split[,,roi][gfp_seg_split[,,roi] == 1]) %>% as.data.frame()
		if(nrow(counts) > 0) {
			counts_pretty <- matrix(ncol = 4, nrow = nrow(counts)) %>% as.data.frame()
			colnames(counts_pretty) <- c("x", "y", "brightness", "count")
			counts_pretty[,3:4] <- counts
			counts_pretty[,1] <- ceiling(roi/input$grid_no)
			if(roi %% input$grid_no == 0) {
				counts_pretty[,2] <- input$grid_no
			} else {
				counts_pretty[,2] <- roi %% input$grid_no
			}
			counts_pretty$brightness <- as.numeric(as.character(counts_pretty$brightness))
	
			counts_pretty <- left_join(hist_data, counts_pretty, by = c("x", "y", "brightness"))

			counts_pretty$count.x[is.na(counts_pretty$count.x)] <- counts_pretty$count.y[is.na(counts_pretty$count.x)]
			counts_pretty <- counts_pretty %>%
				mutate(count = count.x) %>%
				select(-count.x, -count.y)
			hist_data <- counts_pretty
		}		
	}

	rv$grid_size <- grid_size

	hist_data %>%
		ggplot(aes(x = brightness, y = count)) +
				geom_col(width = 1/255) +
				labs(title = "Brightness distribution of fluorescing area by grid 0-1") +
				facet_grid(y ~ x)
})

output$grid_overlay <- renderPlot({
	gfp_grid_overlay <- req(rv$gfp_overlay)
	grid_size <- req(rv$grid_size)
	
	print("adding grid to overlay image")
	for (grid_no_seq in 1:(input$grid_no-1)) {
    gfp_grid_overlay[, c(floor(grid_no_seq*rv$grid_size), ceiling(grid_no_seq*rv$grid_size)), 1] <- 1
    gfp_grid_overlay[c(floor(grid_no_seq*rv$grid_size), ceiling(grid_no_seq*rv$grid_size)), , 1] <- 1
    gfp_grid_overlay[, c(floor(grid_no_seq*rv$grid_size), ceiling(grid_no_seq*rv$grid_size)), 2] <- 1
    gfp_grid_overlay[c(floor(grid_no_seq*rv$grid_size), ceiling(grid_no_seq*rv$grid_size)), , 2] <- 1
    gfp_grid_overlay[, c(floor(grid_no_seq*rv$grid_size), ceiling(grid_no_seq*rv$grid_size)), 3] <- 1
    gfp_grid_overlay[c(floor(grid_no_seq*rv$grid_size), ceiling(grid_no_seq*rv$grid_size)), , 3] <- 1
	}
	plot(Image(gfp_grid_overlay))
})


observeEvent(input$prevButtonUI, {
	print("prev image clicked")
	count_image_no <- rv$image_no
	if(count_image_no > 1) {
		count_image_no <- count_image_no - 1
	} else if(length(rv$image_names) == 1 ) {
		return(NULL)
	} else {
		count_image_no <- length(rv$image_names)
	}
	updateSelectInput(session, "chosenImageUI", selected = rv$image_names[count_image_no])
	print(paste0("image:", rv$image_names[count_image_no]))
})

observeEvent(input$nextButtonUI, {
	print("next image clicked")
	count_image_no <- rv$image_no	
	if(count_image_no < length(rv$image_names)) {
		count_image_no <- count_image_no + 1
	} else if(length(rv$image_names) == 1){
		return(NULL)
	} else {
		count_image_no <- 1
	}
	updateSelectInput(session, "chosenImageUI", selected = rv$image_names[count_image_no])
	print(paste0("image:", rv$image_names[count_image_no]))
})

observeEvent(input$chosenImageUI, {
	if(rv$done > 0) {
	rv$image_no <- c(1:length(rv$image_names))[input$chosenImageUI == rv$image_names]
}
})

observeEvent(input$see_images, {
rv$done <- rv$done + 1
})

observeEvent(input$run, {
	if(rv$load_images == 0) rv$load_images <- 1
	rv$start_run <- rv$start_run + 1
})



# Start Run =====================================================================

observeEvent(rv$start_run, {
	if(rv$start_run > 0) {
	print("start run")
	# Check folders exist
			if(!dir.exists("output")) dir.create("output")
			if(!dir.exists("output/gfp-analysis")) dir.create("output/gfp-analysis")
			if(!dir.exists("output/gfp-analysis/gfp-segmented")) dir.create("output/gfp-analysis/gfp-segmented")
			if(!dir.exists("output/gfp-analysis/gfp-overlay")) dir.create("output/gfp-analysis/gfp-overlay")	
			if(!dir.exists("output/gfp-analysis/cell-stats") & input$cell_output) dir.create("output/gfp-analysis/cell-stats")
			if(!dir.exists("output/gfp-analysis/grid-stats") & input$grid_output) dir.create("output/gfp-analysis/grid-stats")

		check.writeable("output/gfp-analysis/overall_stats.csv")

		if(input$cell_output) invisible(sapply(paste0("output/gfp-analysis/cell-stats/", rv$gfp_names, " stats.csv"), FUN = check.writeable.grid))
		if(input$grid_output) invisible(sapply(paste0("output/gfp-analysis/grid-stats/", rv$gfp_names, " ", input$grid_no, "x", input$grid_no, " grid stats.csv"), FUN = check.writeable.grid))
		

gfp_overall_stats <- matrix(ncol = 4, nrow = length(rv$gfp_names))
colnames(gfp_overall_stats) <- c("Name", "Percentage cellular area fluorescing", "Mean brightness of fluorescing cells", "SD brightness of fluorescing cells")

	 withProgress(
   message = "Analysing images...", value = 0, {
for(row_num in 1:length(rv$gfp_names)) {
incProgress(1/length(rv$gfp_names), message = paste0(rv$gfp_names[row_num]))
if(is.null(input$change_grid_no)) {
	rv$change_grid_no <- FALSE
} else {
	rv$change_grid_no <- input$change_grid_no
}
if(!rv$change_grid_no) {
gfp <- rv[[paste0("image_", rv$gfp_names[row_num] %>% str_replace_all(" ", "_"))]]
bf <- rv[[paste0("image_", rv$bf_names[row_num] %>% str_replace_all(" ", "_"))]]
bf_segmented <- rv[[paste0("image_", rv$bf_names[row_num] %>% str_replace_all(" ", "_"), "_segmented")]]


gfp_cut_top <- gfp

gfp_cut_top[gfp_cut_top > input$brighten] <- input$brighten

gfp_cut_top <- gfp_cut_top*(1/input$brighten)


gfp_b <- gblur(gfp_cut_top, (input$blur/1024)*nrow(gfp_cut_top))

gfp_t <- gfp_b

gfp_t[gfp_t > input$cut_off] <- 1
gfp_t[gfp_t <= input$cut_off] <- 0

gfp_t[bf_segmented == 0] <- 0

gfp_bw <- bwlabel(gfp_t[,,1])
gfp_cfs <- computeFeatures.shape(gfp_bw)
sels <- which(gfp_cfs[,"s.area"] < nrow(gfp)^2*(input$min_cell_size/1024))
gfp_rms <- rmObjects(gfp_bw, sels) 

if(input$remove_if_eccentric) {
gfp_bw <- bwlabel(gfp_rms)
gfp_cfs <- computeFeatures.shape(gfp_bw)
gfp_cfm <- computeFeatures.moment(gfp_bw)
sels <- which(gfp_cfs[,"s.area"] < nrow(gfp)^2*(input$min_cell_size_eccen/1024) & gfp_cfm[,"m.eccentricity"] > (input$max_eccen/1024))
gfp_rms <- rmObjects(gfp_bw, sels) 
}

gfp_rois <- computeFeatures.moment(gfp_rms)
if(!is.null(gfp_rois)) {
	centroids <- round(gfp_rois[, c("m.cy","m.cx")])
}

gfp_overlay <- gfp_cut_top
gfp_overlay[,,1] <- 0
gfp_overlay[,,3] <- bf[,,3]
gfp_overlay[,,1] <- gfp_rms

# % cellular area fluorescing ===========================================================
cat("% cellular area fluorescing\n")

gfp_rms[gfp_rms > 1] <- 1
gfp_rms[gfp_rms < 1] <- 0

perc_fluorescing <- sum(gfp_rms)*100/sum(bf_segmented)

cat("percentage of cellular area fluorescing is ", round(perc_fluorescing, 2), "%\n")

# Get general stats =============================================================
cat("Get general stats\n")

if(!is.null(gfp_rois)) {
gfp_overall_stats[row_num,] <- c(rv$gfp_names[row_num], perc_fluorescing, mean(gfp[gfp_rms == 1]), sd(gfp[gfp_rms == 1]))
} else {
	gfp_overall_stats[row_num,] <- c(rv$gfp_names[row_num], perc_fluorescing, mean(gfp[gfp_rms == 1]), sd(gfp[gfp_rms == 1]))
}

# Get cellular stats ============================================================
if(input$cell_output) {
cat("Get cellular stats\n")
if(!is.null(gfp_rois)) {
if(is.null(nrow(centroids))) {
	cell_stats <- matrix(ncol = 5, nrow = 1)
	cell_stats[,] <- c(1, mean(gfp[gfp_rms == 1]), sd(gfp[gfp_rms == 1]), gfp_rois[,"m.cx"], gfp_rois[,"m.cy"])
} else {
	cell_stats <- matrix(ncol = 5, nrow = nrow(centroids))
	for (i in 1:nrow(centroids)) {
		gfp_bw <- bwlabel(gfp_rms)
		sel_cell <- which(as.numeric(rownames(gfp_rois)) != i)
		gfp_cell <- rmObjects(gfp_bw, sel_cell)
		
		cell_stats[i,] <- c(i, mean(gfp[gfp_cell == 1]), sd(gfp[gfp_cell == 1]), gfp_rois[i,"m.cx"], gfp_rois[i,"m.cy"])
	}
}
colnames(cell_stats) <- c("cell_no", "mean_brightness", "sd_brightness", "x", "y (top to bottom)")


print(cell_stats)
write.csv(cell_stats, file = paste0("output/gfp-analysis/cell-stats/", rv$gfp_names[row_num], " stats.csv"), row.names = FALSE)
}
}
writeImage(gfp_overlay, paste0("output/gfp-analysis/gfp-overlay/", rv$gfp_names[row_num], " overlay.", input$desired_output_format))
writeImage(gfp_rms, paste0("output/gfp-analysis/gfp-segmented/", rv$gfp_names[row_num], " segmented.", input$desired_output_format))

} # change_grid_no close

if(rv$change_grid_no) gfp_rois <- "placeholder"

# Grid output ===================================================================
if(!is.null(gfp_rois)) {
if(input$grid_output) {
cat("Grid output\n")

if(rv$change_grid_no) {
	gfp <- get(paste0("image_", rv$gfp_names[row_num] %>% str_replace_all(" ", "_")))

	if(file.exists(paste0("output/gfp-analysis/gfp-segmented/", rv$gfp_names[row_num], " segmented.", input$desired_output_format))) {
    	gfp_segmented <- readImage(paste0("output/gfp-analysis/gfp-segmented/", rv$gfp_names[row_num], " segmented.", input$desired_output_format))
    } else {
    	shinyalert("No gfp segmented image found", "Please run full script", type = "error")
    }
    
    if(file.exists(paste0("output/bf-analysis/bf-segmented/", rv$bf_names[row_num], " segmented.", desired_output_format))) {
    	bf_segmented <- readImage(paste0("output/bf-analysis/bf-segmented/", rv$bf_names[row_num], " segmented.", input$desired_output_format))
	} else {
		shinyalert("No bf segmented image found", "Please run cellularity script", type = "error")
	}
} else gfp_segmented <- gfp_rms

if (nrow(gfp_segmented) %% input$grid_no != 0) {
	new_size <- nrow(gfp_segmented) - (nrow(gfp_segmented) %% input$grid_no)
	gfp_segmented <- resize(gfp_segmented, w = new_size, h = new_size)
}

gfp <- gfp[,,1]

grid_size <- nrow(gfp_segmented) / input$grid_no

gfp_split <- matsplitter(gfp, grid_size, grid_size)
gfp_seg_split <- matsplitter(gfp_segmented, grid_size, grid_size)
bf_seg_split <- matsplitter(bf_segmented, grid_size, grid_size)

d_stats_grid <- matrix(ncol = 5, nrow = input$grid_no^2)
colnames(d_stats_grid) <- c("Grid x (L>R)", "Grid y (U>D)", "Percentage cells fluorescing", "Mean brightness", "SD in brightness")

for(roi in 1:input$grid_no^2) {
	d_stats_grid[roi,1] <- ceiling(roi/input$grid_no)
	if(roi %% input$grid_no == 0) {
		d_stats_grid[roi,2] <- 4
	} else {
		d_stats_grid[roi,2] <- roi %% input$grid_no
	}
	d_stats_grid[roi,3:5] <- calc.fluro.stats(gfp_split[,,roi], gfp_seg_split[,,roi], bf_seg_split[,,roi])
}
write.csv(d_stats_grid, file = paste0("output/gfp-analysis/grid-stats/", rv$gfp_names[row_num], " ", input$grid_no, "x", input$grid_no, " grid stats.csv"), row.names = FALSE)

}
}
}
if(!rv$change_grid_no){
	write.csv(gfp_overall_stats, file = "output/gfp-analysis/overall_stats.csv", row.names = FALSE)
}
showNotification("Done!")
rv$done <- rv$done + 1
}
)
}
})



observeEvent(input$run_selected, {
	if(rv$load_images == 0) rv$load_images <- 1
	rv$start_run_selected <- rv$start_run_selected + 1
})



observeEvent(rv$start_run_selected, {
	if(rv$start_run_selected > 0) {
			# Check folders exist
			if(!dir.exists("output")) dir.create("output")
			if(!dir.exists("output/gfp-analysis")) dir.create("output/gfp-analysis")
			if(!dir.exists("output/gfp-analysis/gfp-segmented")) dir.create("output/gfp-analysis/gfp-segmented")
			if(!dir.exists("output/gfp-analysis/gfp-overlay")) dir.create("output/gfp-analysis/gfp-overlay")	
			if(!dir.exists("output/gfp-analysis/cell-stats") & input$cell_output) dir.create("output/gfp-analysis/cell-stats")
			if(!dir.exists("output/gfp-analysis/grid-stats") & input$grid_output) dir.create("output/gfp-analysis/grid-stats")

row_num <- rv$image_no
	check.writeable("output/gfp-analysis/overall_stats.csv")

		if(input$cell_output) invisible(check.writeable.grid(paste0("output/gfp-analysis/cell-stats/", rv$gfp_names[row_num], " stats.csv")))
		if(input$grid_output) invisible(check.writeable.grid(paste0("output/gfp-analysis/grid-stats/", rv$gfp_names[row_num], " ", input$grid_no, "x", input$grid_no, " grid stats.csv")))
		

if(file.exists("output/gfp-analysis/overall_stats.csv")) {
	gfp_overall_stats <- read.csv("output/gfp-analysis/overall_stats.csv")
	} else {
	gfp_overall_stats <- data.frame(matrix(ncol = 3, nrow = length(rv$gfp_names)))
}
colnames(gfp_overall_stats) <- c("Name", "Percentage cellular area fluorescing", "Mean brightness of fluorescing cells", "SD brightness of fluorescing cells")

if(is.null(input$change_grid_no)) {
	rv$change_grid_no <- FALSE
} else {
	rv$change_grid_no <- input$change_grid_no
}


if(!rv$change_grid_no) {
gfp <- rv[[paste0("image_", rv$gfp_names[row_num] %>% str_replace_all(" ", "_"))]]
bf <- rv[[paste0("image_", rv$bf_names[row_num] %>% str_replace_all(" ", "_"))]]
bf_segmented <- rv[[paste0("image_", rv$bf_names[row_num] %>% str_replace_all(" ", "_"), "_segmented")]]


gfp_cut_top <- gfp

gfp_cut_top[gfp_cut_top > input$brighten] <- input$brighten

gfp_cut_top <- gfp_cut_top*(1/input$brighten)


gfp_b <- gblur(gfp_cut_top, (input$blur/1024)*nrow(gfp_cut_top))

gfp_t <- gfp_b

gfp_t[gfp_t > input$cut_off] <- 1
gfp_t[gfp_t <= input$cut_off] <- 0

gfp_t[bf_segmented == 0] <- 0

gfp_bw <- bwlabel(gfp_t[,,1])
gfp_cfs <- computeFeatures.shape(gfp_bw)
sels <- which(gfp_cfs[,"s.area"] < nrow(gfp)^2*(input$min_cell_size/1024))
gfp_rms <- rmObjects(gfp_bw, sels) 

if(input$remove_if_eccentric) {
gfp_bw <- bwlabel(gfp_rms)
gfp_cfs <- computeFeatures.shape(gfp_bw)
gfp_cfm <- computeFeatures.moment(gfp_bw)
sels <- which(gfp_cfs[,"s.area"] < nrow(gfp)^2*(input$min_cell_size_eccen/1024) & gfp_cfm[,"m.eccentricity"] > (input$max_eccen/1024))
gfp_rms <- rmObjects(gfp_bw, sels) 
}

gfp_rois <- computeFeatures.moment(gfp_rms)
if(!is.null(gfp_rois)) {
	centroids <- round(gfp_rois[, c("m.cy","m.cx")])
}

gfp_overlay <- gfp_cut_top
gfp_overlay[,,1] <- 0
gfp_overlay[,,3] <- bf[,,3]
gfp_overlay[,,1] <- gfp_rms

# % cellular area fluorescing ===========================================================
cat("% cellular area fluorescing\n")

gfp_rms[gfp_rms > 1] <- 1
gfp_rms[gfp_rms < 1] <- 0

perc_fluorescing <- sum(gfp_rms)*100/sum(bf_segmented)

cat("percentage of cellular area fluorescing is ", round(perc_fluorescing, 2), "%\n")

# Get general stats =============================================================
cat("Get general stats\n")

if(!is.null(gfp_rois)) {
gfp_overall_stats[row_num,] <- c(rv$gfp_names[row_num], perc_fluorescing, mean(gfp[gfp_rms == 1]), sd(gfp[gfp_rms == 1]))
} else {
	gfp_overall_stats[row_num,] <- c(rv$gfp_names[row_num], perc_fluorescing, mean(gfp[gfp_rms == 1]), sd(gfp[gfp_rms == 1]))
}

# Get cellular stats ============================================================
if(input$cell_output) {
cat("Get cellular stats\n")
if(!is.null(gfp_rois)) {
if(is.null(nrow(centroids))) {
	cell_stats <- matrix(ncol = 5, nrow = 1)
	cell_stats[,] <- c(1, mean(gfp[gfp_rms == 1]), sd(gfp[gfp_rms == 1]), gfp_rois[,"m.cx"], gfp_rois[,"m.cy"])
} else {
	cell_stats <- matrix(ncol = 5, nrow = nrow(centroids))
	for (i in 1:nrow(centroids)) {
		gfp_bw <- bwlabel(gfp_rms)
		sel_cell <- which(as.numeric(rownames(gfp_rois)) != i)
		gfp_cell <- rmObjects(gfp_bw, sel_cell)
		
		cell_stats[i,] <- c(i, mean(gfp[gfp_cell == 1]), sd(gfp[gfp_cell == 1]), gfp_rois[i,"m.cx"], gfp_rois[i,"m.cy"])
	}
}
colnames(cell_stats) <- c("cell_no", "mean_brightness", "sd_brightness", "x", "y (top to bottom)")


print(cell_stats)
write.csv(cell_stats, file = paste0("output/gfp-analysis/cell-stats/", rv$gfp_names[row_num], " stats.csv"), row.names = FALSE)
}
}
writeImage(gfp_overlay, paste0("output/gfp-analysis/gfp-overlay/", rv$gfp_names[row_num], " overlay.", input$desired_output_format))
writeImage(gfp_rms, paste0("output/gfp-analysis/gfp-segmented/", rv$gfp_names[row_num], " segmented.", input$desired_output_format))

} # change_grid_no close

if(rv$change_grid_no) gfp_rois <- "placeholder"

# Grid output ===================================================================
if(!is.null(gfp_rois)) {
if(input$grid_output) {
cat("Grid output\n")

if(rv$change_grid_no) {
	gfp <- get(paste0("image_", rv$gfp_names[row_num] %>% str_replace_all(" ", "_")))

	if(file.exists(paste0("output/gfp-analysis/gfp-segmented/", rv$gfp_names[row_num], " segmented.", input$desired_output_format))) {
    	gfp_segmented <- readImage(paste0("output/gfp-analysis/gfp-segmented/", rv$gfp_names[row_num], " segmented.", input$desired_output_format))
    } else {
    	shinyalert("No gfp segmented image found", "Please run full script", type = "error")
    }
    
    if(file.exists(paste0("output/bf-analysis/bf-segmented/", rv$bf_names[row_num], " segmented.", desired_output_format))) {
    	bf_segmented <- readImage(paste0("output/bf-analysis/bf-segmented/", rv$bf_names[row_num], " segmented.", input$desired_output_format))
	} else {
		shinyalert("No bf segmented image found", "Please run cellularity script", type = "error")
	}
} else gfp_segmented <- gfp_rms

if (nrow(gfp_segmented) %% input$grid_no != 0) {
	new_size <- nrow(gfp_segmented) - (nrow(gfp_segmented) %% input$grid_no)
	gfp_segmented <- resize(gfp_segmented, w = new_size, h = new_size)
}

gfp <- gfp[,,1]

grid_size <- nrow(gfp_segmented) / input$grid_no

gfp_split <- matsplitter(gfp, grid_size, grid_size)
gfp_seg_split <- matsplitter(gfp_segmented, grid_size, grid_size)
bf_seg_split <- matsplitter(bf_segmented, grid_size, grid_size)

d_stats_grid <- matrix(ncol = 5, nrow = input$grid_no^2)
colnames(d_stats_grid) <- c("Grid x (L>R)", "Grid y (U>D)", "Percentage cells fluorescing", "Mean brightness", "SD in brightness")

for(roi in 1:input$grid_no^2) {
	d_stats_grid[roi,1] <- ceiling(roi/input$grid_no)
	if(roi %% input$grid_no == 0) {
		d_stats_grid[roi,2] <- 4
	} else {
		d_stats_grid[roi,2] <- roi %% input$grid_no
	}
	d_stats_grid[roi,3:5] <- calc.fluro.stats(gfp_split[,,roi], gfp_seg_split[,,roi], bf_seg_split[,,roi])
}
write.csv(d_stats_grid, file = paste0("output/gfp-analysis/grid-stats/", rv$gfp_names[row_num], " ", input$grid_no, "x", input$grid_no, " grid stats.csv"), row.names = FALSE)

}
}
if(!rv$change_grid_no){
	write.csv(gfp_overall_stats, file = "output/gfp-analysis/overall_stats.csv", row.names = FALSE)
}
print(rv$done)
showNotification("Done!")
rv$done <- rv$done + 1 
print(rv$done)

}
})
}

shinyApp(ui = ui, server = server)