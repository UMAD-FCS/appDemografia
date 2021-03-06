import_titillium_web <- function() {
  
  tw_font_dir <- system.file("fonts", "titillium-web", package = "hrbrthemes")
  
  suppressWarnings(suppressMessages(extrafont::font_import(tw_font_dir, prompt = FALSE)))
  
  message(
    sprintf(
      "You will likely need to install these fonts on your system as well.\n\nYou can find them in [%s]",
      tw_font_dir)
  )
  
}

# theme_bdd

theme_bdd <- function(
  base_family = "Titillium Web", 
  base_size = 12,
  plot_title_family = if (.Platform$OS.type == "windows") "Titillium Web" else "Titillium Web Bold",
  plot_title_size = 15,
  plot_title_face = "bold",
  plot_title_margin = 12,
  subtitle_family = if (.Platform$OS.type == "windows") "Titillium Web" else "Titillium Web Light",
  subtitle_size = 13,
  subtitle_face = "plain",
  subtitle_margin = 15,
  strip_text_family = base_family,
  strip_text_size = 12,
  strip_text_face = "plain",
  caption_family = if (.Platform$OS.type == "windows") "Titillium Web" else "Titillium Web",
  caption_size = 10,
  caption_face = "plain", 
  caption_margin = 10,
  axis_text_size = base_size,
  axis_title_family = base_family,
  axis_title_size = 10,
  axis_title_face = "plain",
  axis_title_just = "rt",
  plot_margin = margin(30, 30, 30, 30)) {
  
  ggplot2::theme_minimal(base_family = base_family, base_size = base_size)
  
}

blue_bdd <- "#0075BE"
blue_bdd2 <- "#3290cb"
blue_bdd3 <- "#4c9ed1"

theme_ggparliament_bdd <- function(legend = TRUE,
                                   background_colour = FALSE,
                                   border = FALSE) {
  basic_theme <- theme_void(base_family = "Titillium Web", base_size = 12)
  
  
  if (legend == TRUE) {
    basic_theme <- basic_theme
  } else {
    basic_theme <- basic_theme + ggplot2::theme(legend.position = "none")
  }
  
  
  
  if (!background_colour) {
    basic_theme <- basic_theme
  } else {
    basic_theme <- basic_theme + 
      ggplot2::theme(panel.background = ggplot2::element_rect(fill = "#F5F5F5", colour = NA)) # white smoke fill
  }
  
  
  if (!border) {
    basic_theme <- basic_theme
  } else {
    basic_theme <- basic_theme + 
      ggplot2::theme(panel.border = ggplot2::element_rect(colour = "#F5F5F5", fill = NA)) # white smoke colour
  }
  
  basic_theme
}  

wrapit <- function(x) {
  wtext <- paste(strwrap(x, width = 120), collapse=" \n ")
  return(wtext)
}