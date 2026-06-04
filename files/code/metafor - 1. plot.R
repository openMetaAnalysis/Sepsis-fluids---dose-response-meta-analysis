# This file is available at http://openmetaanalysis.github.io/Sepsis-fluids_and_timing_-_dose-response_meta-analysis
# Author:rbadgett@kumc.edu
# Permissions:
#* Code GNU GPLv3 https://choosealicense.com/licenses/gpl-3.0/
#* Images CC BY-NC-SA 4.0 https://creativecommons.org/licenses/by-nc-sa/4.0/
# Optimized for coding with R Studio document outline view
# Last edited 2025-06-02

# Background
# See chat in "Research - sepsis", "Library Dosresmeta model error fix"
# Summary: could not get library doseresmeta or metafor to meet needs 
# Chat switched to custom code,
# * search for newdat and doseXhours
 
#== Startup ======
library(tcltk) # For interactions and troubleshooting, part of base package so no install needed.

#* Set working directory -----
if (Sys.getenv("RSTUDIO") != "1"){
  args <- commandArgs(trailingOnly = FALSE)
  script_path <- sub("--file=", "", args[grep("--file=", args)])  
  script_path <- dirname(script_path)
  setwd(script_path)
}else{
  setwd(dirname(rstudioapi::getSourceEditorContext()$path))
}
getwd()

# __________________________________-----
# Functions ------
function_plot_print <- function (plotname, plotwidth, plotheight, imagetype) {
  
  #plotname <- gsub("[:\\s\n?!']", "", plotname)
  plotname <- gsub(":|\\s|\\n|\\?|\\!|\\'", "", plotname)
  
  current.date <- as.character(strftime(Sys.time(), format="%Y-%m-%d", tz="", usetz=FALSE))
  
  rstudioapi::savePlotAsImage(
    paste(plotname, ' -- ', current.date, '.', imagetype, sep=''),
    format = imagetype, width = plotwidth, height = plotheight)
}

add_tick <- function(xpos) {
  axis(1, at = xpos, labels = FALSE, tcl = -0.4,
       col = "red", col.ticks = "red")
  mtext(round(xpos), side = 1, at = xpos,
        line = 1.2, col = "red", cex = 0.8)
}

# ____________________-----
# Overall weighted prediction R2 -----
function_overall_prediction_r2 <- function() {
  
  needed_vars <- c("cases", "n", "dose", "hours")
  
  dat_r2 <- data.import[complete.cases(data.import[, needed_vars]), , drop = FALSE]
  
  dat_r2 <- dat_r2[
    is.finite(dat_r2$cases) &
      is.finite(dat_r2$n) &
      dat_r2$n > 0 &
      is.finite(dat_r2$dose) &
      is.finite(dat_r2$hours),
    ,
    drop = FALSE
  ]
  
  obs_r2 <- dat_r2$cases / dat_r2$n
  
  newdat_r2 <- data.frame(
    dose               = dat_r2$dose,
    dose_squared       = dat_r2$dose^2,
    hours              = dat_r2$hours,
    doseXhours         = dat_r2$dose * dat_r2$hours,
    dose_squaredXhours = dat_r2$dose^2 * dat_r2$hours
  )
  
  pred_r2 <- as.numeric(
    predict(
      res_quad,
      newmods = as.matrix(newdat_r2),
      transf  = plogis
    )$pred
  )
  
  w_r2 <- dat_r2$n
  
  ss_res <- sum(w_r2 * (obs_r2 - pred_r2)^2, na.rm = TRUE)
  ss_tot <- sum(w_r2 * (obs_r2 - weighted.mean(obs_r2, w_r2, na.rm = TRUE))^2,
                na.rm = TRUE)
  
  if (!is.finite(ss_tot) || ss_tot <= 0) {
    return(NA_real_)
  }
  
  100 * (1 - ss_res / ss_tot)
}

function_panel_arm_count_label <- function(hours_value) {
  
  needed_vars <- c("cases", "n", "dose", "hours")
  
  dat_arms <- data.import[complete.cases(data.import[, needed_vars]), , drop = FALSE]
  
  dat_arms <- dat_arms[
    is.finite(dat_arms$cases) &
      is.finite(dat_arms$n) &
      dat_arms$n > 0 &
      is.finite(dat_arms$dose) &
      is.finite(dat_arms$hours) &
      abs(dat_arms$hours - hours_value) < sqrt(.Machine$double.eps),
    ,
    drop = FALSE
  ]
  
  bquote(directly~observed~arms == .(nrow(dat_arms)))
}

## _________________________________-----
function_plot_panel <- function(hours) {
  # hours <- 3 # for testing
  # data prep --------------------------------------------------------
  dose_seq <- seq(min(data.import$dose),
                  max(data.import$dose), length.out = 200)
  step <- diff(dose_seq)[1]
  
  newdat <- within(data.frame(dose = dose_seq), {
    dose_squared      <- dose^2
    hours        <- hours
    doseXhours   <- dose * hours
    dose_squaredXhours <- dose_squared * hours
  })
  preds <- predict(res_quad,
                   newmods = as.matrix(newdat),
                   transf  = plogis)
  
  # basic curve ------------------------------------------------------
  par(las = 1, mgp = c(3, 0.8, 0))
  plot(dose_seq, preds$pred, type = "l",
       ylim = c(0, 0.4),
       xlab = "Fluid volume (ml/kg)",
       ylab = "Mortality\n(predicted)",
       yaxt = "n")
  axis(2, at = seq(0, 0.4, 0.1),
       labels = sprintf("%.1f", seq(0, 0.4, 0.1)))
  lines(dose_seq, preds$ci.lb, lty = 2)
  lines(dose_seq, preds$ci.ub, lty = 2)
  
  # Left-side text block for treatment-arm annotation ------------------
  arms_panel_label <- function_panel_arm_count_label(hours)
  
  usr <- par("usr")
  text(x = usr[1] + 0.20 * diff(usr[1:2]),
       y = usr[4] - 0.07 * diff(usr[3:4]),
       labels = arms_panel_label,
       adj = c(0, 1),
       cex = 1.1)

  # unsafe zones -----------------------------------------------------
  thr_ci    <- min(preds$ci.ub)
  best_idx  <- which.min(preds$ci.ub)
  best_dose <- dose_seq[best_idx]
  
  worse_idx <- which(preds$pred > thr_ci + 1e-12)
  left_idx  <- worse_idx[dose_seq[worse_idx] <  best_dose]
  right_idx <- worse_idx[dose_seq[worse_idx] >= best_dose]
  mid_idx   <- setdiff(seq_along(dose_seq), worse_idx)
  
  # band helper for full-height rectangles --------------------------
  fill_band <- function(idx) {
    if (!length(idx)) return()
    rect(dose_seq[min(idx)] - step/2, 0,
         dose_seq[max(idx)] + step/2, 0.4,
         col = rgb(1, 0, 0, 0.12), border = NA)
  }
  
  # code selection block --------------------------------------------
  if (1 == 2) {
    ## Code for color blocks to fill vertical distance --------------
    if (length(left_idx))  fill_band(left_idx)
    if (length(right_idx)) fill_band(right_idx)
    
    ## green centre band (full-height) ------------------------------
    mid_band <- function(idx) {
      if (!length(idx)) return()
      rect(dose_seq[min(idx)] - step/2, 0,
           dose_seq[max(idx)] + step/2, 0.4,
           col = rgb(0, 1, 0, 0.10), border = NA)
    }
    mid_band(mid_idx)
    
  } else {
    
    # code  for original polygon distribution of colors -------------
    poly_full <- function(idx, col) {
      if (!length(idx)) return()
      polygon(
        x = c(dose_seq[min(idx)] - step/2,
              dose_seq[idx],
              dose_seq[max(idx)] + step/2,
              dose_seq[min(idx)] - step/2),
        y = c(0,
              preds$pred[idx],
              0,
              0),
        col = col, border = NA)
    }
    
    poly_full(left_idx , rgb(1, 0, 0, 0.12))
    poly_full(right_idx, rgb(1, 0, 0, 0.12))
    poly_full(mid_idx  , rgb(0, 1, 0, 0.10))
  }
  
  ### reference line & ticks ------------------------------------------
  abline(v = best_dose, col = "red", lty = 2)
  text(best_dose, 0.02,
       paste0("Lowest 95%-CI ≈ ", round(best_dose), " ml/kg"),
       pos = 4, col = "red")
  
  add_tick <- function(xp) {
    axis(1, at = xp, labels = FALSE, tcl = -1.4,
         col = "red", col.ticks = "red")
    mtext(round(xp), side = 1, at = xp,
          line = 3.4, col = "red", cex = 0.8)
  }
  if (length(left_idx))
    add_tick(dose_seq[max(left_idx)]  + step/2)
  if (length(right_idx))
    add_tick(dose_seq[min(right_idx)] - step/2)
  
  ### Right-side text block for doses -------------------------------------------
  text_legend <- sprintf("Model-favored dose = %.0f ml/kg", best_dose)
  if (length(left_idx))
    text_legend <- c(text_legend,
                     sprintf("Higher predicted mortality ≤ %.0f ml/kg", dose_seq[max(left_idx)]))
  if (length(right_idx))
    text_legend <- c(text_legend,
                     sprintf("Higher predicted mortality ≥ %.0f ml/kg", dose_seq[min(right_idx)]))
  
  x_leg <- best_dose + 0.02 * diff(range(dose_seq))
  
  y_leg <- c(
    usr[4] - 0.07 * diff(usr[3:4]),
    usr[4] - 0.20 * diff(usr[3:4]),
    usr[4] - 0.33 * diff(usr[3:4])
  )[seq_along(text_legend)]
  
  text(x = rep(x_leg, length(text_legend)),
       y = y_leg,
       labels = text_legend,
       adj = c(0, 1),
       cex = 1.1,
       col = ifelse(grepl("^Unsafe", text_legend), "red", "black"))
  }

# __________________________________________-----
# Libraries --------------
library(crayon)
library(metafor)
library(openxlsx)

# Data grab -----------
### Data grab ===================================

file.filter   <- matrix(c("Text","*.txt","Spreadsheets","*.csv;*.xls;*.xlsx","All","..\\data\\*.*"),byrow=TRUE,ncol=2)
filename      <- choose.files(filters = file.filter,caption = "Select data file",index = 2, multi=FALSE)
#file.extension<- substr(filename, nchar(filename) - 2, nchar(filename))
file.extension<- substr(filename,regexpr("\\.[^\\.]*$", filename)+1, nchar(filename))
data.import <- NULL
if (file.extension == 'csv'){
  data.import   <- read.csv(filename, header=TRUE, sep=",", na.strings="NA", dec=".", stringsAsFactors=FALSE, strip.white=TRUE)
}else{
  wb.temp <- loadWorkbook(filename)
  data.import <- read.xlsx (wb.temp, sheet = 1, startRow = 1, colNames = TRUE, na.strings = "NA", detectDates = TRUE)
}

# (1) Load data and do continuity correction ------
data.import <- data.import[
  complete.cases(data.import[, c("cases", "n", "dose", "hours")]),
  ,
  drop = FALSE
]

data.import$dose_squared <- data.import$dose^2

data.import$cases[data.import$cases == 0] <- 0.5

all_events <- data.import$cases == data.import$n

data.import$n[all_events]     <- data.import$n[all_events] + 0.5
data.import$cases[all_events] <- data.import$cases[all_events] - 0.5

# (2) Fit the model with dose, dose_squared, hours -----
# Step A: In your data, define columns for the interactions:
data.import$doseXhours   <- data.import$dose * data.import$hours
data.import$dose_squaredXhours <- data.import$dose_squared * data.import$hours

overall_prediction_r2 <- function_overall_prediction_r2()

# Step B: Modify the formula to match those names and use metafor:
res_quad <- rma.glmm(
  measure = "PLO",
  xi      = cases,
  ni      = n,
  mods    = ~ dose + dose_squared + hours + doseXhours + dose_squaredXhours,
  data    = data.import,
  slevel  = "Study",
  model   = "UM.RS",
  method  = "ML",
  add     = 0.5,
  to      = "all"
)
summary(res_quad)

# _______________________________-----
# Print panels -----------------
if(dev.cur() > 1) dev.off() 

#* Par restore
#par(mar=c(5.1 + 2,4.1,4.1,2.1), mfrow=c(1,1)) # (bottom, left, top, right)

layout(matrix(1:5, ncol = 1), 
       heights = c(0.11, 0.2725, 0.2725, 0.2725, 0.2725))

par(oma = c(4, 1, 0.25, 1))  # Outer margins: bottom, left, top, right
par(cex.axis = 1.3)
par(cex.lab  = 1.3)

#---- Caption panel ----
par(mar = c(0, 0, 0, 0))
plot.new()
par(usr = c(0, 1, 0, 1))

cap_text <- paste0(
  "Figure 1. Model-predicted mortality by fluid volume over the first 24 hours. ",
  "Each panel is a time-specific slice from the same dose-by-time meta-regression fitted to all eligible observed study arms across time points; ",
  "weighted in-sample prediction R² across observed arms was ",
  format(round(overall_prediction_r2, 1), nsmall = 1),
  "%. The in-panel count shows the number of directly observed study arms at that time; ",
  "Panel C has no directly observed 12-hour arms and is an interpolated model prediction. ",
  "Red shading marks dose ranges where predicted mortality is higher than the figure's conservative best-dose threshold.*"
)

cap_lines <- strwrap(cap_text, width = 115)

text(x = 0,
     y = 0.96 - (seq_along(cap_lines) - 1) * 0.17,
     labels = cap_lines,
     adj = c(0, 1),
     cex = 1.45)

# Reset margins for the subsequent panels
par(mar = c(5 + 2, 4 + 2, 0.8, 0))

# A) 3 hours --------------
function_plot_panel(3)
mtext(bquote(bold("Panel A. ") ~ .("Optimal fluid volume by 3 hours.")), 
      side = 3, line = 0, at = par("usr")[1], cex = 1, adj = 0, outer = FALSE)

# B) 6 hours --------------
function_plot_panel(6)
mtext(bquote(bold("Panel B. ") ~ .("Optimal fluid volume by 6 hours.")), 
      side = 3, line = 0, at = par("usr")[1], cex = 1, adj = 0, outer = FALSE)

# C) 12 hours --------------
function_plot_panel(12)
mtext(bquote(bold("Panel C. ") ~ .("Optimal fluid volume by 12 hours.")), 
      side = 3, line = 0, at = par("usr")[1], cex = 1, adj = 0, outer = FALSE)

# D) 24 hours --------------
function_plot_panel(24)
mtext(bquote(bold("Panel D. ") ~ .("Optimal fluid volume by 24 hours.")), 
      side = 3, line = 0, at = par("usr")[1], cex = 1, adj = 0, outer = FALSE)

# Footer -----------------------
mtext("Notes:",
      side = 1, line = 5, col = "black", cex = 1, adj = 0, font = 2)

temp_text <- "* In this range, the confidence intervals of the predicted mortality are above the predicted mortality of the lowest point."
temp_text <- "* Red shading marks dose ranges where the model-predicted mortality is above the lowest upper 95% confidence-limit value across the dose curve." # These regions should be interpreted as model-favored versus less-favored dose ranges, not as definitive proof of safe or unsafe dosing.
temp_text <- paste(strwrap(temp_text, 90),
                   collapse = "\n")

mtext(temp_text, side = 1, line = 8, col = "black", cex = 1, adj = 0)

#mtext(paste0("rbadgett@kumc.edu, ",Sys.Date()), side=1, line = 9, cex = 0.8, adj=1)

#* Print -----
function_plot_print("Figure 1vb. Optimal fluid rates over 24 hours", 800, 1500, imagetype = "png")

