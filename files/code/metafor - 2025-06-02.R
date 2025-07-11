# This file is available at http://openmetaanalysis.github.io/Sepsis-fluids_and_timing_-_dose-response_meta-analysis
# Author:rbadgett@kumc.edu
# Permissions:
#* Code GNU GPLv3 https://choosealicense.com/licenses/gpl-3.0/
#* Images CC BY-NC-SA 4.0 https://creativecommons.org/licenses/by-nc-sa/4.0/
# Optimized for coding with R Studio document outline view
# Last edited 2025-06-02

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

# draw one panel for a given time-horizon ------
function_plot_panel <- function(hours) {
  
  # prediction grid ------
  dose_seq  <- seq(min(data.import$dose),
                   max(data.import$dose),
                   length.out = 100)
  fix_hours <- hours
  step      <- diff(dose_seq)[1]               # grid spacing
  
  newdat <- data.frame(
    dose           = dose_seq,
    dose_sq        = dose_seq^2,
    hours          = fix_hours,
    doseXhours     = dose_seq * fix_hours,
    doseSqXhours   = (dose_seq^2) * fix_hours
  )
  preds <- predict(res_quad,
                   newmods = as.matrix(newdat),
                   transf  = plogis)
  
  # basic curve (y-label pushed left) ------
  par(las = 1, mgp = c(3.5, 0.8, 0))           # mgp[1] enlarges label margin
  plot(dose_seq, preds$pred, type = "l",
       #ylim = c(0, max(preds$ci.ub)),
       ylim = c(0, 0.5),
       xlab = "Fluid volume (ml/kg)",
       ylab = "Mortality\n(predicted)")
  lines(dose_seq, preds$ci.lb, lty = 2)
  lines(dose_seq, preds$ci.ub, lty = 2)
  
  # threshold & zone indices ------
  thr_ci    <- min(preds$ci.ub)                # lowest upper-CI
  best_idx  <- which.min(preds$ci.ub)
  best_dose <- dose_seq[best_idx]
  
  worse_idx <- which(preds$pred > thr_ci + 1e-12)
  left_idx  <- worse_idx[dose_seq[worse_idx] <  best_dose]
  right_idx <- worse_idx[dose_seq[worse_idx] >= best_dose]
  mid_idx   <- setdiff(seq_along(dose_seq), worse_idx)
  
  # helper: closed polygon (overlaps by ±step/2) ------
  poly_zone <- function(idx, col) {
    if (length(idx) == 0) return()
    x_left  <- dose_seq[min(idx)]  - step/2
    x_right <- dose_seq[max(idx)]  + step/2
    polygon(
      x = c(x_left,  dose_seq[idx],  x_right,  x_left),
      y = c(0,       preds$pred[idx], 0,       0),
      col = col, border = NA)
  }
  
  # colour zones (green first, red over-paint) ------
  poly_zone(mid_idx  , rgb(0, 1, 0, 0.10))     # green
  poly_zone(left_idx , rgb(1, 0, 0, 0.12))     # red left
  poly_zone(right_idx, rgb(1, 0, 0, 0.12))     # red right
  
  # reference line & label ------
  abline(v = best_dose, col = "red", lty = 2)
  text(best_dose, 0.05,
       paste0("Lowest 95%-CI ≈ ", round(best_dose), " ml/kg"),
       pos = 4, col = "red")
  
  # red ticks & numbers (internal borders) ------
  axis_tick <- function(xp) {
    axis(1, at = xp, labels = FALSE,
         tcl = -1.0, col = "red", col.ticks = "red")
    mtext(round(xp), side = 1, at = xp,
          line = 2.6, col = "red", cex = 0.8)
  }
  if (length(left_idx)  > 0) axis_tick(dose_seq[max(left_idx)]  + step/2)
  if (length(right_idx) > 0) axis_tick(dose_seq[min(right_idx)] - step/2)
}

# __________________________________________-----
# Libraries --------------
library(metafor)
library(openxlsx)

# Data grab -----------
### Data grab ===================================

# co <- read.table("https://data.princeton.edu/eco572/datasets/cohhpop.dat", col.names=c("age","pop"), header=FALSE)
file.filter   <- matrix(c("Text","*.txt","Spreadsheets","*.csv;*.xls;*.xlsx","All","..\\data\\*.*"),byrow=TRUE,ncol=2)
filename      <- choose.files(filters = file.filter,caption = "Select data file",index = 1,multi=FALSE)
#file.extension<- substr(filename, nchar(filename) - 2, nchar(filename))
file.extension<- substr(filename,regexpr("\\.[^\\.]*$", filename)+1, nchar(filename))
data.import <- NULL
if (file.extension == 'csv'){
  data.import   <- read.csv(filename, header=TRUE, sep=",", na.strings="NA", dec=".", stringsAsFactors=FALSE, strip.white=TRUE)
}else{
  #data.import   <- read.xlsx(filename)
  #data.import<- read.table(filename, header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)
  # Help https://rdrr.io/cran/openxlsx/man/loadWorkbook.html
  wb.temp <- loadWorkbook(filename)
  data.import <- read.xlsx (wb.temp, sheet = 1, startRow = 1, colNames = TRUE, na.strings = "NA", detectDates = TRUE)
}

# (1) Load data and do continuity correction
data.import$dose_sq <- data.import$dose^2
data.import$cases[data.import$cases == 0] <- 0.5
all_events <- data.import$cases == data.import$n
data.import$n[all_events]      <- data.import$n[all_events] + 0.5
data.import$cases[all_events]  <- data.import$cases[all_events] - 0.5

# (2) Fit the model with dose, dose_sq, hours -----
# Step A: In your data, define columns for the interactions:
data.import$doseXhours   <- data.import$dose * data.import$hours
data.import$doseSqXhours <- data.import$dose_sq * data.import$hours

# Step B: Modify the formula to match those names:
res_quad <- rma.glmm(
  measure = "PLO",
  xi      = cases,
  ni      = n,
  mods    = ~ dose + dose_sq + hours + doseXhours + doseSqXhours,
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
       heights = c(0.05, 0.3, 0.3, 0.3, 0.3))
par(oma = c(1, 1, 1, 1))  # Outer margins: bottom, left, top, right
par(cex.axis = 1.3)
par(cex.lab  = 1.3)

#---- Title ----
par(mar = c(0, 0, 0, 0))
plot.new()
mtext(bquote(bold("Figure 1. ") ~ .("Optimal fluid volume over the first 24 hours")), 
      side = 3, line = -1, cex = 1, adj = 0, outer = FALSE)

# Reset margins for the subsequent panels
par(mar = c(5 + 2, 4 + 2, 0.8, 0))  # Reduce top margin (from 1 to 0.8) to decrease gap

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
mtext("Red zones: doses whose predicted mortality exceeds the lowest upper 95% CI across all doses.",
      side = 1, line = 6, col = "red", cex = 1, adj = 0)
mtext(paste0("rbadgett@kumc.edu, ",Sys.Date()),
      side=1, line = 7, cex = 0.8, adj=1)

#* Print -----
function_plot_print("Figure 1v2. Optimal fluid rates over 24 hours", 800, 1500, imagetype = "png")

