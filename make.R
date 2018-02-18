Sys.setenv(RSTUDIO_PANDOC="/Applications/RStudio.app/Contents/MacOS/pandoc")
rmarkdown::render("src/appendix.Rmd", output_file="../appendix.pdf")
