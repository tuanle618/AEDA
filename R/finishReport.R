finishReport = function(..., sub.dir = "Data_Report", save.mode = TRUE){
  x = list(...)
  for (i in names(x)) {
    catf("```{r %s, child = %s}", i,  x[[i]])
    writeLines("```")
    writeLines("")
  }
  for (i in names(x)){
    ### writeReport is the S3 Method which should pick the correct write function for each object
    writeReport(x[[i]], sub.dir, save.mode)
  }
}

# finishReport(x=1,y="test")
