# if a Sub Directory doesn't exist it Creates one (in the working directory) and moves
# the wd to the sub directory. It returns the original wd
createDir = function(sub.dir, save.mode = TRUE) {
  temp.wd = getwd()

  # In Save mode folder must not exist and has to be writable
  assertPathForOutput(sub.dir, overwrite = !save.mode)
  if (file.exists(sub.dir)) {
    setwd(file.path(temp.wd, sub.dir))
  } else {
    dir.create(file.path(temp.wd, sub.dir))
    setwd(file.path(temp.wd, sub.dir))
  }
  return(temp.wd)
}

# creates a random generated Variable name.
reportId = function(length = 16) {
  collapse(sample(c(letters, LETTERS), size = length, replace = TRUE), sep = "")
}

# Takes an object and adds more attributes
addAttToObj = function(object, ...){
  att = list(...)
  for (i in names(object)) {
    att[[i]] = object[[i]]
  }
  setClasses(att, class(object))
}

# Takes an object and creates a new one with the same attributes plus "..."
makeS3Obj2 = function(classes, object, ...){
  result = addAttToObj(object, ...)
  class(result) = classes
  return(result)
}

# Wrapper for loading not already loaded librarys
rmdLibrary = function(needed.pkgs, file, force = FALSE){
  catf("library(%s)\n", needed.pkgs, file = file)
}

# Wrapper for loading data
rmdloadData = function(name, path, file){
  catf("%s = readRDS(\"%s.rds\")", name, path, file = file)
}

# Wrapper for writing lines
rmdWriteLines = function(vec, con){
  for (i in seq_along(vec)) {
    writeLines(vec[i], con = con)
  }
}
# Example: rmdWriteLines(letters, stdout())

# Collects needed packages
getPkgs = function(obj){
  unique(c(obj$needed.pkgs, obj$plot.code$needed.pkg))
}

# Saves an object and writes code to load it into the rmd file
saveLoadObj = function(obj, name, file){
  #save object
  obj.file.name = paste0(name, ".rds")
  if (file.exists(obj.file.name))
    stop(paste0(obj.file.name, " already exists! Please rename the *.report object in function call write*Report() "))
  saveRDS(obj, file = obj.file.name)
  #load object; x$var.id is needed so the plo
  rmdloadData(obj$report.id, name, file)
}


# Checks if a rmd file exists and if it exists then increase the index. It returns
# the first file that doesnt exist.
rmdName = function(name, index = 1L, max.depth = 100L) {
  rmd.file = paste0(name, index, ".rmd")
  if (file.exists(rmd.file) & index <= max.depth) {
    index = index + 1L
    rmdName(name, index, max.depth)
  } else {
    if (index > max.depth) warning(paste0("Too many rmd-Files: \"", rmd.file, "\"", " will be overwritten"))
    return(rmd.file)
  }
}
#splits a list with j sublists into ceiling(j / k) list with each maximal k list
#done for mulitplot. e.g plotlist consists of 11 sublists, then we plot maximal 9 plots
#into one page, hence: here j = 11 and k = 9 results into 2 pages where on
#the first page 9 plots and on the second the rest 2

splitGGplotList = function(mylist, k) {
  j = length(mylist)
  no.new.lists = ceiling(j / k)
  fact.vec = integer()
  for (v in seq_len(no.new.lists)) {
    tmp.vec = rep(v, times = k)
    fact.vec = c(fact.vec, tmp.vec)
  }
  fact.vec = fact.vec[1:j]
  out.list = split(mylist, fact.vec, use.names)
}


##wrapper for multiplot with further plotlist splitting
multiplotPages = function(list, k, no.cols, ...) {
  #create sublists which act as page
  splitted.list = splitGGplotList(list, k)
  for (page in seq_len(length(splitted.list))) {
    #call multiplot with further arguments
    multiplot(plotlist = splitted.list[[page]],
      cols = ifelse(length(splitted.list[[page]]) < no.cols, length(splitted.list[[page]]), no.cols), ...)
  }
}

#helper to open a r-code chunk block with options. Make sure to close the r-chunk with ''' at the end
#options is a list with chunk options: handles all kind of data types as it will be converted into character
#example: options = list(echo = FALSE, message = TRUE, ...)
writeRblock = function(options = list(echo = FALSE, message = FALSE)) {
  a = "```{r "
  b = paste(sprintf("%s = %s", names(x), x), collapse = ", ")
  c = paste0(a, b, "}")
}


writeRinline = function(r.code) {
  paste0("`r ", r.code, " `")
}


# S3 method to get id of an AEDA object
getId = function(x) UseMethod("getId")
getId.default = function(x){
  warning(paste0("getId does not know how to handle object of class \"",
    class(x), "\""))
}
getId.CorrReport = function(x){
  x$id
}

##new
getId.BasicReport = function(x) {
  x$report.id
}

getId.NumSumReport = function(x) {
  x$report.id
}

##
getType = function(x) UseMethod("getType")
getType.default = function(x){
  warning(paste0("getType does not know how to handle object of class \"",
    class(x), "\""))
}
getType.CorrReport = function(x){
  x$type
}
getType.NumSumReport = function(x){
  x$type
}
getType.BasicReport = function(x){
  x$type
}
