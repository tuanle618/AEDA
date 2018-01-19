# if a Sb Directory doesn't exist it Creates one (in the working directory) and moves
# the wd to the sub directory. It returns the original wd
createDir = function(sub.dir, save.mode = TRUE) {
  temp.wd = getwd()
  if (file.exists(sub.dir) & save.mode) {
    stop(paste0("Directory: \"", sub.dir, "\" already exists. Stopping to ensure no Data is lost."))
  } else if (file.exists(sub.dir)) {
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
  saveRDS(obj, file = obj.file.name)
  #load object; x$var.id is needed so the plo
  rmdloadData(obj$report.id, name, file)
}

#splits a list with j sublists into ceiling(j / k) list with each maximal k list
#done for mulitplot. e.g plotlist consists of 11 sublists, then we plot maximal 9 plots
#into one page, hence: here j = 11 and k = 9 results into 2 pages where on
#the first page 9 plots and on the second the rest 2

splitList = function(mylist, k) {
  j = length(mylist)
  no.new.lists = ceiling(j / k)
  out.list = vector("list", length = no.new.lists)
  for (i in 0:(no.new.lists - 1)) {
    for (j in 1:k) {
      name = paste0("sublist ", i + 1)
      #how to handle subscript out of bounds
      a = tryCatch({
        mylist[[i * j + j]]
      }, error = function(e){
        NULL
      }
        )
      out.list[[i + 1]][j] = list(name = a)
    }
  }
  return(out.list)
}
