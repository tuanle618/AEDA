# if a Sb Directory doesn't exist it Creates one (in the working directory) and moves
# the wd to the sub directory. It returns the original wd

createDir = function(sub.dir) {
  temp.wd = getwd()
  if (file.exists(sub.dir)){
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
  for(i in names(object)){
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
