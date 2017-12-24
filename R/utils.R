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
