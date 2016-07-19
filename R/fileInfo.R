project_last_modtime = function(path)
{
  info = file.info(dir(path, full.names = TRUE, recursive = TRUE))
  last = tail(info[order(info$mtime),], 1)
  data_frame(Name = rownames(last), LastModificationTime = as.character(last$mtime))
}

project_description_file = function(path, asHTML = TRUE)
{
  descrPath = file.path(path,"DESCRIPTION")

  if(!file.exists(descrPath)) return(h3(""))

  descrContent = readLines(descrPath)

  content = descrContent[grep("Description:", descrContent)]

  h3(content)
}

project_description_full = function(path, asHTML = TRUE)
{
  descrPath = file.path(path,"DESCRIPTION")

  if(!file.exists(descrPath)) return(NULL)

  descrContent = readLines(descrPath)

  id = paste0("id",round(1000000*runif(1,0,100)))


  list(
  bsModal("md", "Description", lapply(descrContent, h3), trigger = id),
  actionButton(inputId = id, label = "Show description"))


}

project_rmd_modal = function(path, file = "README")
{
  descrPath = file.path(path, paste0(file,c(".md",".Rmd")))

  descrPath = descrPath[file.exists(descrPath)]


  if(length(descrPath) == 0) return(NULL)

  descrPath = descrPath[1]

  id = paste0("id",round(1000000*runif(1,0,100)))


  list(
    bsModal(paste0(id, "MD"), "Description", includeMarkdown(descrPath), trigger = id, size = "large"),
    actionButton(inputId = id, paste(label = "Show", file)))
}

runFinder = function(path = getwd())
{
  .GLOBALPATH <<- path
  app = system.file("app/app.R", package = "ProjectFinder")
  runApp(app = app, launch.browser = TRUE)
}

