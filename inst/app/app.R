library(shiny)
library(shinydashboard)
library(DT)
library(dplyr)
library(shinyAce)
library(shinyBS)

ui <- dashboardPage(

  dashboardHeader(),
  dashboardSidebar(sidebarMenu(menuItem("Blocks", tabName = "blocks", icon = icon("th")))),
  dashboardBody(
    bsModal("aceNotes", "Edit project notes", trigger = "tmp",
            verbatimTextOutput("editFilePath"),
            uiOutput("aceEditorUI")

    ),

    tabItems(
      tabItem(tabName = "blocks",
              fluidRow(uiOutput("dashBoxes"))
      )
    )
  )
)

server <- function(input, output, session) {

  projAll = reactive({
    repos = unique(dirname(dir(.GLOBALPATH, pattern = "(*.Rproj)|(README.md)|(README.Rmd)|(NEWS.Rmd)|(NEWS.md)|(.notes)", recursive = TRUE, full.names = TRUE)))
  })

  namesAll = reactive({
    basename(projAll())
  })

  boxes = reactive({

    lastModTimeFrames = lapply(projAll(), project_last_modtime)

    modOrder = order(sapply(lastModTimeFrames, "[[",2), decreasing = TRUE)
    lastModTimeFrames = lastModTimeFrames[modOrder]

    nmAll = namesAll()[modOrder]
    prAll = projAll()[modOrder]

    boxes = mapply(function(nm, path, lastMod)
    {
      descr = project_description_file(path)

      tg = shinydashboard::box(title = sprintf("%s",nm), width = 4,
                          h3(sprintf("Last mod time: %s", lastMod[[2]])),
                          descr,
                          h3(sprintf("Path: %s",path), style = "word-break: break-all;"),
                          h4(sprintf("Last modified file: %s", lastMod[[1]]),style = "word-break: break-all;"),
                          h2("Notes:"),
                          verbatimTextOutput(paste0(nm,"NotesText")),
                          actionButton(paste0(nm,"Notes"), "Edit notes"),
                          project_description_full(path),
                          project_rmd_modal(path,"README"),
                          project_rmd_modal(path,"NEWS"))

      tg
    },nmAll,prAll,lastModTimeFrames, SIMPLIFY = FALSE)


    list(boxes = boxes, buttonIds = paste0(nmAll,"Notes"), paths = prAll)

  })

  output$dashBoxes = renderUI({

    boxes()$boxes
  })


  ButtonsLastVals = reactiveValues(vals = NULL, path = NULL, id = NULL)

  observe({


    vals = unlist(mapply(function(id, path)
    {
      notes = file.path((path),".notes")

      if(file.exists(notes))
      {
        txt = readLines(notes, warn = FALSE)
        txt = paste(txt, collapse = "\n")
        output[[paste0(id,"Text")]] = renderText({txt})
      }

      input[[id]]
    }, boxes()$buttonIds, boxes()$path))

    isolate({
      if(!is.null(ButtonsLastVals$vals))
      {
        pressed = which(ButtonsLastVals$vals != vals)
        ButtonsLastVals$path = file.path(((boxes()$path[[pressed]])),".notes")
        ButtonsLastVals$id   = paste0(boxes()$buttonIds[[pressed]],"Text")
        output$editFilePath = renderText({ ButtonsLastVals$path })

        if(file.exists(ButtonsLastVals$path))
        {
          txt = paste(readLines(ButtonsLastVals$path), collapse = "\n")

          output$aceEditorUI = renderUI({ aceEditor("notesEditor", fontSize = 14, height = "300px", wordWrap = TRUE, debounce = 500, value = txt) })

        } else
        {
          output$aceEditorUI = renderUI({ aceEditor("notesEditor", fontSize = 14, height = "300px", wordWrap = TRUE, debounce = 500, value = "") })
        }

        toggleModal(session, modalId = "aceNotes")
      }

    })


    ButtonsLastVals$vals = vals

  }, priority = 50)

  edited = reactiveValues(edited = NULL)
  observe({

    req(ButtonsLastVals$path)
    req(ButtonsLastVals$id)
    txt = input$notesEditor

    isolate({
    if(is.null(edited$edited) || edited$edited == ButtonsLastVals$id)
    {
      output[[ButtonsLastVals$id]] = renderText({ txt })

      cat(paste0(txt,"\n"), file = ButtonsLastVals$path)
    }})

    edited$edited = ButtonsLastVals$id


  })

}

shinyApp(ui, server)
