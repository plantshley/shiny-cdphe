#Glossary stuff

tabPanel(value = "glossary tab", title = "glossary tab", 
br(), 
fluidRow(id = "gloss_row1", 
wellPanel(id = "gloss_well", width = "100%", 
      accordion(
        id = "accordion1",
        accordionItem(
          title = "Term 1",
          collapsed = TRUE,
          "This is some text!"
        ),
        
        accordionItem(
          title = "Term 2",
          collapsed = TRUE,
          "This is some text!"
        ), 
        
        accordionItem(
          title = "Term 3",
          collapsed = TRUE,
          "This is some text!"
        ))))
) 