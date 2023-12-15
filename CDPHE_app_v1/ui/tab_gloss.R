#Glossary stuff

tabPanel(value = "glossary tab", title = "glossary tab", 
br(), 
fluidRow(id = "gloss_row1", 
wellPanel(id = "gloss_well", width = "100%", 
      accordion(#title = "Group 1", 
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
        )),
      
      accordion(#title = "Group 2", 
                id = "accordion2",
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
        ))

      )), 
br(), 
fluidRow(id = "gloss_row2",
         actionBttn("refs", "References"))
) 