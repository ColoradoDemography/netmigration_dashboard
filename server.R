
library(plotly)

source("setup.R")

function(input, output, session) {

county=reactive({filter(read.csv("county_names.csv"), county==input$county)%>%
                  select(countyfips)%>%
    as.numeric()})

### Net Migration by Age ####
  
  output$netMigAge=renderPlotly({mig_age_p(county())})
  
  nm_data=reactive({mig_age_d(county(), input$county)})
  
  output$netMigAgeData=downloadHandler(
    filename= function(){
      paste(unique(nm_data()$County), "Net Migration by Age.csv", sep="_")
    },
    content= function(file){
      write.csv(nm_data(), file, row.names=FALSE)
    }
    
  )
  


### Share of Net Migration by Age ####

output$netMigAgeShare=renderPlotly({mig_share_p(county())})

nm__share_data=reactive({mig_share_d(county(), input$county)})

output$netMigAgeDataShare=downloadHandler(
  filename= function(){
    paste(unique(nm_share_data()$County), "Net Migration by Age.csv", sep="_")
  },
  content= function(file){
    write.csv(nm_share_data(), file, row.names=FALSE)
  }
  
)

}