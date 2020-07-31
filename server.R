library(dplyr)
library(tidyr)
library(plotly)
library(scales)


county_choices=read.csv("county_names.csv", stringsAsFactors = FALSE)%>%
  select(county)

all_data= read.csv("NetMigrationByAgeComparison.csv", stringsAsFactors = FALSE)

#### Net Migration by Age Graph and Data #####

## Generates a Plotly Chart
mig_age_p=function(fips){
  
  data=all_data%>%    
    mutate(FIPS=as.numeric(FIPS))%>%
    filter(FIPS==fips)
  
  p=plot_ly(data=data, x= ~FiveYearAgeGroups, y=~NetMig9500, type="scatter", line=list(color = "rgb(31,74,126)"), name="1995 to 2000")
    p=add_trace(p, x=~FiveYearAgeGroups, y=~NetMig0010, line=list(color="rgb(239,117,33)", width=2.5, dash="dash"), name= "2000 to 2010")
    p=add_trace(p,x=~FiveYearAgeGroups, y=~NetMig1020, line=list(color="rgb(0,149,58)", width=2.5, dash="dot"), name= "2010 to 2020")
    p=layout(p,
      barmode="stacked",
      title="Net Migration by Age - Net Migrants",
      xaxis=list(
        title=" Age"),
      yaxis=list(
        title=" Net Migration"),
      margin=list(t=60)
    )
  p
}

## Generates the data download
mig_age_d=function(fips){
  
  x=all_data%>%
    mutate(FIPS=as.numeric(FIPS))%>%
    filter(FIPS==fips)%>%
    # bind_cols(data.frame(County=rep(name, 90)))%>%
    select(County=County.name, AgeGroup=FiveYearAgeGroups, NetMig9500, NetMig0010, NetMig1020)
  
  
  return(x)
}

#### Share of Net Migration by Age Graph and Data #####

## Generates a Plotly Chart
mig_share_p=function(fips){
  
  data=all_data%>%    
    mutate(FIPS=as.numeric(FIPS))%>%
    filter(FIPS==fips)
  
  p= plot_ly(data=data, x=~FiveYearAgeGroups, y=~1000*as.numeric(Rate9500), type="scatter",line=list(color = "rgb(31,74,126)"), name="1995 to 2000")
  p=  add_trace(p, x=~FiveYearAgeGroups, y=~1000*as.numeric(Rate0010),  line=list(color="rgb(239,117,33)", width=2.5, dash="dash"), name= "2000 to 2010")
  p=  add_trace(p, x=~FiveYearAgeGroups, y=~1000*as.numeric(Rate1020),  line=list(color="rgb(0,149,58)", width=2.5, dash="dot"), name= "2010 to 2020")
  p=  layout(p,
      title="Net Migration by Age - Rates", 
      xaxis=list(
        title=" Age"),
      yaxis=list(
        title=" Net Migration Rate (per 1,000 population)"),
      margin=list(t=60)
    )
  p
}

## Generates the data download
mig_share_d=function(fips){
  
  x=all_data%>%
    mutate(FIPS=as.numeric(FIPS))%>%
    filter(FIPS==fips)%>%
    # bind_cols(data.frame(County=rep(name, 90)))%>%
    select(County=County.name, AgeGroup=FiveYearAgeGroups, Rate9500, Rate0010, Rate1020)
  
  
  return(x)
}

# 
# source("setup.R")

function(input, output, session) {

county=reactive({filter(read.csv("county_names.csv"), county==input$county)%>%
                  select(countyfips)%>%
    as.numeric()})

### Net Migration by Age ####
  
  output$netMigAge=renderPlotly({mig_age_p(county())})
  
  nm_data=reactive({mig_age_d(county())})
  
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

nm_share_data=reactive({mig_share_d(county())})

output$netMigAgeDataShare=downloadHandler(
  filename= function(){
    paste(unique(nm_share_data()$County), "Net Migration by Age.csv", sep="_")
  },
  content= function(file){
    write.csv(nm_share_data(), file, row.names=FALSE)
  }
  
)

}
