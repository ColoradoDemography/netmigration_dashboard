library(dplyr)
library(tidyr)
library(plotly)
library(scales)

options(shiny.sanitize.errors = TRUE)
county_choices=read.csv("county_names.csv", stringsAsFactors = FALSE)%>%
  select(county)

all_data= read.csv("NetMigrationByAgeComparison.csv", stringsAsFactors = FALSE)

#### Net Migration by Age Graph and Data #####

## Generates a Plotly Chart
mig_age_p=function(fips){
  
  data=all_data%>%    
    mutate(FIPS=as.numeric(FIPS))%>%
    filter(FIPS==fips)
  
  plot_ly(data, x=FiveYearAgeGroups, y=NetMig9500, line=list(color = "rgb(31,74,126)"), name="1995 to 2000")%>%
    add_trace(data=.,x=as.numeric(FiveYearAgeGroups), y=as.numeric(NetMig0010), line=list(color="rgb(0,149,58)", width=2.5, dash="dash"), name= "2000 to 2010")%>%
    add_trace(data=.,x=as.numeric(FiveYearAgeGroups), y=as.numeric(NetMig1020), line=list(color="rgb(239,117,33)", width=2.5, dash="dot"), name= "2010 to 2020")%>%
    layout(
      barmode="stacked",
      title="Net Migration by Age - Net Migrants",
      xaxis=list(
        title=" Age"),
      yaxis=list(
        title=" Net Migration"),
      margin=list(t=60)
    )

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
  
  plot_ly(data, x=FiveYearAgeGroups, y=percent(Share9500), line=list(color = "rgb(31,74,126)"), name="1995 to 2000")%>%
    add_trace(data=., x=FiveYearAgeGroups, y=percent(Share0010), line=list(color="rgb(0,149,58)", width=2.5, dash="dash"), name= "2000 to 2010")%>%
    add_trace(data=., x=FiveYearAgeGroups, y=percent(Share1020), line=list(color="rgb(239,117,33)", width=2.5, dash="dot"), name= "2010 to 2020")%>%
    layout(
      title="Net Migration by Age - Share",
      xaxis=list(
        title=" Age"),
      yaxis=list(
        title=" Net Migration Share (%)"),
      margin=list(t=60)
    )
  
}

## Generates the data download
mig_share_d=function(fips){
  
  x=all_data%>%
    mutate(FIPS=as.numeric(FIPS))%>%
    filter(FIPS==fips)%>%
    # bind_cols(data.frame(County=rep(name, 90)))%>%
    select(County=County.name, AgeGroup=FiveYearAgeGroups, Share9500, Share0010, Share1020)
  
  
  return(x)
}


