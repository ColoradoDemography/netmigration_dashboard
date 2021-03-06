library(plotly)
library(shiny)

source("setup.R")  


function(req) {
  htmlTemplate("index.html",
               county=selectInput("county","Select a county:", choices = unique(county_choices$county), selected = 'Colorado'),
               net_mig_plot=plotlyOutput("netMigAge"),
               net_mig_dl=downloadButton('netMigAgeData', 'Download Data (CSV)'),
               net_share_plot=plotlyOutput("netMigAgeShare"),
               net_share_dl=downloadButton('netMigAgeDataShare', 'Download Data (CSV)')
               )
}

