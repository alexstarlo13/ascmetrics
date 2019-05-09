#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#



library(shiny)
library(ggplot2)  # for the diamonds dataset
library(readxl)
library(tidyverse)
if (!require("DT")) install.packages("DT")
library(DT)
library(scales)
laleveldata <- read_excel("/Users/AlexStarling/Documents/rwork/laleveldata.xlsx")

laleveldata$Age65plus<-percent(laleveldata$Age65plus)
laleveldata$Age85plus<-percent(laleveldata$Age85plus)
laleveldata$requestLTC18<-percent(laleveldata$requestLTC18)
laleveldata$requestLTC65<-percent(laleveldata$requestLTC65)
laleveldata$percentchangesince2010<-percent(laleveldata$percentchangesince2010)
laleveldata$spendvsrnf<-percent(laleveldata$spendvsrnf)
laleveldata$resandnursingpercentexpend65plus<-percent(laleveldata$resandnursingpercentexpend65plus)
laleveldata$resandnursingpercentexpend18plus<-percent(laleveldata$resandnursingpercentexpend18plus)
laleveldata$carehomepercentgood<-percent(laleveldata$carehomepercentgood)
laleveldata$domcarepercentgood<-percent(laleveldata$domcarepercentgood)
laleveldata$carehomebedspercentchange5year<-percent(laleveldata$carehomebedspercentchange5year)
laleveldata$domcarelocationspercentchange5year<-percent(laleveldata$domcarelocationspercentchange5year)
laleveldata$workerturnover<-percent(laleveldata$workerturnover)
laleveldata$vacancyrate<-percent(laleveldata$vacancyrate)
laleveldata$reserves<-percent(laleveldata$reserves)
laleveldata$percentdtoc<-percent(laleveldata$percentdtoc)
laleveldata$nonbritishworkers<-percent(laleveldata$nonbritishworkers)
laleveldata$changeincouncilspend<-percent(laleveldata$changeincouncilspend)


la <- as.data.frame(t(laleveldata[,-1]))

colnames(la) <- laleveldata$LA

rownames(la) <- c("ONS Code", "Region","Demographics", "Population" , "Adult Population (18+)","Population aged 85 and over (%)", "Population aged 65 and over (%)", "Life Expectancy: Male","Life Expectancy: Female",
"Long-term Support","Total requests for support, 65+ (per 100,000 pop)","Total requests for support, 18-64 (per 100,000 pop)","Requests that end in Long Term Care 18-64","Requests that end in Long Term Care 65+","Expenditure", "Total Net Current Expenditure (NCE), thousands",
"% change in adult social care expenditure since 2010/11","Net Current Expenditure per 100,000 adults, thousands"
, "Net Current Expenditure per User", "Spend versus RNF","Residential & nursing care as % of total gross expenditure, 65+
","Residential & nursing care as % of total gross expenditure, 18-64","Markets - Last updated 1st of April 2019","Care Homes, % Good/Outstanding
","Dom Care Agencies, % Good/Outstanding","Number of different home care providers ", "Number of different care home providers
","Number of care home beds","% change in past 5 years, care home beds",
"% change in past 5 years, home care agencies","Unit Costs",
"External Dom Care cost per hour",
"Residential and Nursing Unit Cost per week",
"Residential Care unit Cost per week","Workforce",
"Direct Care Employees, Independent","Turnover Rate, Direct Care only",
"Vacancy Rate, Direct Care only",
"Hourly pay, Independent Direct Care workforce",
"% of Direct Care workforce from EEA (non-British)","Finance & Performance",
"% change in council spending powers since 2010/11",
"Council reserves as % of spend",
"DToC from hospital to ASC: Attributable to social care services per 100,000 pop",
"% of Total DToC attributable to social care"
)

ui <- fluidPage(
  titlePanel("Adult Social Care Metrics for LA Visits"),
  title = "Adult Social Care Metrics for LA Visits",
  sidebarLayout(
    sidebarPanel(
      conditionalPanel(
        'input.dataset === "la"',
        selectizeInput(
          'e2', 'Select LA or Region', choices = list(
            National = c('National'),
            Region = c('North East', 'North West','Yorkshire and The Humber',
                       'East Midlands',
                       'West Midlands',
                       'East of England',
                       'London',
                       'South East',
                       'South West'),
            North.East = c('Northumberland','Gateshead',
                           'Newcastle upon Tyne',
                           'North Tyneside',
                           'South Tyneside',
                           'Sunderland',
                           'Hartlepool',
                           'Middlesbrough',
                           'Redcar and Cleveland',
                           'Stockton-on-Tees',
                           'County Durham',
                           'Darlington'),
            North.West = c('Cumbria',
'Bolton',
                           'Bury',
                           'Manchester',
                           'Oldham',
                           'Rochdale',
                           'Salford',
                           'Stockport',
                           'Tameside',
                           'Trafford',
                           'Wigan',
                           'Knowsley',
                           'Liverpool',
                           'Sefton',
                           'St. Helens',
                           'Wirral',
                           'Halton',
                           'Warrington',
                           'Lancashire',
                           'Blackburn with Darwen',
                           'Blackpool',
                           'Cheshire East',
                           'Cheshire West and Chester'),
            Yorkshire.and.the.Humber =c('Barnsley',
                                        'Doncaster',
                                        'Rotherham',
                                        'Sheffield',
                                        'Bradford',
                                        'Calderdale',
                                        'Kirklees',
                                        'Leeds',
                                        'Wakefield',
                                        'East Riding of Yorkshire',
                                        'Kingston upon Hull, City of',
                                        'North East Lincolnshire',
                                        'North Lincolnshire',
                                        'North Yorkshire',
                                        'York'),
            East.Midlands =c('Lincolnshire',
                             'Northamptonshire',
                             'Derbyshire',
                             'Derby',
                             'Leicestershire',
                             'Leicester',
                             'Rutland',
                             'Nottinghamshire',
                             'Nottingham'),
            West.Midlands =c('Warwickshire',
                             'Birmingham',
                             'Coventry',
                             'Dudley',
                             'Sandwell',
                             'Solihull',
                             'Walsall',
                             'Wolverhampton',
                             'Staffordshire',
                             'Stoke-on-Trent',
                             'Herefordshire, County of',
                             'Worcestershire',
                             'Shropshire',
                             'Telford and Wrekin'),
            East.of.England =c('Hertfordshire',
                               'Norfolk',
                               'Suffolk',
                               'Luton',
                               'Essex',
                               'Southend-on-Sea',
                               'Thurrock',
                               'Cambridgeshire',
                               'Peterborough',
                               'Bedford',
                               'Central Bedfordshire' ),
            London =c('Camden', 'Greenwich',
                      'Hackney',
                      'Hammersmith and Fulham',
                      'Islington',
                      'Kensington and Chelsea',
                      'Lambeth',
                      'Lewisham',
                      'Southwark',
                      'Tower Hamlets',
                      'Wandsworth',
                      'Westminster',
                      'Barking and Dagenham',
                      'Barnet',
                      'Bexley',
                      'Brent',
                      'Bromley',
                      'Croydon',
                      'Ealing',
                      'Enfield',
                      'Haringey',
                      'Harrow',
                      'Havering',
                      'Hillingdon',
                      'Hounslow',
                      'Kingston upon Thames',
                      'Merton',
                      'Newham',
                      'Redbridge',
                      'Richmond upon Thames',
                      'Sutton',
                      'Waltham Forest' ),
            South.East =c('Oxfordshire',
                      'Buckinghamshire',
                      'Milton Keynes',
                      'Bracknell Forest',
                      'West Berkshire',
                      'Reading',
                      'Slough',
                      'Windsor and Maidenhead',
                      'Wokingham',
                      'Isle of Wight',
                      'Surrey',
                      'West Sussex',
                      'Hampshire',
                      'Portsmouth',
                      'Southampton',
                      'East Sussex',
                      'Brighton and Hove',
                      'Kent',
                      'Medway'),
            South.West =c('Dorset',
                      'Bournemouth',
                      'Poole',
                      'Wiltshire',
                      'Swindon',
                      'Cornwall',
                      'Gloucestershire',
                      'Somerset',
                      'Bath and North East Somerset',
                      'Bristol, City of',
                      'North Somerset',
                      'South Gloucestershire',
                      'Devon',
                      'Plymouth',
                      'Torbay')

            
      
          ), multiple = TRUE
        ),
helpText("Note: National and Regional figures are all averages.",
         "To print whole table - change entries from 10 to 100 and then print page.")  )
        
    ),
    mainPanel(
      tabsetPanel(
        id = 'dataset',
        tabPanel("la", DT::dataTableOutput("mytable1"))
 
      )
    )
  )
)

server <- function(input, output) {
  
  # choose columns to display

  output$mytable1 <- DT::renderDataTable({
    DT::datatable(la[, input$e2, drop = FALSE])
  })
  

  
}


shinyApp(ui, server)

