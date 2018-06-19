library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(readxl)
library(DT)

##IPL Dataset


setwd("C://Users//Administrator//Desktop//tableu")

ball_by_ball<-read_excel("Ball_by_Ball.xlsx")
match<-read_excel("Match.xlsx")
player<-read_excel("Player.xlsx")
player_match<-read_excel("Player_Match.xlsx")
season<-read_excel("Season.xlsx")
team<-read_excel("Team.xlsx")


## Lets merge few dataset
ball_by_ball_new<-merge(x=ball_by_ball,y=player,by.x="Striker_Id",
                        by.y = "Player_Id",all.x = TRUE)

ball_by_ball_new1<-merge(x=ball_by_ball_new,y=season,by.x = "Session_Id",
                         by.y = "Season_Id",all.x = TRUE)

ball_by_ball_final<-merge(x=ball_by_ball_new1,y=team,by.x = "Team_Batting_Id",
                          by.y = "Team_Id",all.x = TRUE)


## Header of the dashboard 

header <- dashboardHeader(title="IPL Dashboard")

sidebar<-dashboardSidebar(selectInput(inputId="Year",
                                      label = "select the year",
                                      choices = c("All",unique(ball_by_ball_final$Season_Year)),selected = "All"),
                          sidebarMenu(
                            menuItem("Dashboard",tabName = "KPI",icon = icon("Dashboard")),
                            menuItem("Data",tabName = "Row_Data"),
                            menuItem("Visit Us",href = "http://google.com")
                            ))

## Body for the dashboard

body = dashboardBody(
  tabItems(
    tabItem(tabName = "KPI",
            fluidRow(
              valueBoxOutput("value1"),
              valueBoxOutput("value2"),
              valueBoxOutput("value3")),
            
            # Fluid row for two charts
            fluidRow(
              box(title = "Top 10 batsman",width = 6,collapsible = TRUE,plotOutput("TOP10")),
              box(title = "Top 10 6-hitter",width = 6,collapsible = TRUE,plotOutput("TOP6s")))),
    
     # This is for new tab
    tabItem(tabName = "Row_Data",
            fluidRow(
              box(title = "Row Data", width = 12, collapsible = TRUE, dataTableOutput("RD"))))
            )
)



## Lets create a UI

UI <- dashboardPage(header = header,sidebar = sidebar,body = body)


####################################################################

server = function(input,output) {
  output$value1=renderValueBox({
  data = {
    if (input$Year == "All"){
      data1 = filter(ball_by_ball_final)
    }else{
      data1 = filter(ball_by_ball_final, Season_Year== input$Year)
    }
    data1
  }
  
  valueBox(length(unique(data$Match_Id)),"Number of Matches")
  
  })
  
  #No. of 6s
  
  output$value2<-renderValueBox({
    data = {
      if (input$Year == "All"){
        data1 = filter(ball_by_ball_final,Batsman_Scored=="6")
      }else{
        data1 = filter(ball_by_ball_final, Season_Year== input$Year & Batsman_Scored=="6")
      }
      data1
    }
    Most_Six<- data %>% group_by(Team_Short_Code) %>% summarise(Six=n())
    Team_with_Moat_six<- Most_Six %>% filter(Six== max(Six))
    valueBox(sum(Most_Six$Six),paste("Team with Max 6s:",Team_with_Moat_six$Team_Short_Code))
    
  })
  
  #No. of fifties
  
  output$value3 = renderValueBox({
    data <- {
      if(input$Year == 'All'){
        data1 = ball_by_ball_final %>% group_by(Match_Id,Team_Short_Code,Player_Name) %>% 
          summarise(Runs = sum(as.numeric(Batsman_Scored)))
        data1 = data1 %>% filter(Runs > 49 & Runs < 100)
      } else{
        data1 = ball_by_ball_final %>% filter(Season_Year == input$Year) %>% 
          group_by(Match_Id,Team_Short_Code,Player_Name) %>% 
          summarise(Runs = sum(as.numeric(Batsman_Scored)))
        data1 = data1 %>% filter(Runs > 49 & Runs < 100)
      }
      data1
    }
    player <- data1 %>% group_by(Player_Name) %>% summarise(Count = n()) %>% arrange(-Count) %>% 
      head(1) %>% select(Player_Name)
    valueBox(nrow(data),paste("Total Number of Fifties:",player$Player_Name))
  })
  
  # Top ten Batsman Based on Total Run
  
  output$TOP10<- renderPlot({
    data<-{
      if(input$Year == "All"){
        data1<-ball_by_ball_final
      }else{
          data1<- filter(ball_by_ball_final,Season_Year == input_Year)
      }
      data1
    }
    Top_10 = data %>% group_by(Player_Name) %>% 
      summarise(Total_Runs = sum(as.numeric(Batsman_Scored))) %>% arrange( -Total_Runs)%>%
      head(10)
    
    ggplot(Top_10,aes(x=reorder(Player_Name,-Total_Runs),y=Total_Runs))+
      geom_bar(stat="Identity",width=.5,fill="#4CA64C") + theme_bw()+
      geom_text(aes(label=Total_Runs),vjust=-0.25)+
      theme(axis.text.x = element_text(angle = 90)) +
      labs(y="Runs",x=" ",caption="Data Source : data.world")+
      theme(axis.text.y = element_blank(),axis.ticks.y = element_blank())
  })
  
}


## Create my APP
shinyApp(UI,server)


                              
                          