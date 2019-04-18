

#install.packages("ggplot2")
#install.packages("dplyr")
#install.packages("shiny")
#install.packages("shinythemes")

library(ggplot2)
library(dplyr)

t<-rnorm(100)


betas<-c(1.5,1.4,1.2,1.1,1.0,0.9,0.8,0.7,0.6,0.5,0.4,0.3,0.2,0.1,0.05)

k<-c(1:100)

for(x in 1:15){

y<-c()
y[1]=0

for(i in 2:100){

j=i-1

y[i]=betas[x]*y[j]+t[i]

}

w<-data.frame(beta=betas[x], tempo=k, valor=y)

if(x ==1){

mydata<-w

}
if(x !=1){

mydata<-rbind(mydata,w)

}


}

View(mydata)

attach(mydata)

plot(tempo[beta==0.05],valor[beta==0.05])



attach(mydata)

library(shiny)
library(shinythemes)

ui <- fluidPage(theme=shinytheme("lumen"),
titlePanel("AutoRegressivos"),
sidebarLayout(
sidebarPanel(

selectInput(inputId = "beta", label=strong(expression(paste(beta))),
choices=unique(betas),selected=0.5)),

mainPanel(plotOutput(outputId = "lineplot", height = "300px"))

)
)

server <- function(input,output) {


output$lineplot <-renderPlot({

ggplot(data=subset(mydata, beta %in% input$beta),aes(x=tempo,y=valor))+geom_line(size=1)+geom_point(size=2)
})

}


shinyApp(ui = ui, server =server)


