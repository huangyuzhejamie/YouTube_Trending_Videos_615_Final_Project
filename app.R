
library(shiny)
library("plotly", lib.loc="~/R/win-library/3.6")
library("shinyWidgets", lib.loc="~/R/win-library/3.6")
library("wordcloud", lib.loc="~/R/win-library/3.6")
library("wordcloud2", lib.loc="~/R/win-library/3.6")
library(SnowballC)
library(tokenizers)
library("tidytext", lib.loc="~/R/win-library/3.6")
library("textdata", lib.loc="~/R/win-library/3.6")
library(rjson)
library(jsonlite)
library(tidyverse)
library(RColorBrewer)
library(scales)
library("webshot", lib.loc="~/R/win-library/3.6")
library("htmlwidgets", lib.loc="~/R/win-library/3.6")

#Import data and data cleaning
us<-read.csv("US.csv",header = TRUE)
uscategory<-fromJSON("US_category_id.json")

ca<-read.csv("CA.csv",header = TRUE)
cacategory<-fromJSON("CA_category_id.json")

de<-read.csv("DE.csv",header = TRUE)
decategory<-fromJSON("DE_category_id.json")

fr<-read.csv("FR.csv",header = TRUE)
frcategory<-fromJSON("FR_category_id.json")

gb<-read.csv("GB.csv",header = TRUE)
gbcategory<-fromJSON("GB_category_id.json")

ind<-read.csv("IN.csv",header = TRUE)
indcategory<-fromJSON("IN_category_id.json")

jp<-read.csv("JP.csv",header = TRUE)
jpcategory<-fromJSON("JP_category_id.json")

kr<-read.csv("KR.csv",header = TRUE)
krcategory<-fromJSON("KR_category_id.json")

mx<-read.csv("MX.csv",header = TRUE)
mxcategory<-fromJSON("MX_category_id.json")

ru<-read.csv("RU.csv",header = TRUE)
rucategory<-fromJSON("RU_category_id.json")

category_id<-as.vector(uscategory$items$id)
category_name<-as.vector(uscategory$items$snippet$title)
uscategoryname<-as.data.frame(cbind(category_id,category_name))
uscategoryname$category_id<-as.numeric(levels(uscategoryname$category_id))[uscategoryname$category_id]
uss<-us%>%left_join(uscategoryname,by="category_id")

category_id<-as.vector(cacategory$items$id)
category_name<-as.vector(cacategory$items$snippet$title)
cacategoryname<-as.data.frame(cbind(category_id,category_name))
cacategoryname$category_id<-as.numeric(levels(cacategoryname$category_id))[cacategoryname$category_id]
cas<-ca%>%left_join(cacategoryname,by="category_id")
cas<-cas[,-c(9:34)]
cas<-cas[!is.na(cas$category_name),]


category_id<-as.vector(decategory$items$id)
category_name<-as.vector(decategory$items$snippet$title)
decategoryname<-as.data.frame(cbind(category_id,category_name))
decategoryname$category_id<-as.numeric(levels(decategoryname$category_id))[decategoryname$category_id]
des<-de%>%left_join(decategoryname,by="category_id")
des<-des[,-c(9:34)]


category_id<-as.vector(frcategory$items$id)
category_name<-as.vector(frcategory$items$snippet$title)
frcategoryname<-as.data.frame(cbind(category_id,category_name))
frcategoryname$category_id<-as.numeric(levels(frcategoryname$category_id))[frcategoryname$category_id]
fr$category_id<-as.numeric(fr$category_id)
frs<-fr%>%left_join(frcategoryname,by="category_id")
frs<-frs[!is.na(frs$category_name),]
frs<-frs[,-c(9:47)]


category_id<-as.vector(gbcategory$items$id)
category_name<-as.vector(gbcategory$items$snippet$title)
gbcategoryname<-as.data.frame(cbind(category_id,category_name))
gbcategoryname$category_id<-as.numeric(levels(gbcategoryname$category_id))[gbcategoryname$category_id]
gbs<-gb%>%left_join(gbcategoryname,by="category_id")
gbs<-gbs[,-c(9:34)]


category_id<-as.vector(indcategory$items$id)
category_name<-as.vector(indcategory$items$snippet$title)
indcategoryname<-as.data.frame(cbind(category_id,category_name))
indcategoryname$category_id<-as.numeric(levels(indcategoryname$category_id))[indcategoryname$category_id]
ind$category_id<-as.numeric(ind$category_id)
inds<-ind%>%left_join(indcategoryname,by="category_id")
inds<-inds[!is.na(inds$category_name),]
inds<-inds[,-c(9:26)]



category_id<-as.vector(jpcategory$items$id)
category_name<-as.vector(jpcategory$items$snippet$title)
jpcategoryname<-as.data.frame(cbind(category_id,category_name))
jpcategoryname$category_id<-as.numeric(levels(jpcategoryname$category_id))[jpcategoryname$category_id]
jp$category_id<-as.numeric(jp$category_id)
jps<-jp%>%left_join(jpcategoryname,by="category_id")


category_id<-as.vector(krcategory$items$id)
category_name<-as.vector(krcategory$items$snippet$title)
krcategoryname<-as.data.frame(cbind(category_id,category_name))
krcategoryname$category_id<-as.numeric(levels(krcategoryname$category_id))[krcategoryname$category_id]
kr$category_id<-as.numeric(kr$category_id)
krs<-kr%>%left_join(krcategoryname,by="category_id")
krs<-krs[,-c(9:41)]


category_id<-as.vector(mxcategory$items$id)
category_name<-as.vector(mxcategory$items$snippet$title)
mxcategoryname<-as.data.frame(cbind(category_id,category_name))
mxcategoryname$category_id<-as.numeric(levels(mxcategoryname$category_id))[mxcategoryname$category_id]
mx$category_id<-as.numeric(mx$category_id)
mxs<-mx%>%left_join(mxcategoryname,by="category_id")
mxs<-mxs[,-c(9:37)]


category_id<-as.vector(rucategory$items$id)
category_name<-as.vector(rucategory$items$snippet$title)
rucategoryname<-as.data.frame(cbind(category_id,category_name))
rucategoryname$category_id<-as.numeric(levels(rucategoryname$category_id))[rucategoryname$category_id]
ru$category_id<-as.numeric(ru$category_id)
rus<-ru%>%left_join(rucategoryname,by="category_id")
rus<-rus[,-c(9:37)]

country<-rep("United States",length(uss$video_id))
usss<-cbind(uss,country)
country<-rep("Canada",length(cas$video_id))
cass<-cbind(cas,country)
country<-rep("Germany",length(des$video_id))
dess<-cbind(des,country)
country<-rep("France",length(frs$video_id))
frss<-cbind(frs,country)
country<-rep("United Kingdom",length(gbs$video_id))
gbss<-cbind(gbs,country)
country<-rep("India",length(inds$video_id))
indss<-cbind(inds,country)
country<-rep("Japan",length(jps$video_id))
jpss<-cbind(jps,country)
country<-rep("Korea",length(krs$video_id))
krss<-cbind(krs,country)
country<-rep("Mexico",length(mxs$video_id))
mxss<-cbind(mxs,country)
country<-rep("Russia",length(rus$video_id))
russ<-cbind(rus,country)
total<-rbind(usss,cass,dess,frss,gbss,indss,jpss,krss,mxss,russ)
total<-total[!is.na(total$category_name),]
total_ca<-total%>%group_by(country,category_name)%>%summarise(views_count=sum(as.numeric(views)),likes_count=sum(as.numeric(likes)),comment_count_ca=sum(as.numeric(comment_count)),times=length(unique(video_id)))


#Define choice



#Define UI 

   
ui <- fluidPage(
  
  titlePanel("YouTube Trending Videos Exploration"),   
  sidebarPanel(
    # Select type of view pattern to plot
    selectInput("countryname", "Countryname", unique(total_ca$country))
                              
    
  ),
  mainPanel(
    h3(textOutput("caption")),
    plotlyOutput('Plots1', height = "500px",width = "500px"),
    plotlyOutput('Plots2', height = "500px",width = "500px"),
    plotlyOutput('Plots3', height = "500px",width = "500px"),
    plotlyOutput('Plots4', height = "500px",width = "500px")
   
   )
)

# Define server logic required to draw plots
server <- function(input, output) {
  formulaText <- reactive({
    input$countryname
  })

  output$caption <- renderText({
    formulaText()
  }) 
  output$Plots1 <- renderPlotly({
    
    total_caa=filter(total_ca,country==input$countryname)
    total_caa<-total_caa[order(total_caa$views_count,decreasing = TRUE),]
    total_caaa<-total_caa[1:5,]
    ggplot(total_caaa,aes(x=reorder(category_name,-views_count),y=views_count))+ geom_bar(stat = "identity",fill = brewer.pal(11, "RdBu")[2])+
      theme_bw()+theme(panel.grid=element_blank(),panel.border=element_blank(),axis.line=element_line(size=1,colour="black"))+theme(plot.title = element_text(hjust = 0.5))+theme(axis.text.x=element_text(size=7))+theme(axis.text.y = element_blank())+theme(axis.ticks.y = element_blank())+labs(title="Top 5 Views Category")+
      labs(x="Category", y="Views") 

    }) 
  output$Plots2 <- renderPlotly({
    
    total_caa=filter(total_ca,country==input$countryname)
    total_caa<-total_caa[order(total_caa$likes_count,decreasing = TRUE),]
    total_caaa<-total_caa[1:5,]
    ggplot(total_caaa,aes(x=reorder(category_name,-likes_count),y=likes_count))+ geom_bar(stat = "identity",fill = brewer.pal(11, "RdBu")[2])+
      theme_bw()+theme(panel.grid=element_blank(),panel.border=element_blank(),axis.line=element_line(size=1,colour="black"))+theme(plot.title = element_text(hjust = 0.5))+theme(axis.text.x=element_text(size=7))+theme(axis.text.y = element_blank())+theme(axis.ticks.y = element_blank())+labs(title="Top 5 Likes Category")+
      labs(x="Category", y="Likes") 
    
  }) 
  output$Plots3 <- renderPlotly({
    
    total_caa=filter(total_ca,country==input$countryname)
    total_caa<-total_caa[order(total_caa$comment_count_ca,decreasing = TRUE),]
    total_caaa<-total_caa[1:5,]
    ggplot(total_caaa,aes(x=reorder(category_name,-comment_count_ca),y=views_count))+ geom_bar(stat = "identity",fill = brewer.pal(11, "RdBu")[2])+
      theme_bw()+theme(panel.grid=element_blank(),panel.border=element_blank(),axis.line=element_line(size=1,colour="black"))+theme(plot.title = element_text(hjust = 0.5))+theme(axis.text.x=element_text(size=7))+theme(axis.text.y = element_blank())+theme(axis.ticks.y = element_blank())+labs(title="Top 5 Commented Category")+
      labs(x="Category", y="Comments") 
    
  })     
  output$Plots4 <- renderPlotly({
    
    total_caa=filter(total_ca,country==input$countryname)
    total_caa<-total_caa[order(total_caa$times,decreasing = TRUE),]
    total_caaa<-total_caa[1:5,]
    ggplot(total_caaa,aes(x=reorder(category_name,-times),y=times))+ geom_bar(stat = "identity",fill = brewer.pal(11, "RdBu")[2])+
      theme_bw()+theme(panel.grid=element_blank(),panel.border=element_blank(),axis.line=element_line(size=1,colour="black"))+theme(plot.title = element_text(hjust = 0.5))+theme(axis.text.x=element_text(size=7))+theme(axis.text.y = element_blank())+theme(axis.ticks.y = element_blank())+labs(title="Top 5 Trending Times Category")+
      labs(x="Category", y="Trending Times") 
    
  }) 
    
 
}

# Run the application 
shinyApp(ui = ui, server = server)

