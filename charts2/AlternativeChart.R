library(shiny)
library(shinydashboard)
library(ggplot2)
library(hexbin)
library(gridExtra)
library(grid)
library(plotly)
library(ggtips)

source("players_data.R")
xplayers <- read.csv("xplys2.csv")


# Define UI ----
ui <- fluidPage(
    titlePanel("NBA Players Shot Chart"),
    
    
    fluidRow(
        
        
        column(3,
               wellPanel(
                   
                   
                   br(),
                   
                   uiOutput("player_photo"),
                   
                   
                   selectizeInput(inputId = "whatseason",
                                  label = "What Season?",
                                  choices = list( "2018/2019" = 1, "2017/2018" = 2, "2016/2017" = 3, "2015/2016" = 4,
                                                 "2014/2015" = 5, "2013/2014" = 6, "2012/2013" = 7, "2011/2012" = 8,
                                                 "2010/2011" = 9, "2009/2010" = 10),selected = 1),
                   selectizeInput(inputId = "player_name",
                                  label = "Player",
                                  choices = c("Enter a player..." = "", xplayers),
                                  selected = "All" ),
                   
                   radioButtons("radio", label = h3("Filter"),
                                choices = list("Total Shots" = 1, "Made Shots" = 2), 
                                selected = 1)),
               
               submitButton("Submit")
               
               
               
               
               
               
        ),
        
      
        
        column( 3, plotOutput("plot1")),
        
    
        column( 3, plotOutput("plot3"))),
    
    fluidRow(
        
        br(),br(),br(),br(),br(),
        
        radioButtons("new", label = h3("Do you want to add a new player?"),
                     choices = list("Yes" = 1, "No" = 2), 
                     selected = 2),
        submitButton("Add"),
        
        conditionalPanel(
            
            condition = "input.new == 1 ",
            
            br(), br(), br(),
            
            
            
            column(3,
                   
                   wellPanel(
                       
                       
                       selectizeInput(inputId = "whatseason2",
                                      label = "What Season?",
                                      choices = list("2018/2019" = 1, "2017/2018" = 2, "2016/2017" = 3, "2015/2016" = 4,
                                                     "2014/2015" = 5, "2013/2014" = 6, "2012/2013" = 7, "2011/2012" = 8,
                                                     "2010/2011" = 9, "2009/2010" = 10),selected = 1),
                       selectizeInput(inputId = "player_name2",
                                      label = "Compare to:",
                                      choices = c("Enter a player..." = "", xplayers),
                                      selected = "All"
                       ),  
                       radioButtons("radio2", label = h3("Filter"),
                                    choices = list("Total Shots" = 1, "Made Shots" = 2), 
                                    selected = 1)),
                   submitButton("Submit")
                   
            ),
            
            br(), 
            
            column(3,  plotOutput("plot2")),
            column( 3, plotOutput("plot4"))),
    )
)


#Player Function
player_chart <- function(name, whatseason) {
    
    
    zz <- subset(whatseason, player == name)
    plot<-  halfP + geom_hex(data = zz, aes(x =shot_x ,
                                            y =shot_y,
                                            fill = cut(..count.., c(
                                                0,1,2, 5, 10, Inf))),
                             colour = "lightblue",  binwidth = 1, alpha = 0.75) +
        scale_fill_manual(values = c("grey98", "slategray3", "yellow", "red" , "black"), 
                          labels = c("1","2","2-5","5-10","11+"), name = "Count")+
        ggtitle(paste(name,"Total Shots"))
    return(plot)
}

#Player Histogram Function
player_histogram <- function(name, whatseason) {
    
    
    zz <- subset(whatseason, player == name)
    plot<-  ggplot()+ 
        geom_freqpoly(data = whatseason, binwidth=1,mapping = aes(x = shot_distance, y = 100*..count../sum(..count..),
                                                            color = "Legue Average"), alpha = 0.4, size = 0.8) +
        geom_freqpoly(data = zz ,
                      aes(x = shot_distance, y =100*..count../sum(..count..),
                          color = "Player"), alpha = 1, size = 1.2,) +theme_bw() +xlim(-1,30) + ggtitle("Shot Slection")+
        scale_color_manual("", values = c("steelblue2",  "red4")) + xlab("Shot Distance") +ylab("Frequency %")+
        geom_vline(aes(xintercept = 23.9),  linetype = "dashed",size = 0.9, colour = "gray69")+
        geom_text(mapping = aes(x = 23.5, y = 14, label = "Arch 3pt", hjust = 0.5, vjust = -0.5))+
        geom_vline(aes(xintercept = 22),  linetype = "dashed",size = 0.9, colour = "gray69")+
        geom_text(mapping = aes(x = 21.8, y =15, label = "Corner 3pt", hjust = 0.5, vjust = 0.5))
    return(plot)
}

#Whole season chart functiom 
generate_chart = function(season) {
    plot <-halfP + geom_hex(data = season, aes(x =shot_x ,
                                        y =shot_y,
                                        fill = cut(..count.., c(
                                            0,5,25, 50, 100, Inf))),
                     colour = "lightblue",  binwidth = 1, alpha = 0.75)+
        scale_fill_manual(values = c("grey98", "slategray3", "yellow", "red" , "black"), 
                          labels = c("0-5","5-25","25-50","50-100","100+"), name = "Count")+
        ggtitle(" Shot Chart ") 
    return(plot)
}

#Whole season histogram
season_histogram <- function(whatseason) {
    
    plot<-  ggplot()+ 
        geom_freqpoly(data = whatseason,binwidth=1, mapping = aes(x = shot_distance, y = 100*..count../sum(..count..),
                                                       color = "Average"), alpha = 1, size = 1.2) +
        theme_bw() +xlim(-1,30) + ggtitle("Shot Slection")+
        scale_color_manual("", values = c("steelblue2")) + xlab("Shot Distance") +ylab("Frequency %")+
        geom_vline(aes(xintercept = 23.9),  linetype = "dashed",size = 0.9, colour = "gray69")+
        geom_text(mapping = aes(x = 23.5, y = 12, label = "Arch 3pt", hjust = 0.5, vjust = -1))+
        geom_vline(aes(xintercept = 22),  linetype = "dashed",size = 0.9, colour = "gray69")+
        geom_text(mapping = aes(x = 21.8, y =12, label = "Corner 3pt", hjust = 0.5, vjust = 1))
    
    return(plot)
}
#Read seasons data all 10 seasons
z18_19_02 <- read.csv("upload1819.csv")  #2018-2019 total
z18_19_made_02 <- read.csv("upload1819made.csv")  #2018-2019 made

z17_18_02 <- read.csv("upload1718.csv")  #17-18 total
z17_18_made_02 <- read.csv("upload1718made.csv")  #17-18 made

z16_17_02 <- read.csv("upload1617.csv")  #17-18 total
z16_17_made_02 <- read.csv("upload1617made.csv")  #17-18 made

z15_16_02 <- read.csv("upload1516.csv")  #17-18 total
z15_16_made_02 <- read.csv("upload1516made.csv")  #17-18 made

z14_15_02 <- read.csv("upload1415.csv")  #17-18 total
z14_15_made_02 <- read.csv("upload1415made.csv")  #17-18 made

z13_14_02 <- read.csv("upload1314.csv")  #17-18 total
z13_14_made_02 <- read.csv("upload1314made.csv")  #17-18 made

z12_13_02 <- read.csv("upload1213.csv")  #17-18 total
z12_13_made_02 <- read.csv("upload1213made.csv")  #17-18 made

z11_12_02 <- read.csv("upload1112.csv")  #17-18 total
z11_12_made_02 <- read.csv("upload1112made.csv")  #17-18 made

z10_11_02 <- read.csv("upload1011.csv")  #17-18 total
z10_11_made_02 <- read.csv("upload1011made.csv")  #17-18 made

z09_10_02 <- read.csv("upload0910.csv")  #17-18 total
z09_10_made_02 <- read.csv("upload0910made.csv")  #17-18 made


#############################

##    BEGIN COURT CODE #####


############################

# Need a function to draw circle
circle_fun <- function(center=c(0,0), diameter=1, npoints=500, start=0, end=2){
    tt <- seq(start*pi, end*pi, length.out=npoints)
    data.frame(
        x = center[1] + diameter / 2 * cos(tt),
        y = center[2] + diameter / 2 * sin(tt)
    )
}

# Need symmetry
rev_y <- function(y) 94-y

# Create data frame containing coordinates of polygons
new_coords <- function(x, y, group, descri){
    new_coords_df <- data.frame(x = x, y = y)
    new_coords_df$group <- group
    new_coords_df$side <- 1
    group <- group + 1
    
    # The same thing for the opposite side
    new_coords_df2 <- data.frame(x = x, y = rev_y(y))
    new_coords_df2$group <- group
    new_coords_df2$side <- 2
    group <<- group + 1
    
    # On reunit les donnees
    new_coords_df <- rbind(new_coords_df, new_coords_df2)
    new_coords_df$descri <- descri
    
    return(new_coords_df)
}

#Circles we need 
# Restricted area
cercle_np_out <- circle_fun(center = c(25,5+3/12), diameter = (4+1/6)*2)
cercle_np_in <- circle_fun(center = c(25,5+3/12), diameter = 4*2)
# Three point
cercle_3pts_out <- circle_fun(center = c(25,5+3/12), diameter = (23+9/12)*2)
cercle_3pts_in <- circle_fun(center = c(25,5+3/12), diameter = (23+7/12)*2)
# Hoop
cercle_ce <- circle_fun(center = c(25,5+3/12), diameter = 1.5)
# Free Throws
cercle_lf_out <- circle_fun(center = c(25,19), diameter = 6*2)
cercle_lf_in <- circle_fun(center = c(25,19), diameter = (6-1/6)*2)
# Center Circle
cercle_mil_out <- circle_fun(center = c(25,47), diameter = 6*2)
cercle_mil_in <- circle_fun(center = c(25,47), diameter = (6-1/6)*2)
# Small Center Circle
cercle_mil_petit_out <- circle_fun(center = c(25,47), diameter = 2*2)
cercle_mil_petit_in <- circle_fun(center = c(25,47), diameter = (2-1/6)*2)


#We need to assign the first value of the variable group. Then, each use of new_coords increments group value by one.

group <- 1
court <- new_coords(c(0-1/6,0-1/6,50 + 1/6,50 + 1/6), c(0 - 1/6,0,0,0 - 1/6), group = group, descri = "ligne de fond")
court <- rbind(court, new_coords(x = c(0-1/6,0-1/6,0,0), y = c(0,47-1/12,47-1/12,0), group = group, descri = "ligne gauche"))
court <- rbind(court, new_coords(x = c(50,50,50+1/6,50+1/6), y = c(0,47-1/12,47-1/12,0), group = group, descri = "ligne droite"))
court <- rbind(court, new_coords(x = c(0,0,3,3), y = c(28,28+1/6,28+1/6,28), group = group, descri = "marque entraineur gauche"))
court <- rbind(court, new_coords(x = c(47,47,50,50), y = c(28,28+1/6,28+1/6,28), group = group, descri = "marque entraineur droite"))
court <- rbind(court, new_coords(x = c(3,3,3+1/6,3+1/6), y = c(0,14,14,0), group = group, descri = "3pts bas gauche"))
court <- rbind(court, new_coords(x = c(47-1/6,47-1/6,47,47), y = c(0,14,14,0), group = group, descri = "3pts bas droit"))
court <- rbind(court, new_coords(x = c(17,17,17+1/6,17+1/6), y = c(0,19,19,0), group = group, descri = "LF bas gauche"))
court <- rbind(court, new_coords(x = c(33-1/6,33-1/6,33,33), y = c(0,19,19,0), group = group, descri = "LF bas droit"))
court <- rbind(court, new_coords(x = c(17,17,33,33), y = c(19-1/6,19,19,19-1/6), group = group, descri = "LF tireur"))
court <- rbind(court, new_coords(x = c(14-1/6,14-1/6,14,14), y = c(0,1/2,1/2,0), group = group, descri = "marque fond gauche"))
court <- rbind(court, new_coords(x = c(36,36,36+1/6,36+1/6), y = c(0,1/2,1/2,0), group = group, descri = "marque fond droit"))
court <- rbind(court, new_coords(x = c(19,19,19+1/6,19+1/6), y = c(0,19,19,0), group = group, descri = "LF gauche interieur"))
court <- rbind(court, new_coords(x = c(31-1/6,31-1/6,31,31), y = c(0,19,19,0), group = group, descri = "LF droite interieur"))
court <- rbind(court, new_coords(x = c(22, 22, 28, 28), y = c(4-1/6,4,4,4-1/6), group = group, descri = "planche"))
court <- rbind(court, new_coords(x = c(cercle_3pts_out[31:220,"x"], rev(cercle_3pts_in[31:220,"x"])),
                                 y = c(cercle_3pts_out[31:220,"y"], rev(cercle_3pts_in[31:220,"y"])), group = group, descri = "cercle 3pts"))
court <- rbind(court, new_coords(x = c(cercle_np_out[1:250,"x"], rev(cercle_np_in[1:250,"x"])),
                                 y = c(cercle_np_out[1:250,"y"], rev(cercle_np_in[1:250,"y"])), group = group, descri = "cercle non passage en force"))
court <- rbind(court, new_coords(x = c(20+1/6,20+1/6,20+8/12,20+8/12), y = c(13,13+1/6,13+1/6,13), group = group, descri = "marque bas gauche cercle LF"))
court <- rbind(court, new_coords(x = c(30-8/12,30-8/12,30-1/6,30-1/6), y = c(13,13+1/6,13+1/6,13), group = group, descri = "marque bas droite cercle LF"))
court <- rbind(court, new_coords(x = c(cercle_lf_out[1:250,"x"], rev(cercle_lf_in[1:250,"x"])),
                                 y = c(cercle_lf_out[1:250,"y"], rev(cercle_lf_in[1:250,"y"])), group = group, descri = "cercle LF haut"))
court <- rbind(court, new_coords(x = c(cercle_lf_out[250:269,"x"], rev(cercle_lf_in[250:269,"x"])),
                                 y = c(cercle_lf_out[250:269,"y"], rev(cercle_lf_in[250:269,"y"])), group = group, descri = "cercle LF partie 1"))
court <- rbind(court, new_coords(x = c(cercle_lf_out[288:308,"x"], rev(cercle_lf_in[288:308,"x"])),
                                 y = c(cercle_lf_out[288:308,"y"], rev(cercle_lf_in[288:308,"y"])), group = group, descri = "cercle LF partie 2"))
court <- rbind(court, new_coords(x = c(cercle_lf_out[327:346,"x"], rev(cercle_lf_in[327:346,"x"])),
                                 y = c(cercle_lf_out[327:346,"y"], rev(cercle_lf_in[327:346,"y"])), group = group, descri = "cercle LF partie 3"))
court <- rbind(court, new_coords(x = c(cercle_lf_out[365:385,"x"], rev(cercle_lf_in[365:385,"x"])),
                                 y = c(cercle_lf_out[365:385,"y"], rev(cercle_lf_in[365:385,"y"])), group = group, descri = "cercle LF partie 4"))
court <- rbind(court, new_coords(x = c(cercle_lf_out[404:423,"x"], rev(cercle_lf_in[404:423,"x"])),
                                 y = c(cercle_lf_out[404:423,"y"], rev(cercle_lf_in[404:423,"y"])), group = group, descri = "cercle LF partie 5"))
court <- rbind(court, new_coords(x = c(cercle_lf_out[442:462,"x"], rev(cercle_lf_in[442:462,"x"])),
                                 y = c(cercle_lf_out[442:462,"y"], rev(cercle_lf_in[442:462,"y"])), group = group, descri = "cercle LF partie 6"))
court <- rbind(court, new_coords(x = c(cercle_lf_out[481:500,"x"], rev(cercle_lf_in[481:500,"x"])),
                                 y = c(cercle_lf_out[481:500,"y"], rev(cercle_lf_in[481:500,"y"])), group = group, descri = "cercle LF partie 7"))
court <- rbind(court, new_coords(x = c(17-0.5,17-0.5,17,17), y = c(7,7+1/6,7+1/6,7), group = group, descri = "marque 1 LF gauche"))
court <- rbind(court, new_coords(x = c(17-0.5,17-0.5,17,17), y = c(8+1/6,8+1/3,8+1/3,8+1/6), group = group, descri = "marque 2 LF gauche"))
court <- rbind(court, new_coords(x = c(17-0.5,17-0.5,17,17), y = c(11+1/3,11.5,11.5,11+1/3), group = group, descri = "marque 3 LF gauche"))
court <- rbind(court, new_coords(x = c(17-0.5,17-0.5,17,17), y = c(14.5,14.5+1/6,14.5+1/6,14.5), group = group, descri = "marque 4 LF gauche"))
court <- rbind(court, new_coords(x = c(33,33,33+0.5,33+0.5), y = c(7,7+1/6,7+1/6,7), group = group, descri = "marque 1 LF droite"))
court <- rbind(court, new_coords(x = c(33,33,33+0.5,33+0.5), y = c(8+1/6,8+1/3,8+1/3,8+1/6), group = group, descri = "marque 2 LF droite"))
court <- rbind(court, new_coords(x = c(33,33,33+0.5,33+0.5), y = c(11+1/3,11.5,11.5,11+1/3), group = group, descri = "marque 3 LF droite"))
court <- rbind(court, new_coords(x = c(33,33,33+0.5,33+0.5), y = c(14.5,14.5+1/6,14.5+1/6,14.5), group = group, descri = "marque 4 LF droite"))
court <- rbind(court, new_coords(x = c(0-1/6,0-1/6,50+1/6,50+1/6), y = c(94/2-1/12,94/2, 94/2, 94/2-1/12), group = group, descri = "ligne mediane"))
court <- rbind(court, new_coords(x = c(cercle_mil_out[250:500,"x"], rev(cercle_mil_in[250:500,"x"])),
                                 y = c(cercle_mil_out[250:500,"y"], rev(cercle_mil_in[250:500,"y"])), group = group, descri = "cercle milieu grand"))
court <- rbind(court, new_coords(x = c(cercle_mil_petit_out[250:500,"x"], rev(cercle_mil_petit_in[250:500,"x"])),
                                 y = c(cercle_mil_petit_out[250:500,"y"], rev(cercle_mil_petit_in[250:500,"y"])), group = group, descri = "cercle milieu petit"))
court <- rbind(court, new_coords(x = cercle_ce[,"x"], y = cercle_ce[,"y"], group = group, descri = "anneau"))



#Create the graph 

library(ggplot2)
P <- ggplot() + geom_polygon(data = court, aes(x = x, y = y, group = group), col = "black") +
    coord_equal() +
    ylim(-2,96) +
    xlim(-5,55) +
    scale_x_continuous(breaks = c(0, 12.5, 25, 37.5, 50)) +
    scale_y_continuous(breaks = c(0, 23.5, 47, 70.5, 94)) +
    xlab("") + ylab("") +
    theme(axis.text.x = element_blank(),
          axis.text.y = element_blank(), axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank(), axis.title = element_blank()
    )

P


halfP <- P + coord_cartesian(ylim = c(-2, 44.7))



#############################

##    END COURT CODE #####


####################
####  CODE FOR PICTURE : Just text right now, use when using app locally ###
######################


'playerid <- read.csv("playerid.csv")

playerid$nname = playerid$name

find_player_by_name = function(n) {
  filter(playerid, nname == n)
}

find_player_id_by_name = function(n) {
  find_player_by_name(n)$person_id
}

player_photo_url = function(player_id) {
  paste0("https://stats.nba.com/media/players/230x185/", player_id, ".png")
}
'


####################
####  END CODE FOR PICTURE ###
######################




# Define server logic ----
server <- function(input, output) {
    
    z18_19_02
    
    halfP
    
    #Code for showing picture
    current_player = reactive({
      req(input$player_name)
      find_player_by_name(input$player_name)
    })
    
    output$player_photo = renderUI({
      if (input$player_name == "") {
        tags$img(src = "https://i.imgur.com/hXWPTOF.png", alt = "photo")
      } else if (req(current_player()$person_id)) {
        tags$img(src = player_photo_url(current_player()$person_id), alt = "photo")
      }
    })
    #End
    
    name <- reactive({
        infile <- input$player_name
        
    })
    
    season <- reactive({
        if(input$whatseason ==1 & input$radio ==1) {
            data <- z18_19_02
        } else if (input$whatseason ==2 & input$radio ==1) {
            data<- z17_18_02
        } else if (input$whatseason ==3 & input$radio ==1) { 
            data <- z16_17_02
        } else if (input$whatseason ==4 & input$radio ==1) { 
            data <- z15_16_02
        } else if (input$whatseason ==5 & input$radio ==1) { 
            data <- z14_15_02
        } else if (input$whatseason ==6 & input$radio ==1) { 
            data <- z13_14_02
        } else if (input$whatseason ==7 & input$radio ==1) { 
            data <- z12_13_02
        } else if (input$whatseason ==8 & input$radio ==1) { 
            data <- z11_12_02
        } else if (input$whatseason ==9 & input$radio ==1) { 
            data <- z10_11_02
        } else if (input$whatseason ==10 & input$radio ==1) { 
            data <- z09_10_02
        } else if(input$whatseason ==1 & input$radio ==2 ) {
            data <- z18_19_made_02
        } else if (input$whatseason ==2 & input$radio ==2) {
            data<- z17_18_made_02
        } else if (input$whatseason ==3 & input$radio ==2) { 
            data <- z16_17_made_02
        } else if (input$whatseason ==4 & input$radio ==2) { 
            data <- z15_16_made_02
        } else if (input$whatseason ==5 & input$radio ==2) { 
            data <- z14_15_made_02
        } else if (input$whatseason ==6 & input$radio ==2) { 
            data <- z13_14_made_02
        } else if (input$whatseason ==7 & input$radio ==2) { 
            data <- z12_13_made_02
        } else if (input$whatseason ==8 & input$radio ==2) { 
            data <- z11_12_made_02
        } else if (input$whatseason ==9 & input$radio ==2) { 
            data <- z10_11_made_02
        } else if (input$whatseason ==10 & input$radio ==2) { 
            data <- z09_10_made_02
        } 
        
        
        
        
        
    })
    
    
    
    finalplot <- reactive({
        data<- ifelse(input$player_name == "All",print(generate_chart(season())),  print(player_chart(name(),season()  )))
    })
    
    
    output$plot1 <- renderPlot({
        finalplot() }, width = 500, height = 500 )
    
    finalplot3 <- reactive({
        data<- ifelse(input$player_name == "All",print(season_histogram(season())),  print(player_histogram(name(),season()  )))
    })
    
    output$plot3 <- renderPlot({
        finalplot3()
    }, width = 1000, height = 500 )
    
    
    #Server for second player
    
    name2 <- reactive({
        infile <- input$player_name2
        
    })
    
    season2 <- reactive({
        if(input$whatseason2 ==1 & input$radio2 ==1) {
            data <- z18_19_02
        } else if (input$whatseason2 ==2 & input$radio2 ==1) {
            data<- z17_18_02
        } else if (input$whatseason2 ==3 & input$radio2 ==1) { 
            data <- z16_17_02
        } else if (input$whatseason2 ==4 & input$radio2 ==1) { 
            data <- z15_16_02
        } else if (input$whatseason2 ==5 & input$radio2 ==1) { 
            data <- z14_15_02
        } else if (input$whatseason2 ==6 & input$radio2 ==1) { 
            data <- z13_14_02
        } else if (input$whatseason2 ==7 & input$radio2 ==1) { 
            data <- z12_13_02
        } else if (input$whatseason2 ==8 & input$radio2 ==1) { 
            data <- z11_12_02
        } else if (input$whatseason2 ==9 & input$radio2 ==1) { 
            data <- z10_11_02
        } else if (input$whatseason2 ==10 & input$radio2 ==1) { 
            data <- z09_10_02
        } else if(input$whatseason2 ==1 & input$radio2 ==2 ) {
            data <- z18_19_made_02
        } else if (input$whatseason2 ==2 & input$radio2 ==2) {
            data<- z17_18_made_02
        } else if (input$whatseason2 ==3 & input$radio2 ==2) { 
            data <- z16_17_made_02
        } else if (input$whatseason2 ==4 & input$radio2 ==2) { 
            data <- z15_16_made_02
        } else if (input$whatseason2 ==5 & input$radio2 ==2) { 
            data <- z14_15_made_02
        } else if (input$whatseason2 ==6 & input$radio2 ==2) { 
            data <- z13_14_made_02
        } else if (input$whatseason2 ==7 & input$radio2 ==2) { 
            data <- z12_13_made_02
        } else if (input$whatseason2 ==8 & input$radio2 ==2) { 
            data <- z11_12_made_02
        } else if (input$whatseason2 ==9 & input$radio2 ==2) { 
            data <- z10_11_made_02
        } else if (input$whatseason2 ==10 & input$radio2 ==2) { 
            data <- z09_10_made_02
        } 
        
        
        
    })
    
    
    
    finalplot2 <- reactive({
        data<- ifelse(input$player_name2 == "All",print(generate_chart(season2())), print(player_chart(name2(),season2()  )))
    })
    
    output$plot2 <- renderPlot({
        finalplot2()
    }, width = 500, height = 500 )
    
    
    finalplot4 <- reactive({
        data<- ifelse(input$player_name2 == "All",print(season_histogram(season2())),  print(player_histogram(name2(),season2()  )))
    })
    
    output$plot4 <- renderPlot({
        finalplot4()
    }, width = 700, height = 500 )
    
    
    
}

# Run the app ----
shinyApp(ui = ui, server = server)


