#Load the libraries
library(tidyverse)
library(ggtext)
library(grid)
library(jpeg)
library(cowplot)
library(showtext)

#load text
font_add_google("Raleway", "Raleway")
font_add_google("Reem Kufi", "Reem Kufi")
showtext_auto()

#load image
ima_2 <- readJPEG("frog2.jpg")

#These are for the points on the frog, I realize this is not elegantly done at all
x <- c(1, 2, 3)
y <- c(1, 2, 3)
xy<- cbind(x, y)
xy<- as_tibble(xy)

#basefrog----
basefrog<-
  ggplot(xy, aes(x = x, y = y)) +
  annotation_custom(rasterGrob(ima_2, width=unit(1,"npc"), 
                               height=unit(1,"npc")), 
                    0, 3, -Inf, 1) +
  #toepad
  geom_point(color = "#c5ff5f", aes(x = 1.1, y = 0.22), size = 11, shape =21)+
  geom_point(color = "#c5ff5f", aes(x = 1.1, y = 0.22), size = 8, alpha = 0.3)+
  geom_point(color = "#c5ff5f",aes(x = 1.1, y = 0.22), size = 3, alpha = 1) +         geom_segment(aes(x = 1.1, xend = 1.8, 
                                                                                                       y = 0.22, yend = 0.05), color = "#c5ff5f")+
  geom_segment(aes(x = 1.8, xend = 2.3, 
                   y = 0.05, yend = 0.05), color = "#c5ff5f")+
  geom_text(aes(label = "Digital"), y = 0.1, x = 2.2, 
            color = "#c5ff5f", family = "Raleway", fontface = "italic", size = 5)+
  
  #inguinal
  geom_point(color = "#9ef6ff", aes(x = 1, y = 0.5), size = 3, alpha =1) +
  geom_point(color = "#9ef6ff", aes(x = 1, y = 0.5), size = 8, alpha =0.3) +
  geom_point(color = "#9ef6ff", aes(x = 1, y = 0.5), size = 11, shape = 21) +
  geom_segment(aes(x = 1, xend = 1, 
                   y = 0.5, yend = 0.88), color = "#9ef6ff")+
  geom_segment(aes(x = 1, xend = 1, 
                   y = 0.5, yend = 0.88), color = "#9ef6ff")+
  geom_segment(aes(x = 1, xend = 0.5, 
                   y = 0.88, yend = 0.88), color = "#9ef6ff")+
  geom_text(aes(label = "Inguinal"), y = 0.93, x = 0.7, 
            color = "#9ef6ff", family = "Raleway", fontface = "italic", size = 5)+
  #cranial
  geom_point(color = "#fad700", aes(x = 2.45, y = 0.85), size = 3, alpha =1) +
  geom_point(color = "#fad700", aes(x = 2.45, y = 0.85), size = 8, alpha =0.3) +
  geom_point(color = "#fad700", aes(x = 2.45, y = 0.85), size = 11, shape = 21) +
  geom_segment(aes(x = 2.45, xend = 2.1, y = 0.85, yend = 1), color = "#fad700")+
  geom_segment(aes(x = 2.1, xend = 1.5, y = 1, yend = 1), color = "#fad700")+
  geom_text(aes(label = "Cranial"), y = 1.05, x = 1.8, 
            color = "#fad700", family = "Raleway", fontface = "italic", size = 5)+
  xlim(0, 3)+
  ylim(0, 1) +
  coord_cartesian(clip = "off", xlim = c(0, 7.8), ylim= c(0,1.6))+
  theme_void()+
  theme(panel.background = element_rect(fill = "black",colour = "black")) 

#Check it out!
basefrog

#Adding title----

corefrog<-
  basefrog+
  #Title
  geom_text(label = c("Core chytrid sampling areas"), 
            x = 1.3, y = 1.4,size = 8,
            color = "gold", 
            family = "Reem Kufi",
            fontface = "bold",
            stat = "unique")+
  #Subtitle
  geom_text(label = c("Most common areas of anuran sampling based on survey of \n3.000 methodological papers"), 
            x = 1.5, y = 1.3,size = 3.5,
            color = "lightgrey", 
            family = "Reem Kufi",
            #fontface = "bold",
            stat = "unique") + 
  #Credit
  geom_text(aes(label = "Chloe Fouilloux | 2021 | Plot inspired by Ijeamaka Anyene | Photo by Dirk Ercken"),x = 7.7, y = 0.8, angle = 90,
            color = "lightgrey", family = "Raleway",size = 2.5)+
  #warning
  geom_text(aes(label = "Information is hypothetical and used for visualization purposes only. Do not reproduce figure as if information were factual."), 
            x = 5.2, y = 0.26,
            color = "gold", family = "Raleway",size = 2.5)+
  #circlegraph legend
  geom_text(aes(label = "Positive swabs based on sample site"), 
            x = 5.2, y = 0.18,size = 6, color = "lightgrey",
            fontface = "bold",
            family = "Raleway") +
  geom_text(aes(label = "Toe pad (digit) sampling consistently provided higher zoospore\n counts compared to samples from other regions of adult anurans"), 
            x = 5.2, y = 0.07,size = 3, color = "lightgrey",
            family = "Raleway")

#plot.margin = margin(20, 40, 20, 40),
#\n

corefrog

#Adding lines----

#Chloe attemp of the circle one!!!

center_circle = tibble(
  x = seq(0, 19, by = .5), #this takes 39 rows
  xend = x,
  y = rep(0, 39), #repeat 0, 39 times
  yend = rep(3, 39)) #repeat 3, 39 times

outer_circle = center_circle %>%
  mutate(y = yend, yend = yend + 5)

mega_circle = center_circle %>%
  mutate(y = yend,
         type = rep(LETTERS[1:3], 13), 
         type1= rep(c("a", "b", "c"), c(21, 11, 7)), 
         yend = ifelse(type1 == "a", yend + 12, 
                       ifelse(type1 == "b", yend + 10, yend + 6)))
#awesomecircle
awesomecircle<-
  ggplot() +
  #mega circle
  geom_segment(data = mega_circle,
               aes(x = x, xend = xend,
                   y = y, yend = yend, 
                   color = type1),
               size = 2,
               #color = "black", 
               #linetype = "dashed",
               alpha = 0.8)+ 
  #a bunch of straight lines, overlapped and thicker
  geom_segment(data = outer_circle,
               aes(x = x, xend = xend,
                   y = y, yend = yend),
               size = 0.5,
               color = "white")+ 
  #a bunch of straight lines.
  geom_segment(data = center_circle,
               aes(x = x, xend = xend,
                   y = y, yend = yend),
               size = 0.5,
               color = "white", 
               linetype = "dotted") +
  #scale_y_continuous(limits = c(-2, 10)) +
  coord_polar() +
  scale_color_manual(values = c("#c5ff5f", "#9ef6ff", "#fad700"))+
  theme_void() +
  theme(panel.grid = element_line(color = "black"), 
        panel.background = element_rect(fill = "transparent"),
        panel.grid.major.x = element_blank(), 
        panel.grid.major.y = element_blank(), 
        legend.position = "none")

awesomecircle

#Now let's try to combine these plots lol

#Combo----

#mainplot
corefrog

#insetplot 
awesomecircle

ggdraw() +
  draw_plot(corefrog) +
  draw_plot(awesomecircle, x = 0.31, y = 0.18,
            width = 0.6, height = 0.6)
