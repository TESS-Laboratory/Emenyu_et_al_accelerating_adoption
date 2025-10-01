library(data.table)
library(tidyverse)# data manipulation
library(epiDisplay)#presents graphs and tables in horizontal
library(plotrix)#plots 3D graphs
library(ggpubr)# used to create graphs
library(lubridate)# used to manipulate date
library(RColorBrewer)
library(forcats)
library(patchwork)

# importing Enrollment data-----
Enrolment <- read_csv('UGKEenrolment.csv')#  NB: read_csv allows one to add arguments in code instead of read.csv e.g read_csv('TIST enrolment data072023.csv'),col_types=cols(Date_registered = col_date("%m/%d/%Y %H:%M") if I wanted to change date attributes of specific column from onset

# making sure the values in the date registerd column are recognised as dates in R----
Enrolment$Date<-mdy_hm(Enrolment$date_registered)
Enrolment$Year_Enroled<-year(Enrolment$Date)

# Only filtered groups that planted trees----
Enroled_Trees_Planted<-Enrolment%>%
  filter(Trees>=1)%>%#mutate used to create new columns based o existing column, function in dplyr
  filter(Year_Enroled < "2023")
##Uganda----
Uganda_Enrolment <- Enroled_Trees_Planted %>%
  filter(Country == "UG")

# Enrollment stats----
Uganda_Enrolment_stats<-Uganda_Enrolment %>%
  group_by(Year_Enroled, Proj_Area)%>%
  dplyr::reframe(Groups<- sum(Groups))

colnames(Uganda_Enrolment_stats) <- c("Year","Area","Groups")

UG_Enrolment_stats2<-Uganda_Enrolment%>%
  group_by(Proj_Area)%>%
  dplyr::reframe(Group_sums<-sum(Groups))


colnames(UG_Enrolment_stats2)<-c("Proj_Area","Groups")

#Computing cumulative enrollment ----
Uganda_Enrolment_freq<-Uganda_Enrolment_stats%>%
   dplyr::mutate(cs=cumsum(Groups))
 
colnames(Uganda_Enrolment_freq)<- c("Year","Area","Groups","Total_Groups")

Uganda_Enrolment_freq$Area<- factor(Uganda_Enrolment_freq$Area, levels=c("Lamwo","Kole","Kitgum","Hoima","Isingiro","Lira","Omoro", "Kayunga","Kyenjojo","Gulu","Kiryandongo","Soroti", "Amuru","Rukungiri", "Bushenyi","Kabale", "Kanungu"))
UG_Enrolment_stats2$Proj_Area <-factor(UG_Enrolment_stats2$Proj_Area,levels=c("Kanungu","Kabale","Bushenyi","Rukungiri","Amuru","Soroti","Kiryandongo", "Gulu","Kyenjojo", "Kayunga","Omoro",  "Lira","Isingiro","Hoima","Kitgum","Kole","Lamwo"))
                                                                              

# creating a vector of colors from the various shades of qualitative pallets from RcolorBrewer t-----
qual_col_pals=brewer.pal.info[brewer.pal.info$category=='qual',]
col_vector=unlist(mapply(brewer.pal,qual_col_pals$maxcolors,rownames(qual_col_pals)))
col_vector

#stacked area plot-----
UG <- ggplot(Uganda_Enrolment_freq,
             aes(x = Year, 
                 y = Total_Groups,
                 fill = Area)) +
  geom_area(aes(colour = Area, fill = Area)) +
  scale_fill_manual(values = col_vector) +
  scale_x_continuous(expand = c(0,0)) + 
  scale_y_continuous(expand = c(0,0)) +
  theme_light() +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(color = "black"),
    legend.position = "bottom",
    axis.text = element_text(size = 14),      # axis tick labels
    axis.title = element_text(size = 16),     # axis titles
    legend.text = element_text(size = 12),    # legend labels
    legend.title = element_text(size = 14)    # legend title
  )


UG
##############################################################################################
##Kenya------

Kenya_Enrolment <- Enroled_Trees_Planted %>%
  filter(Country == "KE")

# Enrollment stats----
Kenya_Enrolment_stats<-Kenya_Enrolment%>%
  group_by(Year_Enroled, Proj_Area)%>%
  dplyr::reframe(Groups<- sum(Groups))

colnames(Kenya_Enrolment_stats) <- c("Year","Area","Groups")

#Computing cumulative enrolment ----
Kenya_Enrolment_freq<-Kenya_Enrolment_stats%>%
  group_by(Area)%>%
  dplyr::mutate(cs=cumsum(Groups))

colnames(Kenya_Enrolment_freq)<- c("Year","Area","Groups","Total_Groups")

Kenya_Enrolment_freq$Area<- factor(Kenya_Enrolment_freq$Area, levels=c("Muranga","Machakos","Nyamira","Trans-Nzoia","Embu","Mara","Meru","Nanyuki"))
#stacked area plot-----
KE<-ggplot(Kenya_Enrolment_freq,
       aes(x=Year, 
           y=Total_Groups,
           fill=Area))+
  geom_area(aes(colour=Area, fill=Area))+
    theme_set(theme_light())  +
  scale_fill_manual(values=col_vector)+
  scale_x_continuous(expand=c(0,0)) + scale_y_continuous(expand=c(0,0))+
  theme_bw()+
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(color = "black"),
    legend.position = "bottom",
    axis.text = element_text(size = 14),      # axis tick labels
    axis.title = element_text(size = 16),     # axis titles
    legend.text = element_text(size = 12),    # legend labels
    legend.title = element_text(size = 14)    # legend title
  )


KE
#############################################################################
# Combine Uganda and Kenya plots side by side
UG_KE <- UG + KE +
  plot_layout(ncol = 2) +
  plot_annotation(
    tag_levels = "a",          # sequential letters
    tag_prefix = "(",          # add opening bracket
    tag_suffix = ")"           # add closing bracket
  ) & 
  theme(
    plot.tag = element_text(size = 16, face = "bold"),
    plot.tag.position = "top"  # put labels above the plots
  )

UG_KE

# Save combined plot
ggsave("UG_KE_enrolment.png", UG_KE, dpi = 600, width = 12, height = 6)
