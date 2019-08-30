###################################################################################################
# cfr_edu_region: interactive map
# 
# Input data:
#     1. cfr_edu_region_map.Rdata
#     2. cfr_edu_region_spdf.Rdata
# 
#
# 2019-08-30
#
####################################################################################
#SERVER.R
library(shiny)
library(leaflet)
library(ggplot2)
library(maptools)
library(rgeos)
load("cfr_edu_region_map.Rdata")
load("cfr_edu_region_spdf.Rdata")
####################################################################################
#### barplot
plot_cfr_vs_nobs <- function(CFR=c(2.010228,1.724783,1.64745), 
                             nobs=c(841,2797,1707),
                             lower=c(1.910228,1.624783,1.54745),
                             upper=c(2.510228,1.924783,1.84745),
                             CFRobs=c(2.1,1.8,1.7),
                             upcfr=max(CFR),
                             hline=c(2.010228,1.724783,1.64745)){
  # make data
  tmp=data.frame(group=c("Low","Medium","High") , CFR=as.numeric(CFR) , nobs=as.numeric(nobs))
  tmp[,"group"] = factor(tmp[,"group"] , levels = c("Low","Medium","High"))
  # Calculate the future positions on the x axis of each bar (left border, central position, right border)
  tmp$right=cumsum(tmp$nobs) 
  tmp$left=tmp$right - tmp$nobs + (sum(tmp$nobs) *0.01)*c(0,rep(1,nrow(tmp)-1))
  
  # add prediction intervals
  tmp$pos= (tmp$right + tmp$left)/2
  tmp$lower=as.numeric(lower)
  tmp$upper=as.numeric(upper)
  tmp$CFRobs=as.numeric(CFRobs)
  
  # Plot
  g <- ggplot(tmp, aes(ymin = 0)) + 
    geom_rect(aes(xmin = left, xmax = right, ymax = CFR, fill = group)) +
    xlab("Percentage of women") + 
    ylab("Cohort fertility rate (CFR)") + 
    coord_cartesian(ylim=c(1,upcfr))+
    scale_x_continuous(breaks = unique(tmp$right))+
    theme(plot.margin = unit(c(0,0,0,0), "cm"),
          plot.background = element_rect(fill = NA,colour = "white"),
          plot.title=element_blank(),
          panel.grid = element_blank(),
          panel.grid.major.y = element_line(colour="grey")) + 
    guides(fill=guide_legend("Education"))+ 
    geom_hline(aes(yintercept = hline[1]),colour="red", linetype="dashed",size=1.1) +
    geom_hline(aes(yintercept = hline[2]),colour="green", linetype="dashed",size=1.1) +
    geom_hline(aes(yintercept = hline[3]),colour="blue", linetype="dashed",size=1.1)+
    geom_segment(aes(x=pos,y=lower,xend=pos,yend=upper),data = tmp,size=0.6)+
    geom_segment(aes(x=pos - (right-left)/5,y=lower,xend=pos + (right-left)/5,yend=lower),data = tmp,size=0.6)+
    geom_segment(aes(x=pos - (right-left)/5,y=upper,xend=pos + (right-left)/5,yend=upper),data = tmp,size=0.6)+
    geom_point(aes(x=pos,y=CFRobs),data=tmp,shape=4,size=3.5,col="blue")
    
  if(is.na(CFR[1])){
    return(NA)
  }
  return(g)
}

###############################################
# define a function which finds which state a point is in. This is the function 
# that takes input from click and give the name of the state being clicked
which_state <- function(mapData, long, lat) {
  # This function decide the state being clicked. 
  #
  # Args:
  #   mapData: The map data has a column "long" and a column "lat" to determine
  #       state borders. 
  #   long, lat: longitude and latitude of the clicked point. They are actually
  #       input$clickMap$x and input$clickMap$y assuming click = "clickMap".
  #
  # Returns: 
  #   The name of the state containing the point (long, lat).
  
  pot = SpatialPoints(coords = data.frame(long,lat),proj4string = CRS(proj4string(mapData)))
  inout = over(pot,mapData)
  
  return(inout[1,"geo"])
  
}


####################################################################################

server <- function(input, output) {
  
  # create a reactive value that will store the click position
  data_of_click <- reactiveValues(clickedMarker=NULL)
  
  # Leaflet map 
  output$map <- renderLeaflet({
    cfr_edu_region_map  
  })
  
  
  # Make a barplot or scatterplot depending of the selected point
  output$plot <- renderPlot({
    #plot_cfr_vs_nobs(upcfr = 3)
  })
  
  
  # plot after click
  observeEvent(input$map_click, {
    
    xClick <- input$map_click$lng
    yClick <- input$map_click$lat
    print(input$map_click)
    state <- which_state(cfr_edu_region_spdf, xClick, yClick)
    print(state)
    selected <- cfr_edu_region_spdf[which(cfr_edu_region_spdf$geo == state),]
    if(is.null(proj4string)){
      proj4string(selected) <- CRS(proj4string(mapData))
    }
    
    
    proxy <- leafletProxy("map")
    proxy %>% addPolygons(data = selected, 
                          fillColor = "yellow",
                          fillOpacity = 1, 
                          color = "whight",
                          weight = 1, 
                          stroke = T,
                          layerId = "Selected")
    
    output$plot <- renderPlot({
      plot_cfr_vs_nobs(CFR   = cfr_edu_region_spdf@data[state,c("EB_fert_Low","EB_fert_Medium","EB_fert_High")],
                       nobs  = cfr_edu_region_spdf@data[state,c("pro_Low","pro_Medium","pro_High")],
                       lower = cfr_edu_region_spdf@data[state,c("EB_fert_Low_CIL","EB_fert_Medium_CIL","EB_fert_High_CIL")],
                       upper = cfr_edu_region_spdf@data[state,c("EB_fert_Low_CIU","EB_fert_Medium_CIU","EB_fert_High_CIU")],
                       CFRobs= cfr_edu_region_spdf@data[state,c("fert_Low","fert_Medium","fert_High")],
                       upcfr = 3,
                       hline = as.numeric(cfr_edu_region_spdf@data[state,c("Aver_Low","Aver_Medium","Aver_High")])) #+
      #ggtitle(paste(data[state,"REGION_ENGLISH"]))
    })
    
    
    output$Country <- renderText(ifelse(is.na(selected$GDP),NA,paste("Country: ",selected$COUNTRY)))
    output$Region  <- renderText(ifelse(is.na(selected$GDP),NA,paste("Region: ",selected$REGION_ENGLISH)))
    output$GDP     <- renderText(ifelse(is.na(selected$GDP),NA,paste("GDP: ",round(selected$GDP), " euro")))
    output$Fertility <- renderText(ifelse(is.na(selected$GDP),NA,paste("CFR: ",paste0(format(c(selected$EB_fert_Low,selected$EB_fert_Medium,selected$EB_fert_High),nsmall = 2),collapse = ", "))))
    output$Noteline  <- renderText(ifelse(is.na(selected$GDP),NA,"................................................."))
    output$Note1     <- renderText(ifelse(is.na(selected$GDP),NA,"Bars and their confidence intervals show the cohort fertility rates (CFRs) by level of education and region of living estimated by the Empirical Bayesian method."))
    output$Note2     <- renderText(ifelse(is.na(selected$GDP),NA,"Blue crosses show the observed cohort fertility rates (CFRs) by level of education and region of living. Rates for Belgium, Hungary, Lithuania, Netherlands, Norway, and Sweden are based on full samples."))
    output$Note3     <- renderText(ifelse(is.na(selected$GDP),NA,"Dashed lines show the average across 15 European countries of observed cohort fertility rates (CFRs) by level of education."))
  })
}