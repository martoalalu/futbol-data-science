create_half_pitch <- function(grass_colour, line_colour, background_colour, goal_colour){
  
  theme_blankPitch = function(size=12) { 
    theme(
      #axis.line=element_blank(), 
      axis.text.x=element_blank(), 
      axis.text.y=element_blank(), 
      #axis.ticks.y=element_text(size=size),
      #   axis.ticks=element_blank(),
      axis.ticks.length=unit(0, "lines"), 
      #axis.ticks.margin=unit(0, "lines"), 
      axis.title.x=element_blank(), 
      axis.title.y=element_blank(), 
      legend.background=element_rect(fill=background_colour, colour=NA), 
      legend.key=element_rect(colour=background_colour,fill=background_colour), 
      legend.key.size=unit(1.2, "lines"), 
      legend.text=element_text(size=size), 
      legend.title=element_text(size=size, face="bold",hjust=0),
      strip.background = element_rect(colour = background_colour, fill = background_colour, size = .5),
      panel.background=element_rect(fill=background_colour,colour=background_colour), 
      #       panel.border=element_blank(), 
      panel.grid.major=element_blank(), 
      panel.grid.minor=element_blank(), 
      panel.spacing=element_blank(), 
      plot.background=element_blank(), 
      plot.margin=unit(c(0, 0, 0, 0), "lines"), 
      plot.title=element_text(size=size*1.2), 
      strip.text.y=element_text(colour=background_colour,size=size,angle=270),
      strip.text.x=element_text(size=size*1))}
  
  ymin <- 0 # minimum width
  ymax <- 80 # maximum width
  xmin <- 60 # minimum length
  xmax <- 120 # maximum length
  
  # Defining features along the length
  boxEdgeOff <- 102
  sixYardOff <- 114
  penSpotOff <- 108
  halfwayline <- 60
  
  # Defining features along the width
  boxEdgeLeft <- 18
  boxEdgeRight <- 62
  sixYardLeft <- 30 
  sixYardRight <- 50
  goalPostLeft <- 36
  goalPostRight <- 44
  CentreSpot <- 40   
  
  # other dimensions
  centreCirle_d <- 20   
  
  ## define the circle function
  circleFun <- function(center = c(0,0),diameter = 1, npoints = 100){
    r = diameter / 2
    tt <- seq(0,2*pi,length.out = npoints)
    xx <- center[1] + r * cos(tt)
    yy <- center[2] + r * sin(tt)
    return(data.frame(x = xx, y = yy))
  }
  
  #### create leftD arc ####
  dArc <- circleFun(c((40),(penSpotOff)),centreCirle_d,npoints = 1000)
  ## remove part that is in the box
  dArc <- dArc[which(dArc$y <= (boxEdgeOff)),]
  
  ## initiate the plot, set some boundries to the plot
  p <- ggplot() + xlim(c(ymin,ymax)) + ylim(c(xmin,xmax)) +
    # add the theme 
    theme_blankPitch() +
    # add the base rectangle of the pitch 
    geom_rect(aes(xmin=ymin, xmax=ymax, ymin=xmin, ymax=xmax), fill = grass_colour, colour = line_colour) +
    # add the 18 yard box offensive
    geom_rect(aes(xmin=boxEdgeLeft, xmax=boxEdgeRight, ymin=boxEdgeOff, ymax=xmax), fill = grass_colour, colour = line_colour) +
    # add the six yard box offensive
    geom_rect(aes(xmin=sixYardLeft, xmax=sixYardRight, ymin=sixYardOff, ymax=xmax), fill = grass_colour, colour = line_colour) +
    # add the arc circle 
    geom_path(data=dArc, aes(x=x,y=y), colour = line_colour) +
    # add the goal offensive
    geom_segment(aes(x = goalPostLeft, y = xmax, xend = goalPostRight, yend = xmax),colour = goal_colour, size = 1)
  
  return(p)
  
}
create_full_pitch <- function(grass_colour, line_colour, background_colour, goal_colour, BasicFeatures){
  
  theme_blankPitch = function(size=12) { 
    theme(
      #axis.line=element_blank(), 
      axis.text.x=element_blank(), 
      axis.text.y=element_blank(), 
      #axis.ticks.y=element_text(size=size),
      #   axis.ticks=element_blank(),
      axis.ticks.length=unit(0, "lines"), 
      #axis.ticks.margin=unit(0, "lines"), 
      axis.title.x=element_blank(), 
      axis.title.y=element_blank(), 
      legend.background=element_rect(fill=background_colour, colour=NA), 
      legend.key=element_rect(colour=background_colour,fill=background_colour), 
      legend.key.size=unit(1.2, "lines"), 
      legend.text=element_text(size=size), 
      legend.title=element_text(size=size, face="bold",hjust=0),
      strip.background = element_rect(colour = background_colour, fill = background_colour, size = .5),
      panel.background=element_rect(fill=background_colour,colour=background_colour), 
      #       panel.border=element_blank(), 
      panel.grid.major=element_blank(), 
      panel.grid.minor=element_blank(), 
      panel.spacing=element_blank(), 
      plot.background=element_blank(), 
      plot.margin=unit(c(0, 0, 0, 0), "lines"), 
      plot.title=element_text(size=size*1.2), 
      strip.text.y=element_text(colour=background_colour,size=size,angle=270),
      strip.text.x=element_text(size=size*1))}
  
  ymin <- 0 # minimum width
  ymax <- 80 # maximum width
  xmin <- 0 # minimum length
  xmax <- 120 # maximum length
  
  # Defining features along the length
  boxEdgeDef <- 18
  boxEdgeOff <- 102
  halfwayline <- 60
  sixYardDef <- 6
  sixYardOff <- 114
  penSpotDef <- 12
  penSpotOff <- 108
  
  # Defining features along the width
  boxEdgeLeft <- 18
  boxEdgeRight <- 62
  sixYardLeft <- 30 
  sixYardRight <- 50
  goalPostLeft <- 36
  goalPostRight <- 44
  CentreSpot <- 40   
  
  # other dimensions
  centreCirle_d <- 20     
  
  ## define the circle function
  circleFun <- function(center = c(0,0),diameter = 1, npoints = 100){
    r = diameter / 2
    tt <- seq(0,2*pi,length.out = npoints)
    xx <- center[1] + r * cos(tt)
    yy <- center[2] + r * sin(tt)
    return(data.frame(x = xx, y = yy))
  }
  
  #### create center circle ####
  center_circle <- circleFun(c(halfwayline,CentreSpot),centreCirle_d,npoints = 100)
  
  
  
  if(BasicFeatures == TRUE){
    ## initiate the plot, set some boundries to the plot
    p <- ggplot() + xlim(c(xmin-5,xmax+5)) + ylim(c(ymin-5,ymax+5)) +
      # add the theme 
      theme_blankPitch() +
      # add the base rectangle of the pitch 
      geom_rect(aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill = grass_colour, colour = line_colour) +
      # add the 18 yard box defensive
      geom_rect(aes(xmin=xmin, xmax=boxEdgeDef, ymin=boxEdgeLeft, ymax=boxEdgeRight), fill = grass_colour, colour = line_colour) + 
      # add the 18 yard box offensive
      geom_rect(aes(xmin=boxEdgeOff, xmax=xmax, ymin=boxEdgeLeft, ymax=boxEdgeRight), fill = grass_colour, colour = line_colour) +
      # add halway line
      geom_segment(aes(x = halfwayline, y = ymin, xend = halfwayline, yend = ymax),colour = line_colour) + 
      # add the goal Defensive
      geom_segment(aes(x = xmin, y = goalPostLeft, xend = xmin, yend = goalPostRight),colour = goal_colour, size = 1) +
      # add the goal offensive
      geom_segment(aes(x = xmax, y = goalPostLeft, xend = xmax, yend = goalPostRight),colour = goal_colour, size = 1)
    
  }else{
    ## initiate the plot, set some boundries to the plot
    p <- ggplot() + xlim(c(xmin-5,xmax+5)) + ylim(c(ymin-5,ymax+5)) +
      # add the theme 
      theme_blankPitch() +
      # add the base rectangle of the pitch 
      geom_rect(aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill = grass_colour, colour = line_colour) +
      # add the 18 yard box defensive
      geom_rect(aes(xmin=xmin, xmax=boxEdgeDef, ymin=boxEdgeLeft, ymax=boxEdgeRight), fill = grass_colour, colour = line_colour) + 
      # add the 18 yard box offensive
      geom_rect(aes(xmin=boxEdgeOff, xmax=xmax, ymin=boxEdgeLeft, ymax=boxEdgeRight), fill = grass_colour, colour = line_colour) +
      # add halway line
      geom_segment(aes(x = halfwayline, y = ymin, xend = halfwayline, yend = ymax),colour = line_colour) +
      # add the six yard box Defensive
      geom_rect(aes(xmin=xmin, xmax=sixYardDef, ymin=sixYardLeft, ymax=sixYardRight), fill = grass_colour, colour = line_colour)  +
      # add the six yard box offensive
      geom_rect(aes(xmin=sixYardOff, xmax=xmax, ymin=sixYardLeft, ymax=sixYardRight), fill = grass_colour, colour = line_colour) +
      # add centre circle 
      geom_path(data=center_circle, aes(x=x,y=y), colour = line_colour) +
      # add penalty spot left 
      geom_point(aes(x = penSpotDef , y = CentreSpot), colour = line_colour) + 
      # add penalty spot right
      geom_point(aes(x = penSpotOff , y = CentreSpot), colour = line_colour) + 
      # add centre spot 
      geom_point(aes(x = halfwayline , y = CentreSpot), colour = line_colour) + 
      # add the goal Defensive
      geom_segment(aes(x = xmin, y = goalPostLeft, xend = xmin, yend = goalPostRight),colour = goal_colour, size = 1) +
      # add the goal offensive
      geom_segment(aes(x = xmax, y = goalPostLeft, xend = xmax, yend = goalPostRight),colour = goal_colour, size = 1) 
  }
  
  return(p)
  
}
