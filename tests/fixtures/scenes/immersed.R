immersed_showpiece_plot <- function(name,n=12L) {
  surface <- if(name=="trefoil") stat_knot_3d(radius=.12,n=4L*n,sides=6L,fill="#DBAE74") else
    stat_immersed_3d(type=name,n=c(n,max(8L,round(n*.8))),fill="#5CA7CE")
  ggplot2::ggplot()+surface+coord_3d(axes=character(),panes=character(),box="none",azimuth=25,elevation=20)
}
