






#' @export
north_arrow_fancy_orienteering<-function (line_width = 1, line_col = "black", fill = c("white",
                                                                                       "black"), text_col = "black", text_family = "",
                                          text_face = NULL, text_size = 10, text_angle = 0)
{
  arrow_x <- c(0.25, 0.5, 0.5, 0.75, 0.5, 0.5)
  arrow_y <- c(0.1, 0.8, 0.3, 0.1, 0.8, 0.3)
  arrow_id <- c(1, 1, 1, 2, 2, 2)
  text_y <- 0.95
  text_x <- 0.5
  grid::gList(grid::circleGrob(x = 0.505, y = 0.4, r = 0.3,
                               default.units = "npc", gp = grid::gpar(fill = NA,
                                                                      col = line_col, lwd = line_width)), grid::polygonGrob(x = arrow_x,
                                                                                                                            y = arrow_y, id = arrow_id, default.units = "npc",
                                                                                                                            gp = grid::gpar(lwd = line_width, col = line_col, fill = fill)),
              grid::textGrob(label = "N", x = text_x, y = text_y,
                             rot = text_angle, gp = grid::gpar(fontfamily = text_family,
                                                               fontface = text_face, fontsize = text_size, col = text_col)))
}


#' @export
annotation_north_arrow<-function (mapping = NULL, data = NULL, ..., height = unit(1.5,
                                                                                  "cm"), width = unit(1.5, "cm"), pad_x = unit(0.25,
                                                                                                                               "cm"), pad_y = unit(0.25, "cm"), rotation = NULL,
                                  style = north_arrow_orienteering)
{
  if (is.null(data)) {
    data <- data.frame(x = NA)
  }
  GeomNorthArrow<-readRDS("inst/app/www/GeomNorthArrow.rds")

  ggplot2::layer(data = data, mapping = mapping, stat = ggplot2::StatIdentity,
                 geom = GeomNorthArrow, position = ggplot2::PositionIdentity,
                 show.legend = FALSE, inherit.aes = FALSE, params = list(...,
                                                                         height = height, width = width, pad_x = pad_x, pad_y = pad_y,
                                                                         rotation = rotation, style = style))
}


#' @export
annotation_scale<-function (mapping = NULL, data = NULL, ..., plot_unit = NULL,
                     bar_cols = c("black", "white"), line_width = 1,
                     height = unit(0.25, "cm"), pad_x = unit(0.25, "cm"),
                     pad_y = unit(0.25, "cm"), text_pad = unit(0.15, "cm"),
                     text_cex = 0.7, text_face = NULL, text_family = "",
                     tick_height = 0.6)
{
  if (is.null(data)) {
    data <- data.frame(x = NA)
  }
  GeomScaleBar<-readRDS("inst/app/www/GeomScaleBar.rds")
  ggplot2::layer(data = data, mapping = mapping, stat = ggplot2::StatIdentity,
                 geom = GeomScaleBar, position = ggplot2::PositionIdentity,
                 show.legend = FALSE, inherit.aes = FALSE, params = list(...,
                                                                         plot_unit = plot_unit, bar_cols = bar_cols, line_width = line_width,
                                                                         height = height, pad_x = pad_x, pad_y = pad_y, text_pad = text_pad,
                                                                         text_cex = text_cex, text_face = text_face, text_family = text_family,
                                                                         tick_height = tick_height))
}


#' @export
getcolhabs<-function(newcolhabs,palette,n){
  newcolhabs[[palette]](n)
}

#' @export
plotshape<-function(shape){
  ggplot(st_as_sf(shape)) + geom_sf()+
    theme(panel.background = element_rect(fill = "white"),
          panel.border = element_rect(fill=NA,color="black", size=0.5, linetype="solid"))
}

#' @export
map_discrete_variable<-function(data,get,coords,base_shape=NULL,layer_shape=NULL,main="",size=.14,cex.main=15,cex.axes=13,cex.lab=15,cex.sub=14,cex.leg=11,cex.pt=7,subtitle="",leg="", factors=NULL,showcoords=F, cex.coords=NULL, col.coords="firebrick",col.palette='turbo',col.fac="firebrick",symbol=15, scalesize_size=T,scalesize_color=T, points=T, cex.fac=4, as_factor=F,bmu=F,key.height=1, colored_by_factor=NULL,showguides=F, limits=NULL,layer_col="gray",lighten=0.5,base_col="white",base_lighten=1,newcolhabs, extralayers=NULL,  data_depth=if(!is.null(extralayers)){3+(length(extralayers$layers)*2)} else{NULL},breaks_len=5,mybreaks=NULL,cexmin.pt=0,layer_shape_border="Grey",base_shape_border="gray", keyscale=12,  width_hint=0.15,cex_scabar=0.7
                               ){

  {
    {
    base_shape0<-base_shape
    layer_shape0<-layer_shape
    shapes<-get_shapes(base_shape,layer_shape,coords)
    base_shape<-shapes$base_shape
    layer_shape<-shapes$layer_shape
    BS_ggplot<-layer_shape
    if(isTRUE(as_factor)){
      scalesize_size=F
      scalesize_color=T
      if(isTRUE(bmu)){
        somprev<-somC$som.model$unit.classif
        names(somprev)<-names(somC[[1]])
        colhabs<-somC$colunits
        nlevels<-nrow(somC$som.model$grid$pts)
        name.var="bmu"
      } else{
        somprev<-as.numeric(as.factor(data[,get]))
        names(somprev)<-rownames(data[get])
        nlevels<-nlevels(as.factor(somprev))
        colhabs=getcolhabs(newcolhabs,col.palette,as.vector(nlevels))
        if(is.null(leg)){name.var=get} else {name.var=leg}
      }
      prev=newy<-factor(somprev, levels=1:nlevels)
      geopoint<-cbind(coords[names(prev),],prev)
      colnames(geopoint)<-c("x","y","pop")
      colfac= getcolhabs(newcolhabs,col.fac,as.vector(nlevels))[as.factor(prev)]
    } else{
      if(!is.null(factors)){
        colfac= getcolhabs(newcolhabs,col.fac,as.vector(nlevels(as.factor(factors))))[as.factor(factors)]
      }
    }
    if(!is.null(points)){
      geopoint<-cbind(coords[rownames(data),],data[,get])
      colnames(geopoint)<-c("x","y","pop")
      if(is.null(leg)){name.var=get} else {name.var=leg}
    }
    if(is.null(points)& as_factor==F)
    {
      geopoint<-cbind(coords[rownames(data),])
      colnames(geopoint)<-c("x","y")
      name.var=""

    }
    if(!is.factor(geopoint$pop)){
    if(cexmin.pt==0){
    #geopoint[geopoint$pop==min(geopoint$pop),]<-NA
    }}

    geopoint<-na.omit(geopoint)
    if(isTRUE(as_factor)){  mybreaks<-1:nlevels
    } else{
      if(is.factor(geopoint$pop)){ mybreaks<-1:nlevels(geopoint$pop)}else {

      }
    }

    if(is.null(limits)){
      limits<-st_bbox(base_shape)}
    xlimits<-limits[c(1,3)]
    ylimits<-limits[c(2,4)]
    bbox<-t(cbind(xlimits,ylimits))
    #if(any(mybreaks==0)){mybreaks<-mybreaks[-which(mybreaks==0)]}

    suppressWarnings(st_crs(BS_ggplot)<-"+proj=longlat +datum=WGS84 +no_defs")

    p<-ggplot(st_as_sf(to_spatial(geopoint)))
    if(!is.null(layer_shape0)){
      p<-p+geom_sf(data=st_as_sf(layer_shape), fill=mylighten(layer_col,lighten), lty=1,color=layer_shape_border)
    }
    if(!is.null(base_shape0)){
      p<-p+geom_sf(data=base_shape, fill=mylighten(base_col,base_lighten),color=base_shape_border, lty=1)
    }
    if(!is.null(extralayers)){
      for(i in 1:length(  extralayers$layers)){
        col_extra<-getcolhabs(newcolhabs,extralayers$colors[i],nrow(as.data.frame(st_as_sf(extralayers$layers[[i]]))))
        p<-p+geom_sf(data=st_as_sf(extralayers$layers[[i]]), col=mylighten(   col_extra,extralayers$alphas[i]), lty=1)
        names( p$layers)[length( p$layers)]<-paste0("extra",i)
        if(extralayers$labels[i]!='None'){
          p<-p+geom_sf_text(data=st_as_sf(extralayers$layers[[i]]),aes(label=get(extralayers$labels[i])), size=extralayers$sizes[i],check_overlap=T,col=col_extra)
          names( p$layers)[length( p$layers)]<-paste0("extra_lab",i)
        }
      }
    }
    if(!is.null(colored_by_factor)){
      colfactor<-colored_by_factor[,1]
      names(colfactor)<-rownames(data[get])
      nlevels_fac<-nlevels(as.factor(colfactor))
      prev_fac<-colfactor
      col_pts=getcolhabs(newcolhabs,col.palette,as.vector(nlevels_fac))
      colorFAC<-  data.frame(prev_fac=levels(colfactor),col_pts, levels=1:nlevels(colfactor))
      geopoint_fac<-cbind(geopoint,fac=prev_fac[rownames(geopoint)])
      col_pts<-col_pts[rownames(geopoint_fac)]
    } else {col_pts=getcolhabs(newcolhabs,col.palette,100)[2]}
    }

    if(!is.null(points)) {
      if(isTRUE(scalesize_size)){
        if(isTRUE(scalesize_color))
        {  p <-  p+ geom_point( data=geopoint, aes(x=x, y=y, size=pop, col=pop), pch=symbol)
        if(!any(geopoint$pop<0)){
          p<-p+scale_radius(name=name.var, range=c(cexmin.pt,cex.pt), breaks=mybreaks,guide="none")
        }

        } else if(isFALSE(scalesize_color)&is.null(colored_by_factor))
        {   p <- p+geom_point( data=geopoint, aes(x=x, y=y, size=pop), pch=symbol,color=col_pts)+scale_radius(name=name.var, range=c(cexmin.pt,cex.pt), breaks=mybreaks)} else if(isFALSE(scalesize_color)&!is.null(colored_by_factor)){
          p <- p+geom_point( data=geopoint_fac, aes(x=x, y=y, size=pop, col=fac), pch=symbol)+ scale_color_manual(name=leg,labels =  colorFAC$prev_fac,values =  colorFAC$col_pts,drop=F )+scale_radius(name=name.var, range=c(cexmin.pt,cex.pt), breaks=mybreaks)
        }

      } else  if(isFALSE(scalesize_size))
      {
        if(isTRUE(scalesize_color)){
          p<-  p+ geom_point( data=geopoint, aes(x=x, y=y,  col=pop), pch=symbol,size=cex.pt)+scale_radius(name=name.var, range=c(cexmin.pt,cex.pt), breaks=mybreaks,guide="none")} else if(isFALSE((scalesize_color))){
            if(!is.null(colored_by_factor)) {
              p <- p+geom_point( data=geopoint_fac, aes(x=x, y=y, col=fac), pch=symbol,size=cex.pt)+ scale_color_manual(name=leg,labels =  colorFAC$prev_fac,values =  colorFAC$col_pts,drop=F)+scale_radius(name=name.var, range=c(cexmin.pt,cex.pt), breaks=mybreaks,guide="none")
            } else {
              p<-   p+geom_point( data=geopoint, aes(x=x, y=y), pch=symbol,size=cex.pt, color=col_pts[rownames(geopoint)])+scale_radius(name=name.var, range=c(cexmin.pt,cex.pt), breaks=mybreaks,guide="none")
            }
          }
      }
    }
    names( p$layers)[length( p$layers)]<-paste0('points')

    #scale_color_viridis(name=name.var, limits= range(mybreaks)) +
    if(is.null(colored_by_factor)){
      if(isTRUE(as_factor)){p<- p +   geom_point( data=geopoint, aes(x=x, y=y), pch=symbol, color=colhabs[as.factor(prev)]) }else{
        if(!any(geopoint$pop<0)){
          p<-p+
            scale_radius(name=name.var, range=c(cexmin.pt,cex.pt), breaks=mybreaks,guide="none")
        } else{
          p<-p+
            scale_radius(name=name.var, breaks=mybreaks,guide="none")
        }
         p<-p+

        scale_colour_gradientn (colours=getcolhabs(newcolhabs,col.palette,100), guide="none")+

        guides(size = guide_legend(override.aes = list(colour = as.list( scales::col_numeric(getcolhabs(newcolhabs,col.palette,100), domain = NULL)(mybreaks)),size=scales::rescale(mybreaks,c(1,cex.pt)))))

      }
    }
  }
  p

  #p<-p  +  scale_size(name=name.var, range=c(0,cex.pt), breaks=mybreaks)

if(isTRUE(showguides)){
  xcoords<-pretty(coords[,1])
  ycoords<-pretty(coords[,2])
  p<-p+geom_hline(yintercept=ycoords,color = gray(.5), linetype = "dashed", size = .15)+
    geom_vline(xintercept =xcoords,color = gray(.5), linetype = "dashed", size = .15)
}
p<-p+
  #guides( colour = guide_legend())+

  annotation_scale(location = "br", width_hint = width_hint,text_cex=cex_scabar,height  =unit(cex_scabar/4,"cm")) +
  annotation_north_arrow(location = "tl",
                         which_north = "true",
                         width = unit(keyscale, "pt"),
                         height  = unit(keyscale, "pt"),
                         pad_x = unit(.1, "in"),
                         pad_y = unit(0.1, "in"),
                         style = north_arrow_fancy_orienteering) +
  coord_sf(xlim = c(bbox[1,]), ylim = c(bbox[2,]), expand = FALSE) +


  xlab("Longitude") +
  ylab("Latitude") +
  ggtitle(main, subtitle = subtitle) +
  theme(panel.grid.major = element_blank(),
        panel.background=element_rect(fill=NA, color="white"),
        panel.border = element_rect(fill=NA,color="black", size=0.5, linetype="solid"),
        legend.key.size = unit(key.height, 'pt'),
        axis.line=element_line(),
        axis.text=element_text(size=cex.axes),
        axis.title=element_text(size=cex.lab,face="bold"),
        plot.title=element_text(size=cex.main),
        plot.subtitle=element_text(size=cex.sub),
        legend.text=element_text(size=cex.leg),
        legend.title=element_text(size=cex.leg))
if(!is.null(factors)){

  geopoint0<-cbind(coords[rownames(data),],factors)
  colnames(geopoint0)<-c("x","y","factors")
  p<-p+  geom_text( data=geopoint0, aes(x=x, y=y, label=factors,),size=cex.fac,colour=colfac)
}
if(isTRUE(showcoords)){
  geopoint0<-cbind(coords[rownames(data),],data[,get])
  colnames(geopoint0)<-c("x","y","pop")
  p<-p+geom_point( data=geopoint0, aes(x=x, y=y),  size=cex.coords, pch=3, colour=col.coords)

}




if(!is.null(extralayers)){
  if(!is.null(data_depth)){
    point_layer<-p$layers[ grep("points",  names(p$layers))]
    old_layer<-p$layers[-grep("points",  names(p$layers))]
    new_p <- append(old_layer, point_layer, after=data_depth-1)
    p$layers<-new_p}

  }

p}


#' @export
mylighten <- function(color, factor = 0.5) {
  if ((factor > 1) | (factor < 0)) stop("factor needs to be within [0,1]")
  col <- col2rgb(color)
  col <- col + (255 - col)*factor
  col <- rgb(t(col), maxColorValue=255)
  col
}
#' @export
to_spatial<-function(coords,  crs.info="+proj=longlat +datum=WGS84 +no_defs"){
  suppressWarnings({
    colnames(coords)[1:2]<-c("Long","Lat")
    coordinates(coords)<-~Long+Lat
    proj4string(coords) <-CRS(crs.info)
    return(coords)
  })
}
inline<-function (x) {
  tags$div(style="display:inline-block; margin: 0px", x)
}
#' @export
inline2<-function (x) {
  tags$div(style="display:inline-block; margin-top: -100px", x)
}
#' @export



scale_color_matlab<-function (..., alpha = 1, begin = 0, end = 1, direction = 1,discrete = FALSE, option = "D",palette=c(
  "turbo",'viridis', 'plasma',"Rushmore1","FantasticFox1",'Grays',"heat",'Purples',"Blues",'Greens',"black","gray","royalblue", "firebrick","forestGreen",'goldenrod3',"white"),
  newcolhabs)
{

  if (discrete) {
    discrete_scale("colour", palette,  newcolhabs[[palette]], ...)
  } else {
    scale_color_gradientn(colours = do.call( newcolhabs[[palette]],args=list(n=256)),...)

  }
}
#' @export

colsbyprof<-function(depths,cols=c('lightblue','darkblue'))
{
  if(is.null(names(depths))){ids<-1:length(depths)} else {ids<-names(depths)}
  rbPal <- colorRampPalette(cols)
  maxdepth<-max(depths)
  profcols <- rbPal(maxdepth)[as.numeric(cut(depths,breaks = depths))]
  lev<-  pretty(depths)
  proflev <- rbPal(max(lev))[as.numeric(cut(lev,breaks = max(lev)))]
  attr(profcols, 'prettydepth')<-lev
  attr(profcols, 'prettycols')<-proflev
  return(profcols)}
#' @export


scale_color_2<-function (palette="viridis",newcolhabs)
{
  newcolhabs[[palette]](256)
}

