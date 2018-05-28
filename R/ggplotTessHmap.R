ggplotTessHmap <-
function(plot_gg,hvt.results, line.width, color.vec,child.level, pch1 = 19, centroid.size = 3){
  

  del_results <- hvt.results[[1]][seq(1:child.level)]
  #plot(del_results[[1]][[1]], wlines = "tess", lty = 1, lwd = line.width[1], xlab = "", ylab = "")

  
  
  for(lev in length(del_results):1){
    #for(lev1 in 1: length(del_results[[lev]])){
      #df = data.frame(del_results[[lev]][[lev1]]$summary$x,del_results[[lev]][[lev1]]$summary$y)
      #colnames(df) <- c("x","y")
      df_points <- do.call(rbind,lapply(del_results[[lev]],FUN = function(x) x$summary)) 
      
      seg_df <- do.call(rbind,lapply(del_results[[lev]],FUN = function(x) x$dirsgs)) %>% dplyr::mutate(Legend = paste("Level",lev)
                        )
      
      plot_gg <- plot_gg + ggplot2::geom_segment(ggplot2::aes_string(x="x1",y="y1",xend="x2",yend="y2",color="Legend"),
                                                 size =line.width[lev],
                                                 data = seg_df,
                                                 linetype = 1
      ) + ggplot2::scale_color_manual(values = color.vec) +
        ggplot2::geom_point(data = df_points,
                            ggplot2::aes_string(x="x",y="y"),
                            pch=21,
                            size =(centroid.size/lev),
                            fill = color.vec[lev]
                            ) 
        
                            #ggplot2::theme_bw() +  
                            #ggplot2::theme(
                            #  plot.background = ggplot2::element_blank()
                            #  ,panel.grid.major = ggplot2::element_blank()
                            #  ,panel.grid.minor = ggplot2::element_blank()
                             
    
  }
  return(plot_gg)
}
