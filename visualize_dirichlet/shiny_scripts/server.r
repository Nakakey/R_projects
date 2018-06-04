shinyServer(function(input, output,session) {

    output$output01 <- renderPlotly({
      a1 <- input$alpha1
      a2 <- input$alpha2
      a3 <- input$alpha3
      al <- c(a1, a2, a3)
      dens <- apply(gri, 1, function(x)ddirichlet(x, al))
      dens[is.na(dens)] <- 0
      dens[is.infinite(dens)] <- 0
      surf <- cbind(gri, dens)
      df.list <- list(mu1 = x, mu2 = y, likelihood = matrix(dens, nrow = num_grid, ncol = num_grid), type = "surface")
      tmp <- 1 - df.list$mu1 - df.list$mu2
      custom_txt <- paste0("mu1:", round(rep(df.list$mu1, times = num_grid),4),
                    "</br>mu2:", round(rep(df.list$mu2, times = num_grid),4), # correct break syntax
                    "</br>mu3:", round(rep(ifelse(tmp >= 0, tmp, 0), times = num_grid),4),
                    "</br>likelihood:", round(df.list$likelihood,4)) %>%
      matrix(num_grid, num_grid)

      plot_ly(x= ~df.list$mu1, y = ~df.list$mu2, z= ~df.list$likelihood,
              type = "surface",
              text = ~custom_txt,
              hoverinfo = "text + x",
              width = "1280px", height = "960px"
              ) %>%
        layout(title = "Dirichlet distribution",
               scene = list(xaxis = list(title = "coords1"),
               yaxis = list(title = "coords2"),
               zaxis = list(title = "likelihood")))
    })


    ##dipole result visualization
    if(0){
	output$output01 <- renderRglwidget({
        nrow_pf <- nrow(dat[[1]]$mean)
        dat2 <- list.map(dat,mean[nrow_pf,])
        particles <- list.map(dat,mean)
        num_col <- nrow(dat[[1]]$mean)
        num_particle <- length(particles)
        particle = do.call(rbind,particles)
        
        plot3d(point_grid,xlim=c(-brain_size,brain_size),ylim=c(-brain_size,brain_size),zlim=c(-brain_size,brain_size),box=F,axes=F,col=rgb(0.2,0.3,0.5),alpha=0.3,xlab="",ylab="",zlab="",type="l")
        ax = unique(grid_cortex[,1])
        height = matrix(grid_cortex[,3],nrow=length(ax))
        rgl.surface(x=ax,z=ax,y=height,coords=c(1,3,2))
        rgl.points(particle[,1:3],col=col_alpha[num_col],size=5)

        ##grand-truth plot
        gt_t <- t(sapply(gts,function(x)x[,time_step]))
        rgl.points(gt_t[,1:3],zlim=c(-brain_size,brain_size),col=1,size=25,pch=19)

        rgl.bg(color="white")
        rgl.lines(rbind(c(0,0,0),c(0,brain_size,0)),col=1,lwd=10)
        rglwidget()
	})

  res_info <- reactive({
    time_step <- input$timestep
    if(input$type=="prior"){
      num <- length(res_prior[[time_step]])
      nums_dipole <- sapply(1:num,function(x)nrow(res_prior[[time_step]][[x]][[1]]$mean))
      nums_pt <- sapply(res_prior[[time_step]],length)
    }else{
      num <- length(res_posterior[[time_step]])
      nums_dipole <- sapply(1:num,function(x)nrow(res_posterior[[time_step]][[x]][[1]]$mean))
      nums_pt <- sapply(res_posterior[[time_step]],length)
    }
    dat <- data.frame(index=1:num,dipole=nums_dipole,num_pt=nums_pt)
  })

  output$info <- renderTable(res_info())
    }

})

