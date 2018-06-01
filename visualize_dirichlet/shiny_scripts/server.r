shinyServer(function(input, output,session) {

    output$output02 <- renderPlot({
      status <- input$type_p
      if(status=="Min"){
    df_gt <- as.data.frame(cbind(True=gt,Estimate=est,time_step=1:length(gt)))
    }else if(status=="MAP"){
      df_gt <- as.data.frame(cbind(True=gt,Estimate=est_MAP,time_step=1:length(gt)))
    }else if(status=="full"){
      df_gt <- as.data.frame(cbind(True=gt,Estimate_min=est,Estimate_map=est_MAP,time_step=1:length(gt)))
    }else{
    df_gt <- as.data.frame(cbind(True=gt,time_step=1:length(gt)))
    }
    df_gt <- melt(df_gt,id.vars="time_step")

    ##make plot
    g <- ggplot(NULL) + theme_bw() +theme(plot.title = element_text(hjust = 0.5,size=25),text = element_text(size = 20),axis.title = element_text(size = 20),axis.text.x = element_text(size = 15),axis.text.y = element_text(size = 15))

    g <- g +  geom_point(data=df,aes(x=time_step,y=result,size=weight)) +
    geom_line(data=df_gt,aes(x=time_step,y=value,group=variable,colour=variable,linetype=variable),size=2) + geom_vline(aes(xintercept=input$timestep),col="green")
    plot(g) 
    })


	output$output01 <- renderRglwidget({
		time_step <- input$timestep
        ind_model <- input$num_model
        if(input$type=="prior"){dat <- res_prior[[time_step]][[ind_model]]
        }else{dat <- res_posterior[[time_step]][[ind_model]]}
        
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

})

