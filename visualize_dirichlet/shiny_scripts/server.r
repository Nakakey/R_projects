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
      custom_txt <- paste("</br>mu1:", round(gri$x,4),
                    "</br>mu2:", round(gri$y,4),
                    "</br>mu3:", round(gri$z,4),
                    "</br>likelihood:", round(dens,4))

      plot_ly(x= ~gri_xy_ct[,1], y = ~gri_xy_ct[,2], z= ~dens,
              intensity = ~dens,
              type = "mesh3d",
              text = ~custom_txt,
              hoverinfo = "text",
              width = "1280px", height = "960px"
              ) %>%
        layout(title = "Dirichlet distribution",
               scene = list(xaxis = list(title = ""),
               yaxis = list(title = ""),
               zaxis = list(title = "likelihood")))
    })
})
