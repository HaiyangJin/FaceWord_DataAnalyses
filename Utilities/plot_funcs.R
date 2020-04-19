
# function to plot results in FaceWord project
plot_uni <- function(df, contrL, contrR, roi){

  # significance 
  nCon = 16
  nGroup = 4
  sig_uni <- rep(1, nCon)
  sig_uni[seq(1, nCon, nGroup)] <- c(signif(as.data.frame(contrL)[c(5, 11), "p.value"], nDigitals),
                                     signif(as.data.frame(contrR)[c(5, 11), "p.value"], nDigitals))
  ast <- sig_ast(sig_uni)
  
  # split the data into two parts by FaceWord
  fwLevels = as.character(unique(df$FaceWord))
  xaxislabel = toTitleCase(unique(as.character(df$Layout)))
  
  # annotations for ROI and hemi
  dat_text <- data.frame(
    Hemisphere = c("left", "right"),
    label = paste0(c("L", "R"), toupper(roi)),
    x     = c(0.75, 4.25),
    y     = 2.4
  )

  # general codes to create the plots
  uni <- function(subdf, subsig, half) {

    ggplot(data = filter(df, FaceWord == fwLevels[half]), aes(y = emmean, x = Layout, group = Hemisphere)) + # set the data, varialbes for x and y axises
      geom_col(position = "dodge", width = 0.5, fill = "#CDCDC8") +  # position of columns and countour of columns
      facet_grid(. ~ Hemisphere) +
      geom_errorbar(mapping = aes(ymin = lower.CL, ymax = upper.CL), linetype = 1,  # set the error bar
                    show.legend = FALSE, width = 0.2, alpha = .5,
                    position = position_dodge(width=0.9)) +
      scale_x_discrete(labels = xaxislabel) +
      scale_y_continuous(expand= c(0, 0), limits = c(0, activationUL), breaks = seq(0, 3, .5)) +  # remove the space between columns and x axis
      labs(title = toTitleCase(fwLevels[half]), x = "Configuration", y = "Beta values") +  # set the names for main, x and y axises 
      geom_text(label = subsig, size = 8, nudge_y = 0.5, nudge_x = 0.5) + # add starts to the significant columns
      geom_text(data = dat_text, mapping = aes(x = x, y = y, label = label), size = 6.5, fontface = "bold") +
      theme_bw() +
      theme(
        plot.title = element_text(lineheight=.8, face="bold", size = 24, hjust = 0.5, vjust = -1.5),
        axis.text.x = element_text(face = "bold", size = 16),
        axis.text.y = element_text(size = 13),
        axis.title.x = element_text(face = "bold", size = 20, vjust = -2), # the size of the texts in plot
        axis.title.y = element_text(size = 17, vjust = 2.5), # the size of the texts in plot
        # axis.text.x = element_text(angle = 45, vjust = 0.5),
        panel.border = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
        axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'), # , arrow = arrow(length = unit(0.3, "cm"))
        # remove the facet background color
        strip.text = element_blank(),
        strip.background = element_blank(),
        strip.placement = "outside",
        plot.margin = margin(5, 5, 15, 8, unit = "pt")
      )
  }
  
  # the first plot (face or English)
  plot_1 <- uni(df, ast[c(1:4, 9:12)], 1) + 
    theme(axis.title.x = element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
          axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'),
          ) 
  # the second plot (word or Chinese)
  plot_2 <- uni(df, ast[c(5:8, 13:16)], 2)
  
  # combine the two plots
  ggarrange(plot_1, plot_2, nrow = 2)
  
}
