
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
  fwLevels <- levels(df$FaceWord)
  xaxislabel <- toTitleCase(levels(df$Layout))
  
  # annotations for ROI and hemi
  dat_text <- data.frame(
    Hemisphere = c("left", "right"),
    label = paste0(c("L", "R"), toupper(roi)),
    x     = c(0.85, 4.15),
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
      scale_y_continuous(expand= c(0, 0), breaks = seq(0, 3, .5)) +  # remove the space between columns and x axis , limits = c(0, activationUL), 
      labs(title = toTitleCase(fwLevels[half]), x = "Configuration", y = "Beta values") +  # set the names for main, x and y axises 
      coord_cartesian(ylim = c(0, activationUL)) +
      geom_text(label = subsig, size = 8, nudge_y = 0.5, nudge_x = 0.5) + # add stars to the significant columns
      geom_text(data = dat_text, mapping = aes(x = x, y = y, label = label), size = 6.5, fontface = "bold") +
      theme_bw() +
      theme(
        plot.title = element_text(lineheight=.8, face="bold", size = 24, hjust = 0.5, vjust = -1.5),
        plot.margin = margin(5, 5, 15, 8, unit = "pt"),
        text = element_text(colour="black"),
        axis.text = element_text(colour="black"),
        axis.text.x = element_text(face = "bold", size = 16),
        axis.text.y = element_text(size = 13),
        axis.title.x = element_text(face = "bold", size = 20, vjust = -2), # the size of the texts in plot
        axis.title.y = element_text(size = 17, vjust = 2.5), # the size of the texts in plot
        axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
        axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'), # , arrow = arrow(length = unit(0.3, "cm"))
        panel.border = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.spacing = unit(2, "lines"),
        # remove the facet background color
        strip.text = element_blank(),
        strip.background = element_blank(),
        strip.placement = "outside"
      )
  }
  
  # the first plot (face or English)
  plot_1 <- uni(df, ast[c(1:4, 9:12)], 1) + 
    theme(axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
          axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'),
    ) 
  # the second plot (word or Chinese)
  plot_2 <- uni(df, ast[c(5:8, 13:16)], 2)
  
  # combine the two plots
  ggarrange(plot_1, plot_2, nrow = 2,
            heights = c(0.89, 1))  # ratio between the two subplots
}

# function to plot decode results
plot_decode <- function(df, roi) {
  
  # parse ClassifyPair into Stimuli and Layout
  thisdf <- df %>% 
    separate(ClassifyPair, c("Stimuli1", "Layout1", "Stimuli2", "Layout2")) %>% 
    mutate(Stimuli1 = ifelse(Stimuli1 %in% c("face", "word"), paste0(Stimuli1, "s"), Stimuli1),
           Stimuli2 = ifelse(Stimuli2 %in% c("face", "word"), paste0(Stimuli2, "s"), Stimuli2),
           Stimuli = ifelse(Stimuli1 == Stimuli2, Stimuli1, 
                            paste(Stimuli1, Stimuli2, sep = "\nvs.\n")),
           Layout = ifelse(Layout1 == Layout2, Layout1,
                           paste(toTitleCase(Layout1), toTitleCase(Layout2), sep = "\nvs.\n"))) %>% 
    select(-c(Stimuli1, Stimuli2, Layout1, Layout2))
  
  # save the df for intact English vs. intact Chinese
  df_0 <- filter(thisdf, Layout == "intact")
  xaxislabel <- strsplit(unique(df_0$Stimuli), "\nvs.\n")[[1]]
  
  # annotations for ROI and hemi
  dat_text <- data.frame(
    Hemisphere = c("left", "right"),
    label = paste0(c("L", "R"), toupper(roi)),
    x     = c(0.8, 2.2),
    y     = 1.05
  )
  
  # general codes to create the plots
  decode <- function(subdf, half) {
    
    thissubdf <- filter(subdf, Stimuli == xaxislabel[half])
    
    ggplot(data = thissubdf, 
           aes(y = mean, x = Layout)) + # set the data, varialbes for x and y axises
      geom_col(position = "dodge", fill = "#CDCDC8", width = 0.5) +  # position of columns and countour of columns
      facet_grid(. ~ Hemisphere) +
      geom_errorbar(mapping = aes(ymin = lower.CL, ymax = upper.CL), linetype = 1,  # set the error bar
                    show.legend = FALSE, width = 0.25, alpha = .5,
                    position = position_dodge(width=0.9)) +
      geom_hline(yintercept = 0.5, linetype = 5, alpha = 0.5) +  # add the line for 0.5 and 1 (y)
      scale_y_continuous(expand= c(0, 0), breaks = seq(0, 1, .25)) +  # remove the space between columns and x axis
      coord_cartesian(ylim = c(0, 1.1)) +
      labs(title = toTitleCase(xaxislabel[half]), x = "Classification Pairs", y = "Accuracy") +  # set the names for main, x and y axises
      geom_text(label = sig_ast(thissubdf$p), size = 7, nudge_y = 0.15) + # add starts to the significant columns
      geom_text(data = dat_text, mapping = aes(x = x, y = y, label = label), size = 6.5, fontface = "bold") +
      theme_bw() +
      theme(
        plot.title = element_text(lineheight=.8, face="bold", size = 24, hjust = 0.5, vjust = -1.5),
        plot.margin = margin(5, 5, -20, 40, unit = "pt"),
        text = element_text(colour="black"),
        axis.text = element_text(colour="black"),
        axis.text.x = element_text(face = "bold", size = 16),
        axis.text.y = element_text(size = 13),
        axis.title.x = element_text(face = "bold", size = 20, vjust = -2), # the size of the texts in plot
        axis.title.y = element_text(size = 17, vjust = 2.5), # the size of the texts in plot
        axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
        axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'), # , arrow = arrow(length = unit(0.3, "cm"))
        # axis.text.x = element_text(angle = 45, vjust = 0.5),
        panel.border = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.spacing = unit(2, "lines"),
        # remove the facet background color
        strip.text = element_blank(),
        strip.background = element_blank(),
        strip.placement = "outside"
      )
  }
  
  # the first plot (face or English)
  plot_1 <- decode(thisdf, 1) + 
    theme(axis.title.x = element_blank(),
          axis.text.x = element_text(color = "white"),
          axis.ticks.x = element_blank(),
          axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
          axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid')
    ) 
  # the second plot (word or Chinese)
  plot_2 <- decode(thisdf, 2) +
    theme(
      axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
      axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'),
      plot.margin = margin(5, 5, 15, 40, unit = "pt")
    )
  
  # plot intact 1 vs 2
  plot_0 <-
    ggplot(df_0, aes(y = mean, x = Stimuli)) +
    geom_col(position = "dodge", width = .5, fill = "#CDCDC8") +
    facet_grid(. ~ Hemisphere) +
    geom_errorbar(mapping = aes(ymin = lower.CL, ymax = upper.CL), linetype = 1,  # set the error bar
                  show.legend = FALSE, width = 0.25, alpha = .5,
                  position = position_dodge(width=0.9)) +
    geom_hline(yintercept = 0.5, linetype = 5, alpha = 0.5) +  # add the line for 0.5 and 1 (y)
    scale_x_discrete(labels = toTitleCase(df_0$Stimuli)) +
    scale_y_continuous(expand= c(0, 0), breaks = seq(0, 1, .25)) +  # remove the space between columns and x axis
    coord_cartesian(ylim = c(0, 1.1)) +
    labs(title = "Intact stimuli", x = "Classification Pairs", y = "Accuracy") +  # set the names for main, x and y axises
    geom_text(label = sig_ast(df_0$p), size = 7, nudge_y = 0.15) + # add starts to the significant columns
    geom_text(data = dat_text, mapping = aes(x = c(.8, 1.2), y = y, label = label), size = 6.5, fontface = "bold") +
    theme_bw() +
    theme(
      plot.title = element_text(lineheight=.8, face="bold", size = 24, hjust = 0.5, vjust = -1),
      plot.margin = margin(5, 170, 60, 40, unit = "pt"),
      text = element_text(colour="black"),
      axis.text = element_text(colour="black"),
      axis.text.x = element_text(face = "bold", size = 16),
      axis.text.y = element_text(size = 13),
      axis.title.x = element_text(face = "bold", size = 20, vjust = -2), # the size of the texts in plot
      axis.title.y = element_text(size = 17, vjust = 2.5), # the size of the texts in plot
      axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
      axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'), # , arrow = arrow(length = unit(0.3, "cm"))
      # axis.text.x = element_text(angle = 45, vjust = 0.5),
      panel.border = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      panel.spacing = unit(2, "lines"),
      # remove the facet background color
      strip.text = element_blank(),
      strip.background = element_blank(),
      strip.placement = "outside",
    )
  
  # combine the two plots
  ggarrange(plot_0, plot_1, plot_2, nrow = 3, ncol = 1, 
            heights = c(1.12, 0.86, 1),
            labels = c("A", "B", ""), 
            font.label = list(size = 24, face = "bold"))
}

plot_simi <- function(df, roi) {
  # parse ClassifyPair into Stimuli and Layout
  thisdf <- df %>% 
    separate(Combination, c("Stimuli1", "Part1", "Ratio1", "Stimuli2", "Part2", "Ratio2")) %>% 
    mutate(Stimuli1 = ifelse(Stimuli1 %in% c("face", "word"), paste0(Stimuli1, "s"), Stimuli1),
           Stimuli2 = ifelse(Stimuli2 %in% c("face", "word"), paste0(Stimuli2, "s"), Stimuli2),
           Stimuli = ifelse(Stimuli1 == Stimuli2, Stimuli1, 
                            paste(Stimuli1, Stimuli2, sep = "\nvs.\n")),
           RatioC1 = paste0(str_remove(Part1, "0"), "(.", Ratio1, ")"),
           RatioC2 = paste0(str_remove(Part2, "0"), "(.", Ratio2, ")"),
           Combination = paste(toTitleCase(RatioC1), toTitleCase(RatioC2), sep = "\n&\n")) %>% 
    select(-c(Stimuli1, Stimuli2, Part1, Part2))
  
  # save the df for intact English vs. intact Chinese
  xaxislabel <- unique(thisdf$Stimuli)
  
  if (xaxislabel[1] %in% c("English", "Chinese")) {
    xaxislabel = c("English", "Chinese")
  }
  
  # annotations for ROI and hemi
  dat_text <- data.frame(
    Hemisphere = c("left", "right"),
    label = paste0(c("L", "R"), toupper(roi)),
    x     = c(0.8, 3.2),
    y     = 1.05
  )
  
  # general codes to create the plots
  simi <- function(subdf, half) {
    
    thisdf <- filter(subdf, Stimuli == xaxislabel[half])
    
    ggplot(data = thisdf, 
           aes(y = mean, x = Combination)) + # set the data, varialbes for x and y axises
      geom_col(position = "dodge", fill = "#CDCDC8", width = 0.5) +  # position of columns and countour of columns
      facet_grid(. ~ Hemisphere) +
      geom_errorbar(mapping = aes(ymin = lower.CL, ymax = upper.CL), linetype = 1,  # set the error bar
                    show.legend = FALSE, width = 0.25, alpha = .5,
                    position = position_dodge(width=0.9)) +
      geom_hline(yintercept = 0.5, linetype = 5, alpha = 0.5) +  # add the line for 0.5 and 1 (y)
      scale_y_continuous(expand= c(0, 0), breaks = seq(0, 1, .25)) +  # remove the space between columns and x axis
      coord_cartesian(ylim = c(0, 1.1)) +
      labs(title = toTitleCase(xaxislabel[half]), x = "Combinations", y = "Rate of decoding as exchange") +  # set the names for main, x and y axises
      geom_text(label = sig_ast(thisdf$p), size = 7, nudge_y = 0.15) + # add starts to the significant columns
      geom_text(data = dat_text, mapping = aes(x = x, y = y, label = label), size = 6.5, fontface = "bold") +
      theme_bw() +
      theme(
        plot.title = element_text(lineheight=.8, face="bold", size = 24, hjust = 0.5, vjust = -1.5),
        plot.margin = margin(5, 5, -20, 8, unit = "pt"),
        text = element_text(colour="black"),
        axis.text = element_text(colour="black"),
        axis.text.x = element_text(face = "bold", size = 12),
        axis.text.y = element_text(size = 13),
        axis.title.x = element_text(face = "bold", size = 20, vjust = -2), # the size of the texts in plot
        axis.title.y = element_text(size = 15, vjust = 2.5), # the size of the texts in plot
        axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
        axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'), # , arrow = arrow(length = unit(0.3, "cm"))
        # axis.text.x = element_text(angle = 45, vjust = 0.5),
        panel.border = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.spacing = unit(2, "lines"),
        # remove the facet background color
        strip.text = element_blank(),
        strip.background = element_blank(),
        strip.placement = "outside"
      )
  }
  
  # the first plot (face or English)
  plot_1 <- simi(thisdf, 1) + 
    theme(axis.title.x = element_blank(),
          axis.text.x = element_text(color = "white"),
          axis.ticks.x = element_blank(),
          axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
          axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid')
    ) 
  # the second plot (word or Chinese)
  plot_2 <- simi(thisdf, 2) +
    theme(
      axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
      axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'),
      plot.margin = margin(5, 5, 15, 8, unit = "pt")
    )
  
  # combine the two plots
  ggarrange(plot_1, plot_2, nrow = 2,
            heights = c(0.86, 1))
}


# function to plot results in FaceWord project
plot_uni_vwfa <- function(df, contrL, roi){
  
  # significance 
  nCon = 16
  nGroup = 4
  sig_uni <- rep(1, nCon)
  sig_uni[seq(1, nCon, nGroup)] <- c(signif(as.data.frame(contrL)[c(5, 11), "p.value"], nDigitals))
  ast <- sig_ast(sig_uni)
  
  # split the data into two parts by FaceWord
  fwLevels <- levels(df$FaceWord)
  xaxislabel <- toTitleCase(levels(df$Layout))
  
  # annotations for ROI and hemi
  dat_text <- data.frame(
    Hemisphere = c("left"),
    label = toupper(roi),
    x     = c(0.85),
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
      scale_y_continuous(expand= c(0, 0), breaks = seq(0, 3, .5)) +  # remove the space between columns and x axis , limits = c(0, activationUL), 
      labs(title = toTitleCase(fwLevels[half]), x = "Configuration", y = "Beta values") +  # set the names for main, x and y axises 
      coord_cartesian(ylim = c(0, activationUL)) +
      geom_text(label = subsig, size = 8, nudge_y = 0.5, nudge_x = 0.5) + # add stars to the significant columns
      geom_text(data = dat_text, mapping = aes(x = x, y = y, label = label), size = 6.5, fontface = "bold") +
      theme_bw() +
      theme(
        plot.title = element_text(lineheight=.8, face="bold", size = 24, hjust = 0.5, vjust = -1.5),
        plot.margin = margin(5, 5, 15, 8, unit = "pt"),
        text = element_text(colour="black"),
        axis.text = element_text(colour="black"),
        axis.text.x = element_text(face = "bold", size = 16),
        axis.text.y = element_text(size = 13),
        axis.title.x = element_text(face = "bold", size = 20, vjust = -2), # the size of the texts in plot
        axis.title.y = element_text(size = 17, vjust = 2.5), # the size of the texts in plot
        axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
        axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'), # , arrow = arrow(length = unit(0.3, "cm"))
        panel.border = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.spacing = unit(2, "lines"),
        # remove the facet background color
        strip.text = element_blank(),
        strip.background = element_blank(),
        strip.placement = "outside"
      )
  }
  
  # the first plot (face or English)
  plot_1 <- uni(df, ast[c(1:4)], 1) + 
    theme(axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
          axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'),
    ) 
  # the second plot (word or Chinese)
  plot_2 <- uni(df, ast[c(5:8)], 2)
  
  # combine the two plots
  ggarrange(plot_1, plot_2, nrow = 2,
            heights = c(0.89, 1))  # ratio between the two subplots
}

# function to plot decode results
plot_decode_vwfa <- function(df, roi) {
  
  # parse ClassifyPair into Stimuli and Layout
  thisdf <- df %>% 
    separate(ClassifyPair, c("Stimuli1", "Layout1", "Stimuli2", "Layout2")) %>% 
    mutate(Stimuli1 = ifelse(Stimuli1 %in% c("face", "word"), paste0(Stimuli1, "s"), Stimuli1),
           Stimuli2 = ifelse(Stimuli2 %in% c("face", "word"), paste0(Stimuli2, "s"), Stimuli2),
           Stimuli = ifelse(Stimuli1 == Stimuli2, Stimuli1, 
                            paste(Stimuli1, Stimuli2, sep = "\nvs.\n")),
           Layout = ifelse(Layout1 == Layout2, Layout1,
                           paste(toTitleCase(Layout1), toTitleCase(Layout2), sep = "\nvs.\n"))) %>% 
    select(-c(Stimuli1, Stimuli2, Layout1, Layout2))
  
  # save the df for intact English vs. intact Chinese
  df_0 <- filter(thisdf, Layout == "intact")
  xaxislabel <- strsplit(unique(df_0$Stimuli), "\nvs.\n")[[1]]
  
  # annotations for ROI and hemi
  dat_text <- data.frame(
    Hemisphere = c("left"),
    label = toupper(roi),
    x     = c(0.8),
    y     = 1.05
  )
  
  # general codes to create the plots
  decode <- function(subdf, half) {
    
    thissubdf <- filter(subdf, Stimuli == xaxislabel[half])
    
    ggplot(data = thissubdf, 
           aes(y = mean, x = Layout)) + # set the data, varialbes for x and y axises
      geom_col(position = "dodge", fill = "#CDCDC8", width = 0.5) +  # position of columns and countour of columns
      # facet_grid(. ~ Hemisphere) +
      geom_errorbar(mapping = aes(ymin = lower.CL, ymax = upper.CL), linetype = 1,  # set the error bar
                    show.legend = FALSE, width = 0.25, alpha = .5,
                    position = position_dodge(width=0.9)) +
      geom_hline(yintercept = 0.5, linetype = 5, alpha = 0.5) +  # add the line for 0.5 and 1 (y)
      scale_y_continuous(expand= c(0, 0), breaks = seq(0, 1, .25)) +  # remove the space between columns and x axis
      coord_cartesian(ylim = c(0, 1.1)) +
      labs(title = toTitleCase(xaxislabel[half]), x = "Classification Pairs", y = "Accuracy") +  # set the names for main, x and y axises
      geom_text(label = sig_ast(thissubdf$p), size = 7, nudge_y = 0.15) + # add starts to the significant columns
      geom_text(data = dat_text, mapping = aes(x = x, y = y, label = label), size = 6.5, fontface = "bold") +
      theme_bw() +
      theme(
        plot.title = element_text(lineheight=.8, face="bold", size = 24, hjust = 0.5, vjust = -1.5),
        plot.margin = margin(5, 5, -20, 40, unit = "pt"),
        text = element_text(colour="black"),
        axis.text = element_text(colour="black"),
        axis.text.x = element_text(face = "bold", size = 16),
        axis.text.y = element_text(size = 13),
        axis.title.x = element_text(face = "bold", size = 20, vjust = -2), # the size of the texts in plot
        axis.title.y = element_text(size = 17, vjust = 2.5), # the size of the texts in plot
        axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
        axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'), # , arrow = arrow(length = unit(0.3, "cm"))
        # axis.text.x = element_text(angle = 45, vjust = 0.5),
        panel.border = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.spacing = unit(2, "lines"),
        # remove the facet background color
        strip.text = element_blank(),
        strip.background = element_blank(),
        strip.placement = "outside"
      )
  }
  
  # the first plot (face or English)
  plot_1 <- decode(thisdf, 1) + 
    theme(axis.title.x = element_blank(),
          axis.text.x = element_text(color = "white"),
          axis.ticks.x = element_blank(),
          axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
          axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid')
    ) 
  # the second plot (word or Chinese)
  plot_2 <- decode(thisdf, 2) +
    theme(
      axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
      axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'),
      plot.margin = margin(5, 5, 15, 40, unit = "pt")
    )
  
  # plot intact 1 vs 2
  plot_0 <-
    ggplot(df_0, aes(y = mean, x = Stimuli)) +
    geom_col(position = "dodge", width = .5, fill = "#CDCDC8") +
    # facet_grid(. ~ Hemisphere) +
    geom_errorbar(mapping = aes(ymin = lower.CL, ymax = upper.CL), linetype = 1,  # set the error bar
                  show.legend = FALSE, width = 0.25, alpha = .5,
                  position = position_dodge(width=0.9)) +
    geom_hline(yintercept = 0.5, linetype = 5, alpha = 0.5) +  # add the line for 0.5 and 1 (y)
    scale_x_discrete(labels = toTitleCase(df_0$Stimuli)) +
    scale_y_continuous(expand= c(0, 0), breaks = seq(0, 1, .25)) +  # remove the space between columns and x axis
    coord_cartesian(ylim = c(0, 1.1)) +
    labs(title = "Intact stimuli", x = "Classification Pairs", y = "Accuracy") +  # set the names for main, x and y axises
    geom_text(label = sig_ast(df_0$p), size = 7, nudge_y = 0.15) + # add starts to the significant columns
    geom_text(data = dat_text, mapping = aes(x = c(.85), y = y, label = label), size = 6.5, fontface = "bold") +
    theme_bw() +
    theme(
      plot.title = element_text(lineheight=.8, face="bold", size = 24, hjust = 0.5, vjust = -1),
      plot.margin = margin(5, 120, 60, 40, unit = "pt"),
      text = element_text(colour="black"),
      axis.text = element_text(colour="black"),
      axis.text.x = element_text(face = "bold", size = 16),
      axis.text.y = element_text(size = 13),
      axis.title.x = element_text(face = "bold", size = 20, vjust = -2), # the size of the texts in plot
      axis.title.y = element_text(size = 17, vjust = 2.5), # the size of the texts in plot
      axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
      axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'), # , arrow = arrow(length = unit(0.3, "cm"))
      # axis.text.x = element_text(angle = 45, vjust = 0.5),
      panel.border = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      panel.spacing = unit(2, "lines"),
      # remove the facet background color
      strip.text = element_blank(),
      strip.background = element_blank(),
      strip.placement = "outside",
    )
  
  # combine the two plots
  ggarrange(plot_0, plot_1, plot_2, nrow = 3, ncol = 1, 
            heights = c(1.12, 0.86, 1),
            labels = c("A", "B", ""), 
            font.label = list(size = 24, face = "bold"))
}

plot_simi_vwfa <- function(df, roi) {
  # parse ClassifyPair into Stimuli and Layout
  thisdf <- df %>% 
    separate(Combination, c("Stimuli1", "Part1", "Ratio1", "Stimuli2", "Part2", "Ratio2")) %>% 
    mutate(Stimuli1 = ifelse(Stimuli1 %in% c("face", "word"), paste0(Stimuli1, "s"), Stimuli1),
           Stimuli2 = ifelse(Stimuli2 %in% c("face", "word"), paste0(Stimuli2, "s"), Stimuli2),
           Stimuli = ifelse(Stimuli1 == Stimuli2, Stimuli1, 
                            paste(Stimuli1, Stimuli2, sep = "\nvs.\n")),
           RatioC1 = paste0(str_remove(Part1, "0"), "(.", Ratio1, ")"),
           RatioC2 = paste0(str_remove(Part2, "0"), "(.", Ratio2, ")"),
           Combination = paste(toTitleCase(RatioC1), toTitleCase(RatioC2), sep = "\n&\n")) %>% 
    select(-c(Stimuli1, Stimuli2, Part1, Part2))
  
  # save the df for intact English vs. intact Chinese
  xaxislabel <- unique(thisdf$Stimuli)
  
  if (xaxislabel[1] %in% c("English", "Chinese")) {
    xaxislabel = c("English", "Chinese")
  }
  
  # annotations for ROI and hemi
  dat_text <- data.frame(
    Hemisphere = c("left"),
    label = toupper(roi),
    x     = c(0.8),
    y     = 1.05
  )
  
  # general codes to create the plots
  simi <- function(subdf, half) {
    
    thisdf <- filter(subdf, Stimuli == xaxislabel[half])
    
    ggplot(data = thisdf, 
           aes(y = mean, x = Combination)) + # set the data, varialbes for x and y axises
      geom_col(position = "dodge", fill = "#CDCDC8", width = 0.5) +  # position of columns and countour of columns
      # facet_grid(. ~ Hemisphere) +
      geom_errorbar(mapping = aes(ymin = lower.CL, ymax = upper.CL), linetype = 1,  # set the error bar
                    show.legend = FALSE, width = 0.25, alpha = .5,
                    position = position_dodge(width=0.9)) +
      geom_hline(yintercept = 0.5, linetype = 5, alpha = 0.5) +  # add the line for 0.5 and 1 (y)
      scale_y_continuous(expand= c(0, 0), breaks = seq(0, 1, .25)) +  # remove the space between columns and x axis
      coord_cartesian(ylim = c(0, 1.1)) +
      labs(title = toTitleCase(xaxislabel[half]), x = "Combinations", y = "Rate of decoding as exchange") +  # set the names for main, x and y axises
      geom_text(label = sig_ast(thisdf$p), size = 7, nudge_y = 0.15) + # add starts to the significant columns
      geom_text(data = dat_text, mapping = aes(x = x, y = y, label = label), size = 6.5, fontface = "bold") +
      theme_bw() +
      theme(
        plot.title = element_text(lineheight=.8, face="bold", size = 24, hjust = 0.5, vjust = -1.5),
        plot.margin = margin(5, 5, -20, 8, unit = "pt"),
        text = element_text(colour="black"),
        axis.text = element_text(colour="black"),
        axis.text.x = element_text(face = "bold", size = 12),
        axis.text.y = element_text(size = 13),
        axis.title.x = element_text(face = "bold", size = 20, vjust = -2), # the size of the texts in plot
        axis.title.y = element_text(size = 15, vjust = 2.5), # the size of the texts in plot
        axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
        axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'), # , arrow = arrow(length = unit(0.3, "cm"))
        # axis.text.x = element_text(angle = 45, vjust = 0.5),
        panel.border = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.spacing = unit(2, "lines"),
        # remove the facet background color
        strip.text = element_blank(),
        strip.background = element_blank(),
        strip.placement = "outside"
      )
  }
  
  # the first plot (face or English)
  plot_1 <- simi(thisdf, 1) + 
    theme(axis.title.x = element_blank(),
          axis.text.x = element_text(color = "white"),
          axis.ticks.x = element_blank(),
          axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
          axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid')
    ) 
  # the second plot (word or Chinese)
  plot_2 <- simi(thisdf, 2) +
    theme(
      axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
      axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'),
      plot.margin = margin(5, 5, 15, 8, unit = "pt")
    )
  
  # combine the two plots
  ggarrange(plot_1, plot_2, nrow = 2,
            heights = c(0.86, 1))
}

