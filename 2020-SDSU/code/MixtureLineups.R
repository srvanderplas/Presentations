# Identify the best combination of a set of colors/shapes based on a 
# provided distance matrix
best.combo <- function(ngroups = 3, palette, dist.matrix){
  # check distance matrix
  if (nrow(dist.matrix) != length(palette) | 
      ncol(dist.matrix) != length(palette)) {
    stop(paste0("The distance matrix does not match the size of the palette. ",
                "It should be ", length(palette), "x", length(palette), "."))
  }
  if ( (sum(dist.matrix < 0) > 0) ) {
    stop("Distance matrix cannot have negative entries.")
  }
  
  require(combinat)
  clist <- t(combn(1:length(palette), ngroups))
  pairwise.combos <- t(combn(1:ngroups, 2))
  res <- rowSums(apply(pairwise.combos, 1, function(i){
    diag(as.matrix(dist.matrix[clist[,i[1]], clist[,i[2]]]))
  }))
  
  return(palette[clist[which.max(res),]])
}

# Simulate clustered data: N points, in K clusters, with 
# sd.cluster within-cluster standard deviation
sim.clusters <- function(K, N, sd.cluster=.3){  
  xc <- sample(1:K, replace = F)
  yc <- sample(1:K, replace = F)
  xc <- jitter(xc, amount = .2)
  yc <- jitter(yc, amount = .2)
  while (cor(xc,yc) < .25 | cor(xc,yc) > .75) {
    xc <- sample(1:K, replace = F)
    yc <- sample(1:K, replace = F)
    xc <- jitter(xc, amount = .2)
    yc <- jitter(yc, amount = .2)
  }
  
  yc <- scale(yc)
  xc <- scale(xc)
  
  groups <- sample(K, N, replace = TRUE, 
                   prob = abs(rnorm(K, mean = 1/K, sd = 0.5 / K ^ 2)))
  
  yerr <- rnorm(N, sd = sd.cluster)
  xerr <- rnorm(N, sd = sd.cluster)
  
  cluster.data <- data.frame(x = xc[groups] + xerr, 
                             y = yc[groups] + yerr, 
                             group = groups)
  return(cluster.data)
}

# Simulate linear data: N points with (sd.trend)^2 error variance
sim.line <- function(K = 3, N = 45, sd.trend = .3){
  # Simulate data from line
  line.data <- data.frame(x = jitter(seq(-1, 1, length.out = N)), y = 0)
  line.data$y <- line.data$x + rnorm(N, 0, sd.trend)
  
  return(line.data)
}

# Simulate data which is a mixture between clusters and lines
# Calls sim.clusters and sim.line
mixture.sim <- function(lambda, K, N, sd.trend=.3, sd.cluster=.3){
  cluster.data <- sim.clusters(K = K, N = N, sd.cluster = sd.cluster)
  cluster.data[,c("x", "y")] <- scale(cluster.data[,c("x", "y")])
  line.data <- sim.line(K = K, N = N, sd.trend = sd.trend)
  line.data[,c("x", "y")] <- scale(line.data[,c("x", "y")])
  
  ll <- rbinom(n = N, size = 1, prob = lambda)  # one model or the other
  mix.data <- data.frame(
    x = ll*cluster.data$x + (1 - ll)*line.data$x,  
    y = ll*cluster.data$y + (1 - ll)*line.data$y,
    group = as.numeric(cluster.data$group)  
  )
  
  mix.data[,c("x", "y")] <- scale(mix.data[,c("x", "y")])
  
  mix.data$group <- cutree(hclust(dist(mix.data[,c("x", "y")])), k = K) 
  # grouping by the best K clusters
  
  return(mix.data)
}

# Generate data for an entire lineup
gen.data <- function(input){
  # Position of target plots
  pos <- sample(1:20, size = 2)
  
  # Trend target data
  trenddata <- mixture.sim(lambda = 0, 
                           N = input$N, 
                           K = input$K, 
                           sd.trend = input$sd.trend, 
                           sd.cluster = input$sd.cluster)
  # Cluster target data
  clusterdata <- mixture.sim(lambda = 1, 
                             N = input$N, 
                             K = input$K, 
                             sd.trend = input$sd.trend, 
                             sd.cluster = input$sd.cluster)
  
  # Null data
  # Need 19 data sets so that nullabor works; then replace the data at 
  # cluster target position with cluster target data
  nulldata <- rdply(19, function(.sample) 
    mixture.sim(lambda = .5, 
                N = input$N, 
                K = input$K, 
                sd.cluster = input$sd.cluster, 
                sd.trend = input$sd.trend
    ))
  
  data <- lineup(true = trenddata, pos = pos[1], n = 20, samples = nulldata)
  data <- rbind.fill(
    subset(data, .sample != pos[2]), 
    cbind(.sample = pos[2], clusterdata))
  
  data$target1 <- pos[1]
  data$target2 <- pos[2]
  
  data
}

# Generate data for test plots to determine if participants are acting reasonably
gen.test.data <- function(input, type = NULL, N = 20){
  
  if ((is.null(type))) {
    if ("type" %in% names(input)) {
      type <- input$type
    } else {
      warning("type not specified - proceeding using 'trend'")
      type <- 'trend'
    }
  }
  
  pos <- sample(1:N)[1]
  
  # Nulls
  nulldata <- rdply(N - 1, function(.sample) 
    mixture.sim(lambda = .5, 
                N = input$N, 
                K = input$K, 
                sd.cluster = input$sd.cluster, 
                sd.trend = input$sd.trend
    ))
  
  # Signal 
  if ((type == 'trend')) {
    signaldata <- mixture.sim(lambda = 0, 
                              N = input$N, 
                              K = input$K, 
                              sd.trend = input$sd.trend, 
                              sd.cluster = input$sd.cluster)
  } else {
    signaldata <- mixture.sim(lambda = 1, 
                              N = input$N, 
                              K = input$K, 
                              sd.trend = input$sd.trend, 
                              sd.cluster = input$sd.cluster)
  }
  
  data <- lineup(true = signaldata, pos = pos, n = N, samples = nulldata)
  
  data$target1 <- pos
  
  data
}

# Generate data for null plots to determine if participants are acting reasonably
gen.null.data <- function(input, N = 20){
  # Nulls
  nulldata <- purrr::map_df(1:N, function(.sample) 
    mixture.sim(lambda = .5, 
                N = input$N, 
                K = input$K, 
                sd.cluster = input$sd.cluster, 
                sd.trend = input$sd.trend
    ), .id = ".sample")
  
  nulldata
}

# Calculate trend/group/gini statistics for any given dataset
eval.df <- function(df){
  data.frame(
    line = summary(lm(y~x, data = df))$r.squared, 
    group = cluster(df),
    gini = gini(df$group)
  )
}

# Calculate statistics for each lineup
eval.data <- function(df){
  
  nulls <- subset(df, .sample != target1 & .sample != target2)
  groups <- subset(df, .sample == target2)
  lines <- subset(df, .sample == target1)
  
  nl <- ddply(nulls, .(.sample), eval.df)
  
  cl <- eval.df(groups)
  ll <- eval.df(lines)
  
  c(null.line = max(nl$line), 
    null.cluster = max(nl$group), 
    null.gini = min(nl$gini),
    line = ll$line, 
    cluster = cl$group,
    gini = cl$gini)
}

# Calculate gini statistic
gini <- function(y, unbiased = TRUE, na.rm = FALSE){
  if ( (!is.numeric(y))) {
    warning("'y' is not numeric; returning NA")
    return(NA)
  }
  if (!na.rm && any(na.ind <- is.na(y)))
    stop("'x' contain NAs")
  if (na.rm)
    y <- y[!na.ind]
  x <- as.numeric(table(y))/sum(y)
  1 - sum(x ^ 2)
}

# Ensure generated data is within reasonable quantiles and 
# isn't a "bad random sample" with truly extra-ordinary features
eval.data.quantiles <- function(i, data.set.parms, reps = 3){
  tmp.sub <- NULL
  partmp <- data.frame()
  pardata <- data.frame()
  pardata.subplot.stats <- data.frame()
  pardata.stats <- data.frame()
  ntries <- 0
  while (sum(tmp.sub) < reps & ntries < 100) {
    set <- (i - 1) * reps + sum(as.numeric(tmp.sub)) + 1
    message(paste0("set = ", set))
    data.sub <- data.frame(set = set, gen.data(as.list(data.set.parms)))
    data.sub.subplot.stats <- data.sub %>%
      group_by(set, .sample) %>%
      do({
        reg <- lm(y~x, data = .)
        data.frame(.sample = unique(.$.sample), 
                   LineSig = summary(reg)$r.squared, 
                   ClusterSig = cluster(.), 
                   lineplot = unique(.$target1), 
                   groupplot = unique(.$target2))
      })
    
    data.sub.stats <- data.frame(
      data.set.parms[,c("K", "sd.trend", "sd.cluster", "N")], 
      summarize(
        data.sub.subplot.stats,
        set = unique(set),
        line = LineSig[.sample == lineplot], 
        cluster = ClusterSig[.sample == groupplot], 
        null.line = max(LineSig[.sample != lineplot & .sample != groupplot]), 
        null.cluster = max(ClusterSig[.sample != groupplot & .sample != lineplot]),
        lineplot = unique(lineplot),
        groupplot = unique(groupplot)))
    
    # Calculate quantiles of datasets compared to simulated quantiles
    tmp <- data.frame(
      data.set.parms[,c("K", "sd.trend", "sd.cluster", "N")], 
      sim.quantile(data.sub.stats))
    tmp$set <- set
    
    # Require all quantiles to be between (.2, .8)
    tmp.sub1 <- as.logical(rowSums(
      tmp[,c("line", "cluster", "null.line", "null.cluster")] > .2 & 
        tmp[,c("line", "cluster", "null.line", "null.cluster")] < .8) == 4)
    
    # increment ntries
    ntries <- ntries + 1
    
    if ((tmp.sub1)) {
      tmp.sub <- c(tmp.sub, tmp.sub1)
      partmp <- bind_rows(partmp, tmp)
      pardata <- bind_rows(pardata, data.sub)
      pardata.stats <- bind_rows(pardata.stats, data.sub.stats)
      pardata.subplot.stats <- bind_rows(pardata.subplot.stats, 
                                         data.sub.subplot.stats)
    }
  }
  
  if ((ntries == 100)) {
    warning(paste0("Limit of 100 tries reached for \n", 
                   paste(names(data.set.parms), data.set.parms, 
                         sep = "\t", collapse = "\n"), 
                   "\nIncomplete results returned."))
  }
  
  return(
    list(data = pardata, 
         data.stats = pardata.stats, 
         data.subplot.stats = pardata.subplot.stats, 
         quantile.eval = partmp, 
         ntries = ntries)
  )
}

# Generate the ggplot2 plot of lineup data
gen.plot <- function(dd, aes, stats, colorp = NULL, shapep = NULL){
  pointsize <- 1.5
  if (is.null(colorp)) {
    colorp <- best.combo(length(unique(dd$group)), colors, colortm)
  }
  if (is.null(shapep)) {
    shapep <- best.combo(length(unique(dd$group)), shapes, shapetm)
  } 
  
  plot <- ggplot(data = dd, aes(x = x, y = y)) + 
    theme_lineup() + 
    facet_wrap(~.sample, ncol = 5)  
  
  # lines and ellipses are not data structure, so reduce contrast to emphasize points!  
  # Set other geoms/aids
  if (("Reg. Line" %in% stats)) {
    plot <- plot + 
      geom_smooth(method = "lm", 
                  color = "grey30", 
                  se = F, 
                  fullrange = TRUE)
  } 
  if (("Error Bands" %in% stats)) {
    tmp <- dd %>% 
      group_by(.sample) %>%
      do({
        model <- lm(y~x, data = .)
        range <- diff(range(.$x))
        newdata <- data.frame(x = seq(min(dd$x) - .1 * range, 
                                      max(dd$x) + .1 * range, 
                                      length.out = 400))
        data.frame(.sample = unique(.$.sample), x = newdata$x, 
                   predict.lm(model, newdata = newdata, 
                              interval = "prediction", level = 0.9))
      })
    
    if (("Shade Error Bands" %in% stats & "Error Bands" %in% stats)) {
      plot <- plot + 
        geom_line(data = tmp, aes(x = x, y = lwr), linetype = 2, inherit.aes = F) + 
        geom_line(data = tmp, aes(x = x, y = upr), linetype = 2, inherit.aes = F) + 
        geom_ribbon(data = tmp, aes(x = x, ymin = lwr, ymax = upr), 
                    fill = "black", color = "transparent", 
                    alpha = .1, inherit.aes = F)
    } else {
      plot <- plot + 
        geom_line(data = tmp, aes(x = x, y = lwr), linetype = 2, inherit.aes = F) + 
        geom_line(data = tmp, aes(x = x, y = upr), linetype = 2, inherit.aes = F)
    }
  }
  
  if (("Ellipses" %in% stats)) {
    if (("Color" %in% aes)) {
      if (("Shade Ellipses" %in% stats)) {
        plot <- plot + 
          stat_ellipse(geom = "polygon", level = .9, 
                       aes(fill = factor(group), 
                           colour = factor(group)), 
                       alpha = 0.1) + 
          scale_fill_manual(values = colorp)
      } else {
        plot <- plot + 
          stat_ellipse(geom = "polygon", level = .9, 
                       aes(colour = factor(group)), 
                       alpha = 0.2, fill = "transparent")
      }
    } else if (("Shape" %in% aes)) {
      plot <- plot + 
        stat_ellipse(geom = "polygon", level = .9, aes(group = factor(group)), 
                     colour = "grey15", fill = "transparent")
    } else {
      plot <- plot + 
        stat_ellipse(geom = "polygon", level = .9, aes(group = factor(group)), 
                     colour = "grey15", fill = "transparent")
    }
  }
  
  if (("Shade Ellipses" %in% stats & "Ellipses" %in% stats)) {
    plot <- plot + 
      stat_ellipse(geom = "polygon", level = .9, aes(group = factor(group)), 
                   alpha = 0.1, fill = "black", color = "transparent") 
  }
  
  # points on top of everything
  # Set Aesthetics
  if ((length(aes) == 0)) {
    plot <- plot + geom_point(size = pointsize, shape = 1) + 
      scale_shape_discrete(solid = F)
  } else if ((length(aes) == 1)) {
    if (("Color" %in% aes)) {
      plot <- plot + geom_point(aes(color = factor(group)), 
                                size = pointsize, shape = 1) + 
        scale_color_manual(values = colorp)
    } else {
      plot <- plot + geom_point(aes(shape = factor(group)), size = pointsize) + 
        scale_shape_manual(values = shapep)
    }
  } else {
    plot <- plot + geom_point(aes(color = factor(group), shape = factor(group)), 
                              size = pointsize) + 
      scale_color_manual(values = colorp) + 
      scale_shape_manual(values = shapep)
  }
  
  plot
}

# Calculate cluster sum of squares
cluster <- function(dframe) {
  # we assume to have x, y, and a group variable
  xmean <- mean(dframe$x)
  ymean <- mean(dframe$y)
  dframe$dist <- with(dframe, (x - xmean) ^ 2 + (y - ymean) ^ 2)
  SSTotal <- sum(dframe$dist)
  dframe <- dframe %>% 
    group_by(group) %>% 
    mutate(xgroup = mean(x), ygroup = mean(y))
  dframe$gdist <- with(dframe, (x - xgroup) ^ 2 + (y - ygroup) ^ 2)
  SSGroup <- sum(dframe$gdist)
  (SSTotal - SSGroup)/SSTotal
}

# Save the pictures as SVG and PNG and PDF and all the things!
# Also save a csv of the data (for future proofing)
save.pics <- function(df, datastats, plotparms, plotname, testplot = FALSE, nullplot = FALSE){
  i <- unique(df$set)
  if ((nullplot)) {
    dataname <- sprintf("null-set-%d-k-%d-sdline-%.2f-sdgroup-%.2f", i, 
                        datastats$K, datastats$sd.trend, datastats$sd.cluster)
    realfname <- sprintf("null-set-%d-plot-%s-k-%d-sdline-%.2f-sdgroup-%.2f", 
                         i, plotname, 
                         datastats$K, datastats$sd.trend, datastats$sd.cluster)
    fname <- realfname
  } else if ((testplot)) {
    dataname <- sprintf("test-set-%d-k-%d-sdline-%.2f-sdgroup-%.2f", i, 
                        datastats$K, datastats$sd.trend, datastats$sd.cluster)
    realfname <- sprintf("test-set-%d-plot-%s-k-%d-sdline-%.2f-sdgroup-%.2f", 
                         i, plotname, 
                         datastats$K, datastats$sd.trend, datastats$sd.cluster)
    fname <- realfname
  } else {
    dataname <- sprintf("set-%d-k-%d-sdline-%.2f-sdgroup-%.2f", i, 
                        datastats$K, datastats$sd.trend, datastats$sd.cluster)
    realfname <- sprintf("set-%d-plot-%s-k-%d-sdline-%.2f-sdgroup-%.2f", 
                         i, plotname, 
                         datastats$K, datastats$sd.trend, datastats$sd.cluster)
    fname <- digest(realfname, serialize = FALSE)
  }
  
  plotobj <- gen.plot(df, aes = get.aes(plotparms), stats = get.stats(plotparms))
  
  if ((plotname == "plain" | testplot | nullplot) ) {
    write.csv(df, file = paste0("data/", dataname, ".csv"), 
              row.names = FALSE)
  }
  ggsave(plotobj, filename = paste0(path, "lineups/", realfname, ".png"), 
         width = width, height = height, dpi = 100)

  obsPlotLocation <- ifelse(sum(c("lineplot", "groupplot") %in% names(datastats)) == 2, 
                            sprintf("%d, %d", datastats$lineplot, datastats$groupplot),
                            datastats$target1)
  if (nullplot) obsPlotLocation <- NA
  
  diff.sign <- ifelse(sum(c("lineplot", "groupplot") %in% names(datastats)) == 2, 1, -1)
  l3 <- which(plotname == c("plain","color", "shape", 
                            "colorShape", "colorEllipse", "colorShapeEllipse", 
                            "trend", "trendError", 
                            "colorTrend", "colorEllipseTrendError")) - 1
  difficulty <- as.numeric(sprintf('%d%d%d', datastats$l1, datastats$l2, l3))
  
  pValue <- ifelse(sum(c("lineplot", "groupplot") %in% names(datastats)) == 2, 
                   sprintf("line-%.5f-cluster-%.5f", datastats$line, datastats$cluster), 
                   sprintf("%s-%.5f", datastats$type, datastats$target.sig))
  picName <- ifelse(sum(c("lineplot", "groupplot") %in% names(datastats)) == 2, 
                    paste0("lineups/svgs/", fname, ".svg"), 
                    paste0("lineups/", fname, ".svg"))
  
  data.frame(
    pic_id = unique(df$set) + l3/10,
    sample_size = datastats$K,
    test_param = sprintf("turk16-%s", plotname),
    param_value = sprintf("k-%d-sdline-%.2f-sdgroup-%.2f", 
                          datastats$K, datastats$sd.trend, datastats$sd.cluster),
    p_value = pValue,
    obs_plot_location = obsPlotLocation,
    pic_name = picName,
    experiment = "turk16",
    difficulty = diff.sign*difficulty,
    data_name = dataname
  )
}

# Make an interactive lineup
interactive_lineup <- function(plotobj, fname, script, 
                               toggle = "toggle", width = 6, height = 6, 
                               trial = FALSE, ex = FALSE) {
  
  path <- ifelse(ex, "Images/Lineups/example/", "Images/Lineups/")
  
  if ((ex)) {
    ggsave(plotobj, filename = paste0(path, "pngs/", fname, ".png"), 
           width = width, height = height, dpi = 100)
  }
  ggsave(plotobj, filename = paste0(path, "pdfs/", fname, ".pdf"), 
         width = width, height = height, dpi = 100)
  
  CairoPDF(file = tempfile(), width = width, height = height)
  print(plotobj)
  require(gridSVG)
  grobs <- grid.ls(print = FALSE)
  
  idx <- grep("panel-", grobs$name)
  for (i in idx) { 
    grid.garnish(grobs$name[i],
                 onmouseover = paste("frame('",grobs$name[i + 2], ".1')", sep = ""),
                 onmouseout = paste("deframe('",grobs$name[i + 2], ".1')", sep = ""), 
                 onmousedown = paste(sprintf("%shigh(evt, '", toggle),grobs$name[i + 2], ".1')", sep = ""))
  }
  
  # use script on server to get locally executable javascript code
  # or use inline option
  grid.script(filename = script)
  grid.export(name = paste0(path, ifelse(trial, "trials/", "svgs/"), fname, ".svg"), 
              uniqueNames = FALSE, 
              exportJS = "inline", 
              exportCoords = "inline", 
              exportMappings = "inline")
  dev.off()
}