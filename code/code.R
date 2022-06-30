#suppressMessages(library(carenR))
suppressMessages(library(ggplot2))
suppressMessages(library(grid))
suppressMessages(library(gridExtra))

###############################################################################
########################## Error Functions ####################################
###############################################################################

# Variation of function from EDPs (https://github.com/inesareosa/MScThesis)
calculate_error <- function(y, ypred, type = "Absolute") {
  if (type == "Absolute") {
    error <- abs(y - ypred)
  }
  else if (type == "Residual") {
    error <- y - ypred
  }
  #Adds 1 for the visualization to start at 0
  else if (type == "Log") {
    error <- log(1 + abs(y - ypred))
  }
  else {
    stop("Invalid error type: Absolute, Residual or Log.")
  }
  return(error)
}

# Variation of function from EDPs (https://github.com/inesareosa/MScThesis)
single_model_data <- function(data, model, feature_y, type = "Absolute",
                              output_preds = FALSE, output_target = FALSE) {
  y <- data[[feature_y]]
  x <- data[, names(data) != feature_y]
  ypred <- model
  if (isTRUE(output_preds)) {
    x$pred <- ypred
  }
  if (isTRUE(output_target)) {
    x[[feature_y]] <- y
  }
  x$error <- calculate_error(y, ypred, type)
  return(x)
}

###############################################################################
########################### Distribution Rules ################################
###############################################################################


###############################################################################
# > data: defined the dataset to be used                                      #
# > min.sup: defines the minimal value of support, i.e., the frequency an     #
# itemset appears in the dataset                                              #
# > min.conf: defines the minimal value of confidence, i.e., the value of how #
# often a rule is found true                                                  #
# > null: defines null symbol to be character char.                           #
# > lift: defines value for minimal lift rules filter.                        #
# > conv: defines value for minimal conviction rules filter.                  #
# > imp: defines value for minimal improvement rules filter.                  #
# > chi: filter rules through the Chi-Squared test (using 95% confidence      #
# level).                                                                     #
# > X2: filter itemsets containing the consequente through the Chi-Squared    #
# test (values as above).                                                     #
# > Srik: defines list of attributes to apply Srikant discretization.         #
# > K: defines the level of Partial Completeness used in Srikant              #
# discretization.                                                             #
# > Ms: defines value for maximal support used in adjacent interval fusion.   #
# > bonf: applies Bonferroni LCVs adjustment to the given alpha value         #
# according to the potential number of rules in each search space level.      #
# > Alpha: defines the significance value to be used in all statistical tests.#
# e.g. KS, Fisher, Cramer von Mise tests.                                     #
# > disc: none, bin, Srik, Srik.K1.5, Srik.K3.Md, Srik.K6.Md, cin             #
# > ks: derives distribution rules interest using the Kolmogorov-Smirnov test.#
# This is the default test.                                                   #
# > cvm: replaces the KS-test by the Cramer von Mise statistical test. The    #
# test obtains an asymptotic pvalue.                                          #
# > Ss: filter distribution rules using the defined statistical test between  #
# rules and its subrules (an alternative to -imp pruning).                    #
# > kde: derives kernel density estimation for each distribution rule.        #
# > Discfi: defines list of attributes to apply Fayyad & Irani discretization.#
# > Discbuk: defines number of buckets in the equidepth discretization of the #
# POI numeric class attribute.                                                #
# > Discgf: defines list of attributes to apply Goodness-Of-Fit               #
# discretization.                                                             #
# > Discnum: defines number of intervals to be yielded by the discretization  #
# method.                                                                     #
###############################################################################
generate_rules <- function(data, min.sup = 0.02, min.conf = 0.1, null = "?",
                           lift = NULL, conv = NULL, imp = 0.0, chi = FALSE,
                           X2 = FALSE, Srik = NULL, K = NULL, Ms = NULL,
                           bonf = FALSE, Alpha = 0.05, disc = "none",
                           ks = TRUE, cvm = FALSE, Ss = FALSE, kde = FALSE,
                           Discfi = NULL, Discbuk = 3, Discgf = NULL,
                           Discnum = 10) {
  drs <- caren(Train = data, min.sup = min.sup, min.conf = min.conf,
               null = null, imp = imp, lift = lift, conv = conv, chi = chi,
               X2 = X2, Srik = Srik, K = K, Ms = Ms, bonf = bonf,
               Alpha = Alpha, disc = disc, ks = ks, cvm = cvm, Ss = Ss,
               kde = kde, Discfi = Discfi, Discbuk = Discbuk, Discgf = Discgf,
               Discnum = Discnum, Dist = TRUE, POI = "error")
  return(drs)
}

get_subgroups <- function(drs) {
  if (!is.null(drs$Subgroup)) {
    list <- drs$Subgroup
  }
  else {
    list <- drs$subgroup
  }
  res <- c()
  for (i in seq_len(length(list))) {
    if (identical(list[[i]], character(0)) == TRUE) {
      res[i] <- "Whole Population"
    }
    else {
      res[i] <- paste(list[[i]], collapse = ", ")
    }
  }
  return(res)
}

# Name, ant_sup, pvalue, kurt, skew, mean, mode, median, stdev
get_info <- function(drs, dataframe = TRUE) {
  # If output is supposed to be a dataframe
  if (isTRUE(dataframe)) {
    # Generate data frame
    df <- data.frame(matrix(ncol = 9, nrow = 0))
    # Define dataframe names
    colnames(df) <- c("subgroup", "ant_sup", "pvalue", "kurt", "skew", "mean",
                      "mode", "median", "stdev")
    # Iterate over all rules
    for (i in seq_len(nrow(drs))) {
      # Initialize row
      row <- c()
      # Define subgroup
      if (identical(drs[i, ]$Subgroup[[1]], character(0)) == TRUE) {
        row <- c(row, "Whole Population")
      }
      else {
        row <- c(row, paste(drs[i, ]$Subgroup[[1]], collapse = ", "))
      }
      # Add remaining aspects to row
      row <- c(row, drs[i, ]$Ant_sup, drs[i, ]$pvalue, drs[i, ]$Kurt,
               drs[i, ]$Skew, drs[i, ]$Mean, drs[i, ]$Mode, drs[i, ]$Median,
               drs[i, ]$Stdev)
      # Add row to dataframe
      df[nrow(df) + 1, ] <- row
    }
    return(df)
  }
  # Output is supposed to be text
  else {
    text <- ""
    # Iterate over all rules
    for (i in seq_len(nrow(drs))) {
      # Define subgroup
      if (identical(drs[i, ]$Subgroup[[1]], character(0)) == TRUE) {
        subgroup <- "Whole Population"
      }
      else {
        subgroup <- paste(drs[i, ]$Subgroup[[1]], collapse = ", ")
      }
      # Format details
      details <- paste(c(
        paste("\n\tAntecedent Support:", as.character(drs[i, ]$Ant_sup),
              sep = " "),
        paste("\n\tP-value:", as.character(drs[i, ]$pvalue), sep = " "),
        paste("\n\tKurtosis:", as.character(drs[i, ]$Kurt), sep = " "),
        paste("\n\tSkewness:", as.character(drs[i, ]$Skew), sep = " "),
        paste("\n\tMean:", as.character(drs[i, ]$Mean), sep = " "),
        paste("\n\tMode:", as.character(drs[i, ]$Mode), sep = " "),
        paste("\n\tMedian:", as.character(drs[i, ]$Median), sep = " "),
        paste("\n\tStandard Deviation:",  as.character(drs[i, ]$Stdev),
              sep = " ")
      ), collapse = ", ")
      # Print values
      #cat(paste0(subgroup, ":", details, "\n\n"))
      text <- paste0(text, subgroup, ":", details, "\n\n")
    }
    return(text)
  }
}

###############################################################################
################################ Plot Rules ###################################
###############################################################################

plot_rules <- function(drs, page = 1, ylabel = "error", type = "Boxplot") {
  # If type of plots is invalid
  if (identical(type, "Boxplot") == FALSE &&
      identical(type, "Density") == FALSE && 
      identical(type, "Histogram") == FALSE) {
    stop("Invalid plot type: Boxplot, Density or Histogram.")
  }
  # 9 per page
  items_per_page <- 4
  # Number of items
  total <- nrow(drs)
  pages <- 1:total
  # Get general population
  general <- drs[nrow(drs), ]
  # Generate sets of indexes per page
  indexes <- unname(split(pages, ceiling(pages / items_per_page)))
  # Index bigger than number of pages
  if (page > length(indexes) || page <= 0) {
    stop("Page out of bounds.")
  }
  else {
    plots <- list()
    for (i in seq_len(length(indexes[[page]]))) {
      j <- indexes[[page]][i]
      plots[[i]] <- plot_rule(drs[j, ], general, ylabel = ylabel,
                              type = type)
    }
    g <- grid.arrange(grobs = plots, nrow = 2, ncol = 2)
    return(g)
  }
}

# Function from CarenR
# (https://www.dcc.fc.up.pt/~amjorge/software/carenR/R/caren_integrated.R)
as.vector.dist <- function(x) {
  return(untable(as.matrix.dist(x)))
}

# Function from CarenR
# (https://www.dcc.fc.up.pt/~amjorge/software/carenR/R/caren_integrated.R)
untable <- function(m) {
  v <- NULL
  for (i in seq_len(nrow(m))) {
    v <- c(v, rep(m[i, 1], m[i, 2]))
  }
  return(v)
}

# Function from CarenR
# (https://www.dcc.fc.up.pt/~amjorge/software/carenR/R/caren_integrated.R)
as.matrix.dist <- function(dist2) {
  x <- strsplit(strsplit(as.character(dist2), "{ ", fixed = TRUE)[[1]][2],
                " }", fixed = TRUE)[[1]][1]
  y <- strsplit(x, ",")[[1]]
  a <- as.numeric(unlist(strsplit(y, "/")))
  matrix(a, length(a) / 2, 2, byrow = TRUE)
}

plot_rule <- function(dr, general_population, type = "Boxplot",
                      ylabel = "error") {
  # Check if types are valid
  if (identical(type, "Boxplot") == FALSE &&
      identical(type, "Density") == FALSE && 
      identical(type, "Histogram") == FALSE) {
    stop("Invalid plot type: Boxplot, Density or Histogram.")
  }
  # If General Population
  if (identical(dr$Subgroup[[1]], character(0)) == TRUE) {
    subgroup <- "General Population"
  }
  # Else
  else {
    subgroup <- paste(dr$Subgroup[[1]], collapse = ",\n")
  }
  #details <- paste(c(
  #paste("Ant Sup: ", as.character(dr$Ant_sup), sep=" "),
  #paste("P-value: ", as.character(dr$pvalue), sep=" "),
  #paste("Kurt: ", as.character(dr$Kurt), sep=" "),
  #paste("\nSkew: ", as.character(dr$Skew), sep=" "),
  #paste("Mean: ", as.character(dr$Mean), sep=" "),
  #paste("Mode: ", as.character(dr$Mode), sep=" "),
  #paste("\nMedian: ", as.character(dr$Median), sep=" "),
  #paste("Stdev: ",  as.character(dr$Stdev), sep=" ")
  #), collapse = ", "
  #)
  # Subgroup error distribution
  dist <- as.vector.dist(dr$Dist)
  rows_dist <- length(dist)
  # General population error distribution
  general <- as.vector.dist(general_population$Dist)
  rows_general <- length(general)
  error <- c(dist, general)
  # Dataframe using both distributions
  x_sub <- rep(c(subgroup), each = length(dist))
  x_general <- rep(c("Whole Population"), each = length(general))
  group <- c(x_sub, x_general)
  df <- data.frame(group, error)
  # Generate legends for each plot
  legends <- list()
  legends[subgroup] <- paste0(subgroup, "\n", rows_dist, " instances\n(",
                              format(round(((rows_dist / rows_general) * 100), 3),
                                     nsmall = 3), "%)")
  legends["Whole Population"] <- paste0("Whole Population", "\n",
                                        rows_general, " instances\n(", 100, "%)")
  if (type == "Boxplot") {
    # Plot
    plot <- ggplot(data = df, aes(x = group, y = error, fill = group)) +
      theme(legend.position = "none", axis.title.x = element_blank(),
            plot.title = element_text(face = "bold")) +
      labs(y = ylabel) + #, title = subgroup) + #, subtitle = details) +
      geom_boxplot() +
      scale_x_discrete("group", labels = legends) +
      geom_hline(yintercept = median(general),
                 linetype = "dashed", colour = "#333233")
    return(plot)
  }
  else if (type == "Density") {
    # Plot
    p <- ggplot(data = df, aes(x = error, fill = group)) +
      theme(legend.title = element_blank(), legend.position = "bottom",
            legend.box = "horizontal",
            plot.title = element_text(face = "bold")) +
      labs(x = ylabel) + #, title = subgroup) + #, subtitle = details) +
      geom_density(alpha = 0.5) +
      scale_fill_discrete("group", labels = legends)
    return(p)
  }
  else if (type == "Histogram") {
    # Plot
    p <- ggplot(data = df, aes(x = error, fill = group)) +
      theme(legend.title = element_blank(), legend.position = "bottom",
            legend.box = "horizontal",
            plot.title = element_text(face = "bold")) +
      labs(x = ylabel, y = "Frequency") +
      geom_histogram(aes(y = stat(density * width)), binwidth = 0.1,
                     alpha = 0.5, position = "identity") +
      scale_fill_discrete("group", labels = legends)
    return(p)
  }
  else {
    stop("Invalid plot type: Boxplot, Density or Histogram.")
  }
}

filter_rules <- function(drs = NULL, regex = "") {
  l <- unlist(lapply(get_subgroups(drs),
                     function(x) grepl(x, pattern = regex, fixed = TRUE)))
  return(drs[l, ])
}

# Be aware of length limitations
plot_rules_single_plot <- function(drs, general_population, title = "",
                                   ylabel = "Log Error", type = "Boxplot") {
  # Check if types are valid
  if (identical(type, "Boxplot") == FALSE &&
      identical(type, "Density") == FALSE && 
      identical(type, "Histogram") == FALSE) {
    stop("Invalid plot type: Boxplot, Density or Histogram.")
  }
  # Named list for each subgroup to keep track of number of instances
  instance_count <- list()
  # General population error distribution
  general <- as.vector.dist(general_population$Dist)
  rows_general <- length(general)
  x_general <- rep(c("Whole Population"), each = length(general))
  error <- c(general)
  group <- c(x_general)
  df <- data.frame(group, error)
  # Update instance count
  instance_count["Whole Population"] <- rows_general
  # Rules
  for (i in seq_len(nrow(drs))) {
    # Subgroup error distribution
    dist <- as.vector.dist(drs[i, ]$Dist)
    rows_dist <- length(dist)
    error <- c(dist)
    # Dataframe using both distributions
    subgroup_name <- paste(drs[i, ]$Subgroup[[1]], collapse = ",\n")
    x_sub <- rep(c(subgroup_name), each = length(dist))
    group <- c(x_sub)
    df <- rbind(df, data.frame(group, error))
    # Update instance count
    instance_count[subgroup_name] <- rows_dist
  }
  # Generate legends for each plot
  legends <- list()
  for (j in names(instance_count)) {
    legends[j] <- paste0(j, "\n", instance_count[j], " instances\n(",
                         format(round(((as.integer(instance_count[j]) / rows_general)
                                       * 100), 3), nsmall = 3), "%)")
  }
  if (type == "Boxplot") {
    # Plot
    plot <- ggplot(data = df, aes(x = group, y = error, fill = group)) +
      theme(legend.position = "none", axis.title.x = element_blank(),
            plot.title = element_text(face = "bold")) +
      labs(y = ylabel, title = title) +
      scale_x_discrete("group", labels = legends) +
      geom_boxplot() +
      geom_hline(yintercept = median(general), linetype = "dashed",
                 colour = "#333233")
    return(plot)
  }
  else if (type == "Density") {
    # Plot
    p <- ggplot(data = df, aes(x = error, fill = group)) +
      theme(legend.title = element_blank(), legend.position = "bottom",
            legend.box = "horizontal",
            plot.title = element_text(face = "bold")) +
      labs(x = ylabel, title = title) +
      geom_density(alpha = 0.5) +
      scale_fill_discrete("group", labels = legends)
    return(p)
  }
  else if (type == "Histogram") {
    # Plot
    p <- ggplot(data = df, aes(x = error, fill = group)) +
      theme(legend.title = element_blank(), legend.position = "bottom",
            legend.box = "horizontal",
            plot.title = element_text(face = "bold")) +
      labs(x = ylabel, y = "Frequency",  title = title) +
      geom_histogram(aes(y = stat(density * width)), binwidth = 0.1,
                     alpha = 0.5, position = "identity") +
      scale_fill_discrete("group", labels = legends)
    return(p)
  }
  else {
    stop("Invalid plot type: Boxplot, Density of Histogram.")
  }
}

###############################################################################
############################### Network Plot ##################################
###############################################################################

get_nodes <- function(drs) {
  # Store Whole Population Mean to select colors of distribution nodes
  whole_population_mean <- drs[nrow(drs), ]$Mean
  
  # Initialize link dataframe with column names
  links <- data.frame(data.frame(matrix(ncol = 2, nrow = 0)))
  colnames(links) <- c("from", "to")
  
  # Initialize node dataframe with column names
  nodes <- data.frame(matrix(ncol = 8, nrow = 0))
  colnames(nodes) <- c("id", "label", "group", "value",
                       "shape", "title", "color", "shadow")
  
  # Populate both dataframes using rules
  for (i in seq_len(nrow(drs))) {
    # Mean Values of Distribution
    # dist <- as.vector.dist(drs[i, ]$Dist)
    # dist_mean <- toString(mean(dist))
    dist_mean <- toString(drs[i, ]$Mean)
    # If dist_mean is not in dataframe
    if (dist_mean %in% nodes$label == FALSE) {
      # title <- paste0("<p><b>", dist_mean, "</b><br></p>")
      title <- paste0("<p><b>", paste(drs[i, ]$Subgroup[[1]], collapse = ", "),
                      "</b><br></p><hr>",
                      "<p style='font-size: 14px;'><b>Antecedent Support:</b> ",
                      as.character(drs[i, ]$Ant_sup), "</p>",
                      "<p style='font-size: 14px;'><b>P-value:</b> ",
                      as.character(drs[i, ]$pvalue), "</p>",
                      "<p style='font-size: 14px;'><b>Kurtosis:</b> ",
                      as.character(drs[i, ]$Kurt), "</p>",
                      "<p style='font-size: 14px;'><b>Skewness:</b> ",
                      as.character(drs[i, ]$Skew), "</p>",
                      "<p style='font-size: 14px;'><b>Mean:</b> ",
                      as.character(drs[i, ]$Mean), "</p>",
                      "<p style='font-size: 14px;'><b>Mode:</b> ",
                      as.character(drs[i, ]$Mode), "</p>",
                      "<p style='font-size: 14px;'><b>Median:</b> ",
                      as.character(drs[i, ]$Median), "</p>",
                      "<p style='font-size: 14px;'><b>Standard Deviation:</b> ",
                      as.character(drs[i, ]$Stdev), "</p>"
      )
      # Node color is based on the whole population mean
      color <- if (whole_population_mean >= drs[i, ]$Mean) {"blue"} else {"red"}
      # Add node to dataframe
      nodes[nrow(nodes) + 1, ] <- c(nrow(nodes) + 1, dist_mean, "Mean", 1,
                                    "diamond", title, color, TRUE)
    }
    # Subgroup
    lhs <- drs[i, ]$Subgroup
    if (identical(lhs[[1]], character(0)) == TRUE) {
      label <- "Whole Population"
      # If label not in dataframe
      if (label %in% nodes$label == FALSE) {
        title <- paste0("<p><b>", label, "</b><br></p>")
        # Add node to dataframe
        nodes[nrow(nodes) + 1, ] <- c(nrow(nodes) + 1, label, "Itemset", 1,
                                      "ellipse", title, "orange", TRUE)
        # Add link to dataframe
        links[nrow(links) + 1, ] <- c(nodes[nodes$label == label, ]$id,
                                      nodes[nodes$label == dist_mean, ]$id)
      }
      else {
        # Add link to dataframe
        links[nrow(links) + 1, ] <- c(nodes[nodes$label == label, ]$id,
                                      nodes[nodes$label == dist_mean, ]$id)
      }
    }
    else {
      for (j in seq_len(length(lhs[[1]]))) {
        label <- lhs[[1]][j]
        # If label not in dataframe
        if (label %in% nodes$label == FALSE) {
          title <- paste0("<p><b>", label, "</b><br></p>")
          # Add node to dataframe
          nodes[nrow(nodes) + 1, ] <- c(nrow(nodes) + 1, label, "Itemset", 1,
                                        "ellipse", title, "orange", TRUE)
          # Add link to dataframe
          links[nrow(links) + 1, ] <- c(nodes[nodes$label == label, ]$id,
                                        nodes[nodes$label == dist_mean, ]$id)
        }
        else {
          # Add link to dataframe
          links[nrow(links) + 1, ] <- c(nodes[nodes$label == label, ]$id,
                                        nodes[nodes$label == dist_mean, ]$id)
        }
      }
    }
  }
  return(list(nodes = nodes, links = links))
}

# install.packages("visNetwork")
suppressMessages(library("visNetwork"))

plot_graph <- function(nodes, links) {
  g <- visNetwork(nodes, links) %>%
    #g <- visNetwork(nodes, links, height = 900, width = 1440) %>%
    # visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
    visOptions(highlightNearest = list(enabled = TRUE, hover = TRUE,
                                       algorithm = "hierarchical"),
               nodesIdSelection = TRUE) %>%
    # visInteraction(hover = T) %>%
    visEdges(arrows = "to", color = "gray") %>%
    # visIgraphLayout(layout = "layout_in_circle") #%>% # smooth = TRUE
    visIgraphLayout() #%>% # smooth = TRUE
  return(g)
}

###############################################################################
############################# Performance Table ###############################
###############################################################################
suppressMessages(library(reshape))

calculate_quantiles <- function(drs) {
  # Create a data frame to store the quantile values for each subgroup
  df <- data.frame(data.frame(matrix(ncol = 6, nrow = 0)))
  # Store the column names to ease the process
  colnames(df) <- c("subgroup", "min", "q1", "median", "q3", "max")
  # Go through every rule
  for (i in seq_len(nrow(drs))) {
    # Save the subgroup's name and its quantile values
    df[i, ] <- c(paste(drs[i, ]$Subgroup[[1]], collapse = ", "), 
                 quantile(as.vector.dist(drs[i, ]$Dist)))
    
  }
  return(df)
}

# ADD MARGIN!!!!!
# Function used to generate cell behavior, using performance
cell_colors <- function(value_subgroup, value_reference) {
  # Convert to numeric (value might be a string)
  # 1st is concerted to character as without it same values might not be
  # counted as equals
  value_subgroup <- as.numeric(as.character(value_subgroup))
  value_reference <- as.numeric(as.character(value_reference))
  # In case the value is higher than the reference
  if (value_subgroup > value_reference) {
    return("Higher")
  }
  # In the case the value is smaller
  else if (value_subgroup < value_reference) {
    return("Lower")
  }
  # In case the values are equal
  else {
    return("Equal")
  }
} 

# Function to generate a table plot of comparisons to a reference distribution
performance_table <- function(drs, reference_dr) {
  # Save the distribution quantiles (min, q1, median, q3, max)
  qntls_reference <- quantile(as.vector.dist(reference_dr$Dist))
  # Store Reference distribution name to add to plot
  if (identical(reference_dr$Subgroup[[1]], character(0)) == TRUE) {
    reference_name <- "Whole Data"
  }
  else {
    reference_name <- paste(reference_dr$Subgroup[[1]], collapse = ", ") 
  }
  # Produce a dataframe containing subgroup information and quantiles
  df <- calculate_quantiles(drs) 
  # Melt data frame into subgroup, variable (min, q1, median, q3, max) & value
  melted_df <- melt(df, id.var = "subgroup")
  # Initialize array of colors for each subgroup
  colors <- c()
  # Store order of quantiles in order to access indexes on loop
  qntls <- c("min", "q1", "median", "q3", "max")
  
  # Loop over rows to generate colors
  for (i in seq_len(nrow(melted_df))) {
    # Generate color for row based on quantile and its value
    colors <- c(colors, cell_colors(melted_df[i, ]$value, 
                                    qntls_reference[match(melted_df[i, ]$variable, qntls)]))
  }
  # Associate colors with rows
  melted_df$color <- colors
  # Generate table plot
  table <- ggplot(melted_df, aes(y = subgroup, x = variable)) + 
    geom_tile(aes(fill = color, width = 0.95, height = 0.95)) + 
    scale_fill_manual(breaks = c("Higher", "Equal", "Lower"), 
                      values = c("#f7756d", "#619cff", "#00bb39")) +
    theme(axis.title.x = element_blank(), axis.title.y = element_blank(), 
          legend.title = element_blank(), plot.title.position = "plot") +
    labs(title = paste0("Reference Distribution: ", reference_name))
  return(table)
}

###############################################################################
############################# Counter Factuals ################################
###############################################################################

counter_factuals <- function(drs, reference_distribution) {
  # Produce a dataframe containing subgroup information and quantiles
  df <- calculate_quantiles(drs) 
  # Turn subgroups into a column made of lists
  df$subgroup <- strsplit(as.character(df$subgroup), ", ")
  # New list to store variations of each rule (dataframes)
  variations <- list()
  # Loop over every rule
  for (i in seq_len(nrow(df))) {
    # Initialize an auxiliar dataframe to store variations of the i-th rule
    aux <- data.frame(matrix(ncol = 7, nrow = 0))
    colnames(aux) <- c("subgroup", "nr_similarities", "min", "q1", "median", "q3", "max")
    # Loop over the rules
    for (j in seq_len(nrow(df))) {
      # If the rule being compared is the same (i-th), the loop skips it
      if (i == j) {
        next
      }
      # Otherwise the rule is to be analyzed
      else {
        # Check if elements share some bin  
        mutual <- intersect(df[i, ]$subgroup[[1]], df[j, ]$subgroup[[1]])
        # Calculate the length of the shared bins
        mutual_length <- length(mutual)
        # In case the rules shared 1 or more characteristics
        if (mutual_length > 0) {
          # Store the name of the subgroup, number of similarities
          # and its quantile values
          aux[nrow(aux) + 1, ] <- c(
            paste(df[j, ]$subgroup[[1]], collapse = ", "),
            mutual_length,
            as.numeric(df[j, ][-1])
          )
        }
      }
    }
    # Sort auxiliar dataframe by number of similarities (bins)
    aux <- aux[order(aux$nr_similarities, decreasing = TRUE), ]
    # Drop column containing number of similarites
    aux$nr_similarities <- NULL
    # Add the auxiliar dataframe to the list of variations
    variations[[i]] <- aux
  }
  # Initialize vector of instances to drop
  instances_to_drop <- c()
  # Initialize list of performance of rules
  performance_rules <- list()
  # Loop over every rule
  for (i in seq_len(nrow(df))) {
    # initialize vector of rule performance
    rule_performance <- c()
    # Calculate performance of rule based on reference distribution
    for (l in seq_len(length(df[i, ][-1]))) {
      rule_performance <- c(rule_performance, cell_colors(
        as.numeric(as.character(df[i, ][l + 1])), 
        as.numeric(as.character(reference_distribution[l + 1]))))
    }
    # Append rule performance to list
    performance_rules[[i]] <- rule_performance 
    # Initialize vector of rows to drop (Not different from parent node)
    uninteresting_indexes <- c()
    # Loop over every variation associated with the rule
    for (j in seq_len(nrow(variations[[i]]))) {
      # Loop over every quantile of the respective variation
      for (k in seq_len(length(variations[[i]][j, ][-1]))) {
        # Values are compared with the reference distribution
        variations[[i]][j, ][k + 1] <- cell_colors(
          as.numeric(as.character(variations[[i]][j, ][k + 1])), 
          as.numeric(as.character(reference_distribution[k + 1])))
      }
      # Check if performance is exactly the same as parent node and add to vec
      uninteresting_indexes <- c(uninteresting_indexes, 
                                 all(rule_performance == variations[[i]][j, ][-1]))
    }
    # Check if there are rows to delete
    if (length(uninteresting_indexes > 0)) {
      # Drop uninteresting indexes
      variations[[i]] <- variations[[i]][!uninteresting_indexes, ] 
    }
    # Check if instance has more at least one interesting variation
    instances_to_drop <- c(instances_to_drop, nrow(variations[[i]]) == 0)
  }
  # Associate names of parent rules to each dataframe in list
  names(variations) <- get_subgroups(df)
  # Associate names of subgroups to their performance
  names(performance_rules) <- names(variations)
  
  # Drop instances with no variations
  variations <- variations[!instances_to_drop]
  # Return the list with the interesting instances 
  return(list(subgroups = performance_rules, 
              interesting_instances = variations))
}

counter_factuals_as_text <- function(cfs) {
  text <- ""
  for (i in names(cfs$interesting_instances)) {
    text <- paste0(text, ">> ", i, "\n")
    text <- paste0(text, ">> ", paste(unlist(cfs$subgroups[[i]]),
                                      collapse = " "), "\n\n")
    for (j in seq_len(nrow(cfs$interesting_instances[[i]]))) {
      text <- paste0(text, "\t> ", cfs$interesting_instances[[i]][j, ]$subgroup,
                     "\n")
      text <- paste0(text, "\t", paste0(
        cfs$interesting_instances[[i]][j, ][-1],
        collapse = " "), "\n\n")
    }
    text <- paste0(text, "\n\n")
  }
  
  # Explanation of order (min, q1, median, q3, max)
  text <- paste0(text, "\n\n", "Note: The performance values are ordered", 
                 " based on quantiles, i.e., min, q1, median, q3 and max, respectively")
  return(text)
}

###############################################################################
######################### Multi Model Comparison ##############################
###############################################################################

multi_model_plot <- function(multi_rules, type = "Boxplot", ylabel = "Log Error", title = "") {
  # Check if types are valid
  if (identical(type, "Boxplot") == FALSE &&
      identical(type, "Density") == FALSE && 
      identical(type, "Histogram") == FALSE) {
    stop("Invalid plot type: Boxplot, Density or Histogram.")
  }
  # Named list for each subgroup to keep track of number of instances
  instance_count <- list()
  
  df_multi <- data.frame(matrix(nrow = 0, ncol = 3))
  colnames(df_multi) <- c("group", "error", "model")
  
  for (i in seq_len(nrow(multi_rules))) {
    
    dist <- as.vector.dist(multi_rules[i, ]$Dist)
    rows_dist <- length(dist)
    error <- c(dist)
    
    # Dataframe using both distributions
    subgroup_name <- multi_rules[i, ]$Subgroup[[1]]
    x_sub <- rep(c(subgroup_name), each = length(dist))
    group <- c(x_sub)
    
    subgroup_model <- multi_rules[i, ]$model
    x_model <- rep(c(subgroup_model), each = length(dist))
    model <- c(x_model)
    
    df_multi <- rbind(df_multi, data.frame(group, error, model))
    # Update instance count
    instance_count[subgroup_name] <- rows_dist
  }
  
  # Generate legends for each plot
  legends <- list()
  for (j in names(instance_count)) {
    legends[j] <- paste0(j, "\n", instance_count[j], " instances\n(",
                         format(round(((as.integer(instance_count[j]) / rows_general)
                                       * 100), 3), nsmall = 3), "%)")
  }
  # Factor the groups
  df_multi$group <- factor(df_multi$group, levels = get_subgroups(multi_rules))
  
  # Plot
  if (type == "Boxplot") {
    plot <- ggplot(data = df_multi, aes(x = group, y = error, fill = model)) +
      theme(legend.position = "none", axis.title.x = element_blank(),
            plot.title = element_text(face = "bold")) +
      labs(y = ylabel, title = title) +
      scale_x_discrete("group", labels = legends) +
      geom_boxplot()
    return(plot)
  }
  else if (type == "Density") {
    # Plot
    p <- ggplot(data = df_multi, aes(x = error, fill = group)) +
      theme(legend.title = element_blank(), legend.position = "bottom",
            legend.box = "horizontal",
            plot.title = element_text(face = "bold")) +
      labs(x = ylabel, title = title) +
      geom_density(alpha = 0.5) +
      scale_fill_discrete("group", labels = legends)
    return(p)
  }
  else if (type == "Histogram") {
    # Plot
    p <- ggplot(data = df_multi, aes(x = error, fill = group)) +
      theme(legend.title = element_blank(), legend.position = "bottom",
            legend.box = "horizontal",
            plot.title = element_text(face = "bold")) +
      labs(x = ylabel, y = "Frequency",  title = title) +
      geom_histogram(aes(y = stat(density * width)), binwidth = 0.1,
                     alpha = 0.5, position = "identity") +
      scale_fill_discrete("group", labels = legends)
    return(p)
  }
  else {
    stop("Invalid plot type: Boxplot, Density of Histogram.")
  }
}