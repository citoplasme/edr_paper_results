source("/Users/JoaoPimentel/Desktop/article/functions.R")
load("/Users/JoaoPimentel/Desktop/article/data/datasets_with_preds_discretized.RData")

# ---------------- Figure 1 and Figure 2 ----------------
label <- "Log error"
df <- single_model_data(
  data = datasets_with_preds_discretized$a6[, setdiff(names(datasets_with_preds_discretized$a6), c("nnet", "gbm", "svm", "rf"))],
  model = datasets_with_preds_discretized$a6$gbm,
  feature_y = "a6",
  type = "Log")
#save(df, file = "/Users/JoaoPimentel/Desktop/edr_paper_content/a6_gbm.RData")

drs <- generate_rules(data = df, min.conf = 0.6, min.sup = 0.05, imp = 0.00000000000001)
#save(drs, file = "/Users/JoaoPimentel/Desktop/edr_paper_content/a6_gbm_edrs.RData")

general_population <- drs[nrow(drs),]
# Figure 1
f1 <- plot_rule(drs[1, ], general_population, ylabel = label) + 
  theme(axis.text.x = element_text(color = "black", size = 10, face = "bold"),
        axis.text.y = element_text(color = "black", size = 10, face = "bold"),
        #axis.title.x = element_text(color = "black", size = 15, face = "bold"),
        axis.title.y = element_text(color = "black", size = 15, face = "bold"),
        legend.title = element_text(color = "black", size = 15, face = "bold"),
        legend.text = element_text(color = "black", size = 10, face = "bold"))
#ggsave(plot = f1, filename = "/Users/JoaoPimentel/Desktop/edr_paper_content/figures/f1.png", dpi = "retina", units = "cm", width = 15, height = 12)
# Figure 2
f2 <- plot_rule(drs[1, ], general_population, ylabel = label, type = "Density")  + 
  theme(axis.text.x = element_text(color = "black", size = 10, face = "bold"),
        axis.text.y = element_text(color = "black", size = 10, face = "bold"),
        axis.title.x = element_text(color = "black", size = 15, face = "bold"),
        axis.title.y = element_text(color = "black", size = 15, face = "bold"),
        #legend.title = element_text(color = "black", size = 15, face = "bold"),
        legend.text = element_text(color = "black", size = 10, face = "bold"))
ggsave(plot = f2, filename = "/Users/JoaoPimentel/Desktop/edr_paper_content/figures/f2.png", dpi = "retina", units = "cm", width = 15, height = 12)
# ---------------- Figure 3 ----------------
df <- single_model_data(
  data = datasets_with_preds_discretized$a7[, setdiff(names(datasets_with_preds_discretized$a7), c("nnet", "gbm", "svm", "rf"))],
  model = datasets_with_preds_discretized$a7$svm,
  feature_y = "a7",
  type = "Log")
save(df, file = "/Users/JoaoPimentel/Desktop/edr_paper_content/a7_svm.RData")

drs <- generate_rules(data = df, min.conf = 0.6, min.sup = 0.05, imp = 0.00000000000001)
save(drs, file = "/Users/JoaoPimentel/Desktop/edr_paper_content/a7_svm_edrs.RData")

x <- get_nodes(drs)
plot_graph(x$nodes, x$links)

# ---------------- Figure 4 and Figure 5 ----------------
df <- single_model_data(
  data = datasets_with_preds_discretized$a4[, setdiff(names(datasets_with_preds_discretized$a4), c("nnet", "gbm", "svm", "rf"))],
  model = datasets_with_preds_discretized$a4$gbm,
  feature_y = "a4",
  type = "Log")
save(df, file = "/Users/JoaoPimentel/Desktop/edr_paper_content/a4_gbm.RData")

drs <- generate_rules(data = df, min.conf = 0.6, min.sup = 0.05, imp = 0.00000000000001)
save(drs, file = "/Users/JoaoPimentel/Desktop/edr_paper_content/a4_gbm_edrs.RData")

# Figure 4
f4 <- performance_boxplots(rules = drs %>% head(nrow(drs) - 1), reference_conditions = list(), reference_distribution = drs[nrow(drs), ]$Dist %>% as.vector.dist(), label = "Log error") + 
  theme(axis.text.x = element_text(color = "black", size = 10, face = "bold"),
        axis.text.y = element_text(color = "black", size = 10, face = "bold"),
        axis.title.x = element_text(color = "black", size = 15, face = "bold"),
        axis.title.y = element_text(color = "black", size = 15, face = "bold"),
        legend.title = element_text(color = "black", size = 15, face = "bold"),
        legend.text = element_text(color = "black", size = 10, face = "bold"))
ggsave(plot = f4, filename = "/Users/JoaoPimentel/Documents/Github/edr_paper_results/figures/f4.png", dpi = "retina", units = "cm", width = 35, height = 30)

# Figure 5
filtered <- filter_rules(drs = drs, regex = "PO4=[1-13.2]")
f5 <- performance_boxplots(rules = filtered %>% head(nrow(filtered) - 1), reference_conditions = filtered[nrow(filtered), ]$Subgroup, reference_distribution = filtered[nrow(filtered), ]$Dist %>% as.vector.dist(), label = "Log error") + 
  theme(axis.text.x = element_text(color = "black", size = 10, face = "bold"),
        axis.text.y = element_text(color = "black", size = 10, face = "bold"),
        axis.title.x = element_text(color = "black", size = 15, face = "bold"),
        axis.title.y = element_text(color = "black", size = 15, face = "bold"),
        legend.title = element_text(color = "black", size = 15, face = "bold"),
        legend.text = element_text(color = "black", size = 10, face = "bold"))
ggsave(plot = f5, filename = "/Users/JoaoPimentel/Documents/Github/edr_paper_results/figures/f5.png", dpi = "retina", units = "cm", width = 25, height = 7)

# ---------------- Figure 6 ----------------
#source("/Users/joaopimentel/Downloads/Performance/performance_functions.R")
load("/Users/joaopimentel/Documents/GitHub/edr_paper_results/data/a1_gbm.RData")
# Figure 6a
f6a <- edp(data = df, feature = "mnO2", type = "Log", jitter = FALSE)
ggsave(plot = f6a, filename = "/Users/joaopimentel/Documents/GitHub/edr_paper_results/figures/f6a.png", dpi = "retina", units = "cm", width = 17, height = 17)
# Figure 6b 
f6b <- edp(data = df, feature = "oPO4", type = "Log", jitter = FALSE)
ggsave(plot = f6b, filename = "/Users/joaopimentel/Documents/GitHub/edr_paper_results/figures/f6b.png", dpi = "retina", units = "cm", width = 17, height = 17)
# Figure 6c
f6c <- edp(data = df, feature = "NH4", type = "Log", jitter = FALSE)
ggsave(plot = f6c, filename = "/Users/joaopimentel/Documents/GitHub/edr_paper_results/figures/f6c.png", dpi = "retina", units = "cm", width = 17, height = 17)

# ---------------- Figure 7 ----------------
df <- single_model_data(
  data = datasets_with_preds_discretized$a1[, setdiff(names(datasets_with_preds_discretized$a1), c("nnet", "gbm", "svm", "rf"))],
  model = datasets_with_preds_discretized$a1$gbm,
  feature_y = "a1",
  type = "Log")
save(df, file = "/Users/JoaoPimentel/Desktop/edr_paper_content/a1_gbm.RData")

drs <- generate_rules(data = df, min.conf = 0.6, min.sup = 0.05, imp = 0.00000000000001)
save(drs, file = "/Users/JoaoPimentel/Desktop/edr_paper_content/a1_gbm_edrs.RData")

general_population <- drs[nrow(drs),]
# Figure 7a
f7a <- plot_rule(drs[16, ], general_population, ylabel = label) + 
  theme(axis.text.x = element_text(color = "black", size = 10, face = "bold"),
        axis.text.y = element_text(color = "black", size = 10, face = "bold"),
        #axis.title.x = element_text(color = "black", size = 15, face = "bold"),
        axis.title.y = element_text(color = "black", size = 15, face = "bold"),
        legend.title = element_text(color = "black", size = 15, face = "bold"),
        legend.text = element_text(color = "black", size = 10, face = "bold"))
ggsave(plot = f7a, filename = "/Users/JoaoPimentel/Desktop/edr_paper_content/figures/f7a.png", dpi = "retina", units = "cm", width = 15, height = 12)
# Figure 7b
f7b <- plot_rule(drs[16, ], general_population, ylabel = label, type = "Density")  + 
  theme(axis.text.x = element_text(color = "black", size = 10, face = "bold"),
        axis.text.y = element_text(color = "black", size = 10, face = "bold"),
        axis.title.x = element_text(color = "black", size = 15, face = "bold"),
        axis.title.y = element_text(color = "black", size = 15, face = "bold"),
        #legend.title = element_text(color = "black", size = 15, face = "bold"),
        legend.text = element_text(color = "black", size = 10, face = "bold"))
ggsave(plot = f7b, filename = "/Users/JoaoPimentel/Desktop/edr_paper_content/figures/f7b.png", dpi = "retina", units = "cm", width = 15, height = 12)


# ---------------- Figure 8 ----------------
load("/Users/joaopimentel/Documents/GitHub/edr_paper_results/data/acceleration_svm.RData")
# Figure 8a
f8a <- edp(data = df, feature = "Attribute1", type = "Log", jitter = FALSE)
ggsave(plot = f8a, filename = "/Users/joaopimentel/Documents/GitHub/edr_paper_results/figures/f8a.png", dpi = "retina", units = "cm", width = 20, height = 20)
# Figure 8b 
f8b <- edp(data = df, feature = "Attribute6", type = "Log", jitter = FALSE)
ggsave(plot = f8b, filename = "/Users/joaopimentel/Documents/GitHub/edr_paper_results/figures/f8b.png", dpi = "retina", units = "cm", width = 20, height = 20)
# Figure 8c
f8c <- edp(data = df, feature = "Attribute13", type = "Log", jitter = FALSE)
ggsave(plot = f8c, filename = "/Users/joaopimentel/Documents/GitHub/edr_paper_results/figures/f8c.png", dpi = "retina", units = "cm", width = 20, height = 20)
# Figure 8d
f8d <- edp(data = df, feature = "Attribute14", type = "Log", jitter = FALSE)
ggsave(plot = f8d, filename = "/Users/joaopimentel/Documents/GitHub/edr_paper_results/figures/f8d.png", dpi = "retina", units = "cm", width = 20, height = 20)

# ---------------- Figure 9 ----------------
df <- single_model_data(
  data = datasets_with_preds_discretized$acceleration[, setdiff(names(datasets_with_preds_discretized$acceleration), c("nnet", "gbm", "svm", "rf"))],
  model = datasets_with_preds_discretized$acceleration$svm,
  feature_y = "acceleration",
  type = "Log")
save(df, file = "/Users/JoaoPimentel/Desktop/edr_paper_content/acceleration_svm.RData")

drs <- generate_rules(data = df, min.conf = 0.6, min.sup = 0.05, imp = 0.00000000000001)
save(drs, file = "/Users/JoaoPimentel/Desktop/edr_paper_content/acceleration_svm_edrs.RData")

general_population <- drs[nrow(drs),]
#which(get_subgroups(drs) %in% c("Attribute14=[3.26-3.68], Attribute6=[12.3-18.7], Attribute1=nominal1, Attribute13=[30.5-37.2]"))

# Figure 9a
f9a <- plot_rule(drs[38, ], general_population, ylabel = label) + 
  theme(axis.text.x = element_text(color = "black", size = 10, face = "bold"),
        axis.text.y = element_text(color = "black", size = 10, face = "bold"),
        #axis.title.x = element_text(color = "black", size = 15, face = "bold"),
        axis.title.y = element_text(color = "black", size = 15, face = "bold"),
        legend.title = element_text(color = "black", size = 15, face = "bold"),
        legend.text = element_text(color = "black", size = 10, face = "bold"))
ggsave(plot = f9a, filename = "/Users/JoaoPimentel/Desktop/edr_paper_content/figures/f9a.png", dpi = "retina", units = "cm", width = 15, height = 12)
# Figure 9b
f9b <- plot_rule(drs[38, ], general_population, ylabel = label, type = "Density")  + 
  theme(axis.text.x = element_text(color = "black", size = 10, face = "bold"),
        axis.text.y = element_text(color = "black", size = 10, face = "bold"),
        axis.title.x = element_text(color = "black", size = 15, face = "bold"),
        axis.title.y = element_text(color = "black", size = 15, face = "bold"),
        #legend.title = element_text(color = "black", size = 15, face = "bold"),
        legend.text = element_text(color = "black", size = 10, face = "bold"))
ggsave(plot = f9b, filename = "/Users/JoaoPimentel/Desktop/edr_paper_content/figures/f9b.png", dpi = "retina", units = "cm", width = 15, height = 12)


# ---------------- Figure 10 ----------------
df <- single_model_data(
  data = datasets_with_preds_discretized$fuelCons[, setdiff(names(datasets_with_preds_discretized$fuelCons), c("nnet", "gbm", "svm", "rf"))],
  model = datasets_with_preds_discretized$fuelCons$nnet,
  feature_y = "fuel.consumption.country",
  type = "Log")
save(df, file = "/Users/JoaoPimentel/Desktop/edr_paper_content/fuelCons_nnet.RData")

drs <- generate_rules(data = df, min.conf = 0.6, min.sup = 0.35, imp = 0.001)
save(drs, file = "/Users/JoaoPimentel/Desktop/edr_paper_content/fuelCons_nnet_edrs.RData")

general_population <- drs[nrow(drs),]
#which(get_subgroups(drs) %in% c("Attribute22=[2-2], Attribute3=nominal9, Attribute24=nominal33"))

# Figure 10a
f10a <- plot_rule(drs[58, ], general_population, ylabel = label) + 
  theme(axis.text.x = element_text(color = "black", size = 10, face = "bold"),
        axis.text.y = element_text(color = "black", size = 10, face = "bold"),
        #axis.title.x = element_text(color = "black", size = 15, face = "bold"),
        axis.title.y = element_text(color = "black", size = 15, face = "bold"),
        legend.title = element_text(color = "black", size = 15, face = "bold"),
        legend.text = element_text(color = "black", size = 10, face = "bold"))
ggsave(plot = f10a, filename = "/Users/JoaoPimentel/Desktop/edr_paper_content/figures/f10a.png", dpi = "retina", units = "cm", width = 15, height = 12)
# Figure 10b
f10b <- plot_rule(drs[58, ], general_population, ylabel = label, type = "Density")  + 
  theme(axis.text.x = element_text(color = "black", size = 10, face = "bold"),
        axis.text.y = element_text(color = "black", size = 10, face = "bold"),
        axis.title.x = element_text(color = "black", size = 15, face = "bold"),
        axis.title.y = element_text(color = "black", size = 15, face = "bold"),
        #legend.title = element_text(color = "black", size = 15, face = "bold"),
        legend.text = element_text(color = "black", size = 10, face = "bold"))
ggsave(plot = f10b, filename = "/Users/JoaoPimentel/Desktop/edr_paper_content/figures/f10b.png", dpi = "retina", units = "cm", width = 15, height = 12)

# ---------------- Figure 11 ----------------
load("/Users/joaopimentel/Documents/GitHub/edr_paper_results/data/masters_gbm_edrs.RData", masters_gbm <- new.env())
load("/Users/joaopimentel/Documents/GitHub/edr_paper_results/data/masters_nnet_edrs.RData", masters_nnet <- new.env())
load("/Users/joaopimentel/Documents/GitHub/edr_paper_results/data/masters_rf_edrs.RData", masters_rf <- new.env())
load("/Users/joaopimentel/Documents/GitHub/edr_paper_results/data/masters_svm_edrs.RData", masters_svm <- new.env())
source("/Users/JoaoPimentel/Documents/Github/edr_paper_results/code.R")

title <- "CGPA=[7.64-8.27]"
label <- "Log error"

filter_gbm <- masters_gbm$drs[get_subgroups(masters_gbm$drs) == title, ]
filter_gbm$Subgroup <- paste0(filter_gbm$Subgroup, ",\nGradient Boosting Machine")
filter_gbm$model <- "Gradient Boosting Machine"

filter_svm <- masters_svm$drs[get_subgroups(masters_svm$drs) == title, ]
filter_svm$Subgroup <- paste0(filter_svm$Subgroup, ",\nSupport Vector Machine")
filter_svm$model <- "Support Vector Machine"

filter_rf <- masters_rf$drs[get_subgroups(masters_rf$drs) == title, ]
filter_rf$Subgroup <- paste0(filter_rf$Subgroup, ",\nRandom Forest")
filter_rf$model <- "Random Forest"

filter_nnet <- masters_nnet$drs[get_subgroups(masters_nnet$drs) == title, ]
filter_nnet$Subgroup <- paste0(filter_nnet$Subgroup, ",\nArtificial Neural Network")
filter_nnet$model <- "Artificial Neural Network"

median_models <- c()

general_nnet <- masters_nnet$drs[nrow(masters_nnet$drs), ]
general_nnet$Subgroup <- "Artificial Neural Network"
general_nnet$model <- "Artificial Neural Network"
median_models <- c(median_models, median(as.vector.dist(general_nnet$Dist)))

general_gbm <- masters_gbm$drs[nrow(masters_gbm$drs), ]
general_gbm$Subgroup <- "Gradient Boosting Machine"
general_gbm$model <- "Gradient Boosting Machine"
median_models <- c(median_models, median(as.vector.dist(general_gbm$Dist)))

general_rf <- masters_rf$drs[nrow(masters_rf$drs), ]
general_rf$Subgroup <- "Random Forest"
general_rf$model <- "Random Forest"
median_models <- c(median_models, median(as.vector.dist(general_rf$Dist)))

general_svm <- masters_svm$drs[nrow(masters_svm$drs), ]
general_svm$Subgroup <- "Support Vector Machine"
general_svm$model <- "Support Vector Machine"
median_models <- c(median_models, median(as.vector.dist(general_svm$Dist)))

rows_general <- length(as.vector.dist(general_nnet$Dist))

multi_rules <- rbind(general_gbm, general_svm, general_rf, general_nnet, filter_gbm, filter_svm, filter_rf, filter_nnet)

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

df_multi$group <- factor(df_multi$group,
                         levels = c("Artificial Neural Network",
                                    "Gradient Boosting Machine",
                                    "Random Forest",
                                    "Support Vector Machine",
                                    "CGPA=[7.64-8.27],\nArtificial Neural Network",
                                    "CGPA=[7.64-8.27],\nGradient Boosting Machine",
                                    "CGPA=[7.64-8.27],\nRandom Forest",
                                    "CGPA=[7.64-8.27],\nSupport Vector Machine"))

# Plot
f11 <- ggplot(data = df_multi, aes(x = group, y = error, fill = model)) +
  theme(legend.position = "none", axis.title.x = element_blank(),
        plot.title = element_text(face = "bold")) +
  labs(y = "Log error", title = "CGPA=[7.64-8.27]") +
  scale_x_discrete("group", labels = legends) +
  geom_boxplot() +
  geom_hline(yintercept = median_models[1], linetype = "dashed",
             colour = "#fa756a") + 
  geom_hline(yintercept = median_models[2], linetype = "dashed",
             colour = "#7bb000") + 
  geom_hline(yintercept = median_models[3], linetype = "dashed",
             colour = "#00bfc5") + 
  geom_hline(yintercept = median_models[4], linetype = "dashed",
             colour = "#c877ff") +
  theme(axis.text.x = element_text(color = "black", size = 10, face = "bold"),
      axis.text.y = element_text(color = "black", size = 10, face = "bold"),
      #axis.title.x = element_text(color = "black", size = 15, face = "bold"),
      axis.title.y = element_text(color = "black", size = 15, face = "bold"),
      #legend.title = element_text(color = "black", size = 15, face = "bold"),
      legend.text = element_text(color = "black", size = 10, face = "bold"))
ggsave(plot = f11, filename = "/Users/JoaoPimentel/Documents/Github/edr_paper_results/figures/f11.png", dpi = "retina", units = "cm", width = 40, height = 20)

# ---------------- Figure 12 ----------------
performance_boxplots <- function(rules, reference_conditions, reference_distribution, label = "") {
  # Initialize data frame
  df <- data.frame(matrix(nrow = 0, ncol = 2))
  colnames(df) <- c("label_values", "distribution_values")
  # Reference information
  reference_text <- paste(reference_conditions %>% unlist() %>% sort(), collapse = ",")
  reference_size <- length(reference_distribution)
  reference_labels <- rep(c(reference_text), each = reference_size)
  # Update data frame
  tmp <- data.frame(reference_labels, reference_distribution)
  colnames(tmp) <- c("label_values", "distribution_values")
  df <- rbind(df, tmp)
  # Initialize vector that contains all text information to factorize
  subgroup_levels <- c()
  
  for (i in seq_len(nrow(rules))) {
    # Subgroup information
    subgroup_text <- paste(rules[i, ]$Subgroup %>% unlist() %>% sort(), collapse = ",")
    subgroup_distribution <- rules[i, ]$Dist %>% as.vector.dist()
    subgroup_size <- length(subgroup_distribution)
    subgroup_labels <- rep(c(subgroup_text), each = subgroup_size)
    # Update data frame
    tmp <- data.frame(subgroup_labels, subgroup_distribution)
    colnames(tmp) <- c("label_values", "distribution_values")
    df <- rbind(df, tmp)
    # Update vector
    subgroup_levels <- c(subgroup_levels, subgroup_text)
  }
  
  subgroup_levels <- subgroup_levels %>% sort()
  df$label_values <- df$label_values %>% factor(ordered = TRUE, levels = c(subgroup_levels, reference_text))
  
  quartile_values <- boxplot.stats(reference_distribution)$stats
  
  # Plot
  plot <- ggplot(df, aes(x = distribution_values, y = label_values)) +
    geom_boxplot() +
    xlab(label) +
    ylab ("")
  
  plot_build <- ggplot_build(plot)$data[[1]] %>% head(-1)
  
  return(plot +
           # LW
           geom_segment(data = plot_build, aes(x = xmin, xend = xmin, y = ymin, yend = ymax, colour = ifelse(plot_build$xmin > quartile_values[[1]], "red", ifelse(plot_build$xmin == quartile_values[[1]], "blue", "darkgreen"))), size = 1) +
           # Q1
           geom_segment(data = plot_build, aes(x = xlower, xend = xlower, y = ymin, yend = ymax, colour = ifelse(plot_build$xlower > quartile_values[[2]], "red", ifelse(plot_build$xlower == quartile_values[[2]], "blue", "darkgreen"))), size = 1) +
           # Median
           geom_segment(data = plot_build, aes(x = xmiddle, xend = xmiddle, y = ymin, yend = ymax, colour = ifelse(plot_build$xmiddle > quartile_values[[3]], "red", ifelse(plot_build$xmiddle == quartile_values[[3]], "blue", "darkgreen"))), size = 1) +
           # Q3
           geom_segment(data = plot_build, aes(x = xupper, xend = xupper, y = ymin, yend = ymax, colour = ifelse(plot_build$xupper > quartile_values[[4]], "red", ifelse(plot_build$xupper == quartile_values[[4]], "blue", "darkgreen"))), size = 1) +
           # UW
           geom_segment(data = plot_build, aes(x = xmax, xend = xmax, y = ymin, yend = ymax, colour = ifelse(plot_build$xmax > quartile_values[[5]], "red", ifelse(plot_build$xmax == quartile_values[[5]], "blue", "darkgreen"))), size = 1) +
           # Legend
           scale_color_manual(breaks = c("darkgreen", "blue", "red"), values = c("darkgreen", "blue", "red"), labels = c("Lower", "Equal", "Higher")) +
           theme(legend.position = "bottom", legend.box = "horizontal") +
           labs(colour = "Value comparison to reference: ")
  )
}

f12 <- performance_boxplots(rules = masters_svm$drs %>% head(nrow(masters_svm$drs) - 1), reference_conditions = list(), reference_distribution = masters_svm$drs[nrow(masters_svm$drs), ]$Dist %>% as.vector.dist(), label = "Log error") + 
  theme(axis.text.x = element_text(color = "black", size = 10, face = "bold"),
        axis.text.y = element_text(color = "black", size = 10, face = "bold"),
        axis.title.x = element_text(color = "black", size = 15, face = "bold"),
        axis.title.y = element_text(color = "black", size = 15, face = "bold"),
        legend.title = element_text(color = "black", size = 15, face = "bold"),
        legend.text = element_text(color = "black", size = 10, face = "bold"))
ggsave(plot = f12, filename = "/Users/JoaoPimentel/Documents/Github/edr_paper_results/figures/f12.png", dpi = "retina", units = "cm", width = 35, height = 30)

# ---------------- Figure 13 ----------------
plot_all <- function(x, x_label, y, y_label, type = "Boxplot", ylabel) {
  rows_x <- length(x)
  
  rows_y <- length(y)
  target <- c(x, y)
  
  # Dataframe using both distributions
  x_sub <- rep(c(x_label), each = rows_x)
  y_sub <- rep(c(y_label), each = rows_y)
  
  group <- c(x_sub, y_sub)
  
  df <- data.frame(group, target)
  
  # Generate legends for each plot
  legends <- list()
  legends[x_label] <- paste0(x_label, "\n", rows_x, " instances")
  legends[y_label] <- paste0(y_label, "\n", rows_y, " instances")
  
  if (type == "Boxplot") {
    # Plot
    plot <- ggplot(data = df, aes(x = group, y = target, fill = group)) +
      theme(legend.position = "none", axis.title.x = element_blank(),
            plot.title = element_text(face = "bold")) +
      labs(y = ylabel) + 
      geom_boxplot() +
      scale_x_discrete("group", labels = legends)
    
    return(plot)
  }
  else if (type == "Density") {
    # Plot
    p <- ggplot(data = df, aes(x = target, fill = group)) +
      theme(legend.title = element_blank(), legend.position = "bottom",
            legend.box = "horizontal",
            plot.title = element_text(face = "bold")) +
      labs(x = ylabel) +
      geom_density(alpha = 0.5) +
      scale_fill_discrete("group", labels = legends)
    return(p)
  }
} 

dataset_with_error_svm <- single_model_data(
  data = datasets_with_preds_discretized$a1[, setdiff(names(datasets_with_preds_discretized$a1), c("nnet", "gbm", "svm", "rf"))],
  model = datasets_with_preds_discretized$a1$svm,
  feature_y = "a1",
  type = "Residual")
save(df, file = "/Users/JoaoPimentel/Desktop/edr_paper_content/a1_svm_residual.RData")

dataset_with_error_ann <- single_model_data(
  data = datasets_with_preds_discretized$a1[, setdiff(names(datasets_with_preds_discretized$a1), c("nnet", "gbm", "svm", "rf"))],
  model = datasets_with_preds_discretized$a1$nnet,
  feature_y = "a1",
  type = "Residual")
save(df, file = "/Users/JoaoPimentel/Desktop/edr_paper_content/a1_nnet_residual.RData")

f13b <- plot_all(dataset_with_error_svm$error, "SVM", dataset_with_error_ann$error, "ANN", "Boxplot", "Residual error")   + 
  theme(axis.text.x = element_text(color = "black", size = 10, face = "bold"),
        axis.text.y = element_text(color = "black", size = 10, face = "bold"),
        #axis.title.x = element_text(color = "black", size = 15, face = "bold"),
        axis.title.y = element_text(color = "black", size = 15, face = "bold"),
        #legend.title = element_text(color = "black", size = 15, face = "bold"),
        legend.text = element_text(color = "black", size = 10, face = "bold"))
ggsave(plot = f13b, filename = "/Users/JoaoPimentel/Desktop/edr_paper_content/figures/f13b.png", dpi = "retina", units = "cm", width = 15, height = 12)

f13a <- plot_all(abs(dataset_with_error_svm$error), "SVM", abs(dataset_with_error_ann$error), "ANN", "Boxplot", "Absolute error") + 
  theme(axis.text.x = element_text(color = "black", size = 10, face = "bold"),
        axis.text.y = element_text(color = "black", size = 10, face = "bold"),
        #axis.title.x = element_text(color = "black", size = 15, face = "bold"),
        axis.title.y = element_text(color = "black", size = 15, face = "bold"),
        #legend.title = element_text(color = "black", size = 15, face = "bold"),
        legend.text = element_text(color = "black", size = 10, face = "bold"))
ggsave(plot = f13a, filename = "/Users/JoaoPimentel/Desktop/edr_paper_content/figures/f13a.png", dpi = "retina", units = "cm", width = 15, height = 12)

# ---------------- Figure 14 ----------------
label <- "|E(svm)| - |E(ann)|"
df <- datasets_with_preds_discretized$a1[, setdiff(names(datasets_with_preds_discretized$a1), c("nnet", "gbm", "svm", "rf", "a1"))]
df$error <- abs(dataset_with_error_svm$error) - abs(dataset_with_error_ann$error)
save(df, file = "/Users/JoaoPimentel/Desktop/edr_paper_content/a1_svm_minus_nnet.RData")

drs <- generate_rules(data = df, min.conf = 0.6, min.sup = 0.05, imp = 0.00000000000001)
save(drs, file = "/Users/JoaoPimentel/Desktop/edr_paper_content/a1_svm_minus_nnet_edrs.RData")

general_population <- drs[nrow(drs),]

# Figure 14a
f14a <- plot_all(as.vector.dist(general_population$Dist), "Whole Population", NULL, NULL, "Boxplot", label) + 
  theme(axis.text.x = element_text(color = "black", size = 10, face = "bold"),
        axis.text.y = element_text(color = "black", size = 10, face = "bold"),
        #axis.title.x = element_text(color = "black", size = 15, face = "bold"),
        axis.title.y = element_text(color = "black", size = 15, face = "bold"),
        legend.title = element_text(color = "black", size = 15, face = "bold"),
        legend.text = element_text(color = "black", size = 10, face = "bold"))
ggsave(plot = f14a, filename = "/Users/JoaoPimentel/Desktop/edr_paper_content/figures/f14a.png", dpi = "retina", units = "cm", width = 15, height = 12)
# Figure 14b
f14b <- plot_all(as.vector.dist(general_population$Dist), "Whole Population", NULL, NULL, "Density", label) + 
  theme(axis.text.x = element_text(color = "black", size = 10, face = "bold"),
        axis.text.y = element_text(color = "black", size = 10, face = "bold"),
        axis.title.x = element_text(color = "black", size = 15, face = "bold"),
        axis.title.y = element_text(color = "black", size = 15, face = "bold"),
        #legend.title = element_text(color = "black", size = 15, face = "bold"),
        legend.text = element_text(color = "black", size = 10, face = "bold"))
ggsave(plot = f14b, filename = "/Users/JoaoPimentel/Desktop/edr_paper_content/figures/f14b.png", dpi = "retina", units = "cm", width = 15, height = 12)

# ---------------- Figure 15 ----------------
which(get_subgroups(drs) %in% c("oPO4=[1-7.3], mnO2=[7.6-10.29]"))

# Figure 15a
f15a <- plot_rule(drs[6, ], general_population, ylabel = label) + 
  theme(axis.text.x = element_text(color = "black", size = 10, face = "bold"),
        axis.text.y = element_text(color = "black", size = 10, face = "bold"),
        #axis.title.x = element_text(color = "black", size = 15, face = "bold"),
        axis.title.y = element_text(color = "black", size = 15, face = "bold"),
        legend.title = element_text(color = "black", size = 15, face = "bold"),
        legend.text = element_text(color = "black", size = 10, face = "bold"))
ggsave(plot = f15a, filename = "/Users/JoaoPimentel/Desktop/edr_paper_content/figures/f15a.png", dpi = "retina", units = "cm", width = 15, height = 12)
# Figure 15b
f15b <- plot_rule(drs[6, ], general_population, ylabel = label, type = "Density")  + 
  theme(axis.text.x = element_text(color = "black", size = 10, face = "bold"),
        axis.text.y = element_text(color = "black", size = 10, face = "bold"),
        axis.title.x = element_text(color = "black", size = 15, face = "bold"),
        axis.title.y = element_text(color = "black", size = 15, face = "bold"),
        #legend.title = element_text(color = "black", size = 15, face = "bold"),
        legend.text = element_text(color = "black", size = 10, face = "bold"))
ggsave(plot = f15b, filename = "/Users/JoaoPimentel/Desktop/edr_paper_content/figures/f15b.png", dpi = "retina", units = "cm", width = 15, height = 12)

# ---------------- Figure 16 ----------------
which(get_subgroups(drs) %in% c("NH4=[931.83-24064]"))

# Figure 16a
f16a <- plot_rule(drs[4, ], general_population, ylabel = label) + 
  theme(axis.text.x = element_text(color = "black", size = 10, face = "bold"),
        axis.text.y = element_text(color = "black", size = 10, face = "bold"),
        #axis.title.x = element_text(color = "black", size = 15, face = "bold"),
        axis.title.y = element_text(color = "black", size = 15, face = "bold"),
        legend.title = element_text(color = "black", size = 15, face = "bold"),
        legend.text = element_text(color = "black", size = 10, face = "bold"))
ggsave(plot = f16a, filename = "/Users/JoaoPimentel/Desktop/edr_paper_content/figures/f16a.png", dpi = "retina", units = "cm", width = 15, height = 12)
# Figure 16b
f16b <- plot_rule(drs[4, ], general_population, ylabel = label, type = "Density")  + 
  theme(axis.text.x = element_text(color = "black", size = 10, face = "bold"),
        axis.text.y = element_text(color = "black", size = 10, face = "bold"),
        axis.title.x = element_text(color = "black", size = 15, face = "bold"),
        axis.title.y = element_text(color = "black", size = 15, face = "bold"),
        #legend.title = element_text(color = "black", size = 15, face = "bold"),
        legend.text = element_text(color = "black", size = 10, face = "bold"))
ggsave(plot = f16b, filename = "/Users/JoaoPimentel/Desktop/edr_paper_content/figures/f16b.png", dpi = "retina", units = "cm", width = 15, height = 12)


