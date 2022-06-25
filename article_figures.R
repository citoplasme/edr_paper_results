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

# ---------------- Figure 6 ----------------
# EDPS

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

# EDPS

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
# MASTERS

# ---------------- Figure 12 ----------------
# MASTERS

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


