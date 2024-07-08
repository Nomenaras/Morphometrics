#### Patchwork raw, ratios, rav####

library(patchwork)
# raw

raw3 <- raw_pca  + raw_plot_nmds + raw_plot_lda +
  plot_layout(guides = "collect", ncol = 1) +
  plot_annotation(tag_levels = 'a', tag_prefix = '(', tag_sep = NULL, tag_suffix = ')') &
  theme(plot.tag = element_text(face = 'bold', size = 18, hjust = 0, vjust = 0),
        legend.position = "right",
        legend.direction = 'vertical',
        legend.box = 'vertical')



ggsave("/Users/antman/Documents/raw3.pdf", plot = raw3, width = 1.5*20, height = 1.5*15, unit = "cm")


# ratio

rati3 <- rat_pca + rat_plot_nmds + rat_plot_lda +
  plot_layout(guides = "collect", ncol =1) +
  plot_annotation(tag_levels = 'a', tag_prefix = '(', tag_sep = NULL, tag_suffix = ')') &
  theme(plot.tag = element_text(face = 'bold', size = 18, hjust = 0, vjust = 0),
        legend.position = "right",
        legend.direction = 'vertical',
        legend.box = 'vertical')


ggsave("/Users/antman/Documents/rati3.pdf", plot = rati3 , width = 1.5*20, height = 1.5*15, unit = "cm")

# rav

rav3 <- rav_pca + rav_plot_nmds + rav_plot_lda +
  plot_layout(guides = "collect", ncol = 1, nrow = 3) +
  plot_annotation(tag_levels = 'a', tag_prefix = '(', tag_sep = NULL, tag_suffix = ')') &
  theme(plot.tag = element_text(face = 'bold', size = 18, hjust = 0, vjust = 0),
        legend.position = "right",
        legend.direction = 'vertical',
        legend.box = 'vertical')


ggsave("/Users/antman/Documents/rav3.pdf", plot = rav3 , width = 1.5*20, height = 1.5*15, unit = "cm")

