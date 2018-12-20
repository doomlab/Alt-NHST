
library(reshape)
library(ggplot2)
library(cowplot)

#load graph data
Graph_data <- read.csv("Graph_data.05.csv")
long_graph = melt(Graph_data,
                  id = c("Effect","Significance","N"),
                  measured = c("Parametric","Non-Parametric","Bayes","OOM"))
colnames(long_graph) = c("Effect","Significance", "N","Analysis","value")


theme = theme(panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(), 
              panel.background = element_blank(), 
              axis.line.x = element_line(colour = "black"), 
              axis.line.y = element_line(colour = "black"),
              legend.key = element_rect(fill = "white"),
              text = element_text(size = 15))



###################################################### percent sig (.05) findings graphs
sig_data = subset(long_graph, Significance=="Sig")
sig_data_large = subset(sig_data, Effect=="Large")

p1 = ggplot(sig_data_large) + 
  geom_line(aes(x=N, 
                y=value, 
                group = Analysis,
                linetype = Analysis, 
                colour = Analysis),
            size=0.75) + 
  labs(title="Large") +
  coord_cartesian(ylim = c(0,101)) + 
  ylab("Percent Significant") + 
  theme +
  geom_point(data=sig_data_large,
             aes(x = N,
                 y = value,
                 shape = Analysis), 
             size = 3)+
  scale_linetype_manual(values=c("solid", "solid","solid","solid")) +
  scale_color_manual(values = c("gray50","gray50","gray50","gray50")) + 
  scale_shape_manual(values = c(3, 4, 5, 6))

sig_data_medium = subset(sig_data, Effect=="Medium")
p2 = ggplot(sig_data_medium) + 
  geom_line(aes(x=N, 
                y=value, 
                group = Analysis,
                linetype = Analysis, 
                colour = Analysis),
            size=0.75) + 
  labs(title="Medium") +
  coord_cartesian(ylim = c(0,101)) + 
  ylab("Percent Significant") + 
  theme +
  geom_point(data=sig_data_medium,
             aes(x = N,
                 y = value,
                 shape = Analysis), 
             size = 3)+
  scale_linetype_manual(values=c("solid", "solid","solid","solid")) +
  scale_color_manual(values = c("gray50","gray50","gray50","gray50")) + 
  scale_shape_manual(values = c(3, 4, 5, 6))


sig_data_small = subset(sig_data, Effect=="Small")
p3 = ggplot(sig_data_small) + 
  geom_line(aes(x=N, 
                y=value, 
                group = Analysis,
                linetype = Analysis, 
                colour = Analysis),
            size=0.75) + 
  labs(title="Small") +
  coord_cartesian(ylim = c(0,101)) + 
  ylab("Percent Significant") + 
  theme +
  geom_point(data=sig_data_small,
             aes(x = N,
                 y = value,
                 shape = Analysis), 
             size = 3)+
  scale_linetype_manual(values=c("solid", "solid","solid","solid")) +
  scale_color_manual(values = c("gray50","gray50","gray50","gray50")) + 
  scale_shape_manual(values = c(3, 4, 5, 6))

sig_data_none = subset(sig_data, Effect=="None")
p4 = ggplot(sig_data_none) + 
  geom_line(aes(x=N, 
                y=value, 
                group = Analysis,
                linetype = Analysis, 
                colour = Analysis),
            size=0.75) + 
  labs(title="None") +
  coord_cartesian(ylim = c(0,101)) + 
  ylab("Percent Significant") + 
  theme +
  geom_point(data=sig_data_none,
             aes(x = N,
                 y = value,
                 shape = Analysis), 
             size = 3)+
  scale_linetype_manual(values=c("solid", "solid","solid","solid")) +
  scale_color_manual(values = c("gray50","gray50","gray50","gray50")) + 
  scale_shape_manual(values = c(3, 4, 5, 6))


####put the graphs together####
legend = get_legend(p1)
prow <- plot_grid( p4 + theme(legend.position="none"),
                   p3 + theme(legend.position="none"),
                   p2 + theme(legend.position="none"),
                   p1 + theme(legend.position="none"),
                   legend,
                   hjust = -1,
                   nrow = 3
)
prow



###################################################### percent sig (.005) findings graphs
#load graph data
Graph_data2 <- read.csv("Graph_data.05.csv")
long_graph2 = melt(Graph_data2,
                  id = c("Effect","Significance","N"),
                  measured = c("Parametric","Non-Parametric","Bayes","OOM"))
colnames(long_graph2) = c("Effect","Significance", "N","Analysis","value")

sig_data2 = subset(long_graph2, Significance=="Sig")
sig_data_large2 = subset(sig_data2, Effect=="Large")

p12 = ggplot(sig_data_large2) + 
  geom_line(aes(x=N, 
                y=value, 
                group = Analysis,
                linetype = Analysis, 
                colour = Analysis),
            size=0.75) + 
  labs(title="Large") +
  coord_cartesian(ylim = c(0,101)) + 
  ylab("Percent Significant") + 
  theme +
  geom_point(data=sig_data_large,
             aes(x = N,
                 y = value,
                 shape = Analysis), 
             size = 3)+
  scale_linetype_manual(values=c("solid", "solid","solid","solid")) +
  scale_color_manual(values = c("gray50","gray50","gray50","gray50")) + 
  scale_shape_manual(values = c(3, 4, 5, 6))

sig_data_medium2 = subset(sig_data2, Effect=="Medium")
p22 = ggplot(sig_data_medium2) + 
  geom_line(aes(x=N, 
                y=value, 
                group = Analysis,
                linetype = Analysis, 
                colour = Analysis),
            size=0.75) + 
  labs(title="Medium") +
  coord_cartesian(ylim = c(0,101)) + 
  ylab("Percent Significant") + 
  theme +
  geom_point(data=sig_data_medium,
             aes(x = N,
                 y = value,
                 shape = Analysis), 
             size = 3)+
  scale_linetype_manual(values=c("solid", "solid","solid","solid")) +
  scale_color_manual(values = c("gray50","gray50","gray50","gray50")) + 
  scale_shape_manual(values = c(3, 4, 5, 6))


sig_data_small2 = subset(sig_data2, Effect=="Small")
p32 = ggplot(sig_data_small2) + 
  geom_line(aes(x=N, 
                y=value, 
                group = Analysis,
                linetype = Analysis, 
                colour = Analysis),
            size=0.75) + 
  labs(title="Small") +
  coord_cartesian(ylim = c(0,101)) + 
  ylab("Percent Significant") + 
  theme +
  geom_point(data=sig_data_small,
             aes(x = N,
                 y = value,
                 shape = Analysis), 
             size = 3)+
  scale_linetype_manual(values=c("solid", "solid","solid","solid")) +
  scale_color_manual(values = c("gray50","gray50","gray50","gray50")) + 
  scale_shape_manual(values = c(3, 4, 5, 6))

sig_data_none2 = subset(sig_data2, Effect=="None")
p42 = ggplot(sig_data_none2) + 
  geom_line(aes(x=N, 
                y=value, 
                group = Analysis,
                linetype = Analysis, 
                colour = Analysis),
            size=0.75) + 
  labs(title="None") +
  coord_cartesian(ylim = c(0,101)) + 
  ylab("Percent Significant") + 
  theme +
  geom_point(data=sig_data_none,
             aes(x = N,
                 y = value,
                 shape = Analysis), 
             size = 3)+
  scale_linetype_manual(values=c("solid", "solid","solid","solid")) +
  scale_color_manual(values = c("gray50","gray50","gray50","gray50")) + 
  scale_shape_manual(values = c(3, 4, 5, 6))


####put the graphs together####
legend2 = get_legend(p12)
prow2 <- plot_grid( p42 + theme(legend.position="none"),
                   p32 + theme(legend.position="none"),
                   p22 + theme(legend.position="none"),
                   p12 + theme(legend.position="none"),
                   legend,
                   hjust = -1,
                   nrow = 3
)
prow2



###################################################### percent non-significant findings graphs
non_data = subset(long_graph, Significance=="Non")
non_data_large = subset(non_data, Effect=="Large")

a1 = ggplot(non_data_large) + 
  geom_line(aes(x=N, 
                y=value, 
                group = Analysis,
                linetype = Analysis, 
                colour = Analysis),
            size=0.75) + 
  labs(title="Large") +
  coord_cartesian(ylim = c(0,101)) + 
  ylab("Percent Non-Significant") + 
  theme +
  geom_point(data=non_data_large,
             aes(x = N,
                 y = value,
                 shape = Analysis), 
             size = 3)+
  scale_linetype_manual(values=c("solid", "solid","solid","solid")) +
  scale_color_manual(values = c("gray50","gray50","gray50","gray50")) + 
  scale_shape_manual(values = c(3, 4, 5, 6))

non_data_medium = subset(non_data, Effect=="Medium")
a2 = ggplot(non_data_medium) + 
  geom_line(aes(x=N, 
                y=value, 
                group = Analysis,
                linetype = Analysis, 
                colour = Analysis),
            size=0.75) + 
  labs(title="Medium") +
  coord_cartesian(ylim = c(0,101)) + 
  ylab("Percent Non-Significant") + 
  theme +
  geom_point(data=non_data_medium,
             aes(x = N,
                 y = value,
                 shape = Analysis), 
             size = 3)+
  scale_linetype_manual(values=c("solid", "solid","solid","solid")) +
  scale_color_manual(values = c("gray50","gray50","gray50","gray50")) + 
  scale_shape_manual(values = c(3, 4, 5, 6))

non_data_small = subset(non_data, Effect=="Small")
a3 = ggplot(non_data_small) + 
  geom_line(aes(x=N, 
                y=value, 
                group = Analysis,
                linetype = Analysis, 
                colour = Analysis),
            size=0.75) + 
  labs(title="Small") +
  coord_cartesian(ylim = c(0,101)) + 
  ylab("Percent Non-Significant") + 
  theme +
  geom_point(data=non_data_small,
             aes(x = N,
                 y = value,
                 shape = Analysis), 
             size = 3)+
  scale_linetype_manual(values=c("solid", "solid","solid","solid")) +
  scale_color_manual(values = c("gray50","gray50","gray50","gray50")) + 
  scale_shape_manual(values = c(3, 4, 5, 6))


non_data_none = subset(non_data, Effect=="None")
a4 = ggplot(non_data_none) + 
  geom_line(aes(x=N, 
                y=value, 
                group = Analysis,
                linetype = Analysis, 
                colour = Analysis),
            size=0.75) + 
  labs(title="None") +
  coord_cartesian(ylim = c(0,101)) + 
  ylab("Percent Non-Significant") + 
  theme +
  geom_point(data=non_data_none,
             aes(x = N,
                 y = value,
                 shape = Analysis), 
             size = 3)+
  scale_linetype_manual(values=c("solid", "solid","solid","solid")) +
  scale_color_manual(values = c("gray50","gray50","gray50","gray50")) + 
  scale_shape_manual(values = c(3, 4, 5, 6))

####put the graphs together####
legend = get_legend(a1)
arow <- plot_grid( a4 + theme(legend.position="none"),
                   a3 + theme(legend.position="none"),
                   a2 + theme(legend.position="none"),
                   a1 + theme(legend.position="none"),
                   legend,
                   hjust = -1,
                   nrow = 3
)
arow



###################################################### percent 100 agree .05
Graph_data_agree.05 <- read.csv("C:/Users/John/Desktop/Graph_data_agree.05.csv")
agreelong = melt(Graph_data_agree.05,
                 id = c("Effect", "N"),
                 measured = c("Omnibus", "1v2", "2v3"))
colnames(agreelong)[3:4] = c("comparison", "percent")

agree_large = subset(agreelong, Effect == "Large")
w1 = ggplot(agree_large) + 
  geom_line(aes(x=N, 
                y=percent, 
                group = comparison,
                linetype = comparison, 
                colour = comparison),
            size=0.75) + 
  labs(title="Large") +
  coord_cartesian(ylim = c(0,101)) + 
  ylab("Percent Agreement") + 
  theme +
  geom_point(data=agree_large,
             aes(x = N,
                 y = percent,
                 shape = comparison), 
             size = 3)+
  scale_linetype_manual(name = "Comparison", 
                        labels = c("Omnibus", "1 to 2", "2 to 3"),
                        values=c("solid", "solid","solid")) +
  scale_color_manual(name = "Comparison",
                     labels = c("Omnibus", "1 to 2", "2 to 3"),
                     values = c("gray50","gray50","gray50")) + 
  scale_shape_manual(name = "Comparison",
                     labels = c("Omnibus", "1 to 2", "2 to 3"),
                     values = c(3, 4, 5))

agree_medium = subset(agreelong, Effect == "Medium")
w2 = ggplot(agree_medium) + 
  geom_line(aes(x=N, 
                y=percent, 
                group = comparison,
                linetype = comparison, 
                colour = comparison),
            size=0.75) + 
  labs(title="Medium") +
  coord_cartesian(ylim = c(0,101)) + 
  ylab("Percent Agreement") + 
  theme +
  geom_point(data=agree_medium,
             aes(x = N,
                 y = percent,
                 shape = comparison), 
             size = 3)+
  scale_linetype_manual(name = "Comparison", 
                        labels = c("Omnibus", "1 to 2", "2 to 3"),
                        values=c("solid", "solid","solid")) +
  scale_color_manual(name = "Comparison",
                     labels = c("Omnibus", "1 to 2", "2 to 3"),
                     values = c("gray50","gray50","gray50")) + 
  scale_shape_manual(name = "Comparison",
                     labels = c("Omnibus", "1 to 2", "2 to 3"),
                     values = c(3, 4, 5))

agree_small = subset(agreelong, Effect == "Small")
w3 = ggplot(agree_small) + 
  geom_line(aes(x=N, 
                y=percent, 
                group = comparison,
                linetype = comparison, 
                colour = comparison),
            size=0.75) + 
  labs(title="Small") +
  coord_cartesian(ylim = c(0,101)) + 
  ylab("Percent Agreement") + 
  theme +
  geom_point(data=agree_small,
             aes(x = N,
                 y = percent,
                 shape = comparison), 
             size = 3)+
  scale_linetype_manual(name = "Comparison", 
                        labels = c("Omnibus", "1 to 2", "2 to 3"),
                        values=c("solid", "solid","solid")) +
  scale_color_manual(name = "Comparison",
                     labels = c("Omnibus", "1 to 2", "2 to 3"),
                     values = c("gray50","gray50","gray50")) + 
  scale_shape_manual(name = "Comparison",
                     labels = c("Omnibus", "1 to 2", "2 to 3"),
                     values = c(3, 4, 5))

agree_none = subset(agreelong, Effect == "None")
w4 = ggplot(agree_none) + 
  geom_line(aes(x=N, 
                y=percent, 
                group = comparison,
                linetype = comparison, 
                colour = comparison),
            size=0.75) + 
  labs(title="None") +
  coord_cartesian(ylim = c(0,101)) + 
  ylab("Percent Agreement") + 
  theme +
  geom_point(data=agree_none,
             aes(x = N,
                 y = percent,
                 shape = comparison), 
             size = 3)+
  scale_linetype_manual(name = "Comparison", 
                        labels = c("Omnibus", "1 to 2", "2 to 3"),
                        values=c("solid", "solid","solid")) +
  scale_color_manual(name = "Comparison",
                     labels = c("Omnibus", "1 to 2", "2 to 3"),
                     values = c("gray50","gray50","gray50")) + 
  scale_shape_manual(name = "Comparison",
                     labels = c("Omnibus", "1 to 2", "2 to 3"),
                     values = c(3, 4, 5))

####put the graphs together####
legend = get_legend(w1)
wrow <- plot_grid( w4 + theme(legend.position="none"),
                   w3 + theme(legend.position="none"),
                   w2 + theme(legend.position="none"),
                   w1 + theme(legend.position="none"),
                   legend,
                   hjust = -1,
                   nrow = 3
)
wrow



###################################################### percent 100 agree .005
Graph_data_agree.005 <- read.csv("C:/Users/John/Desktop/Graph_data_agree.005.csv")
agreelong2 = melt(Graph_data_agree.005,
                 id = c("Effect", "N"),
                 measured = c("Omnibus", "1v2", "2v3"))
colnames(agreelong2)[3:4] = c("comparison", "percent")

agree_large2 = subset(agreelong2, Effect == "Large")
w12 = ggplot(agree_large2) + 
  geom_line(aes(x=N, 
                y=percent, 
                group = comparison,
                linetype = comparison, 
                colour = comparison),
            size=0.75) + 
  labs(title="Large") +
  coord_cartesian(ylim = c(0,101)) + 
  ylab("Percent Agreement") + 
  theme +
  geom_point(data=agree_large,
             aes(x = N,
                 y = percent,
                 shape = comparison), 
             size = 3)+
  scale_linetype_manual(name = "Comparison", 
                        labels = c("Omnibus", "1 to 2", "2 to 3"),
                        values=c("solid", "solid","solid")) +
  scale_color_manual(name = "Comparison",
                     labels = c("Omnibus", "1 to 2", "2 to 3"),
                     values = c("gray50","gray50","gray50")) + 
  scale_shape_manual(name = "Comparison",
                     labels = c("Omnibus", "1 to 2", "2 to 3"),
                     values = c(3, 4, 5))

agree_medium2 = subset(agreelong2, Effect == "Medium")
w22 = ggplot(agree_medium2) + 
  geom_line(aes(x=N, 
                y=percent, 
                group = comparison,
                linetype = comparison, 
                colour = comparison),
            size=0.75) + 
  labs(title="Medium") +
  coord_cartesian(ylim = c(0,101)) + 
  ylab("Percent Agreement") + 
  theme +
  geom_point(data=agree_medium,
             aes(x = N,
                 y = percent,
                 shape = comparison), 
             size = 3)+
  scale_linetype_manual(name = "Comparison", 
                        labels = c("Omnibus", "1 to 2", "2 to 3"),
                        values=c("solid", "solid","solid")) +
  scale_color_manual(name = "Comparison",
                     labels = c("Omnibus", "1 to 2", "2 to 3"),
                     values = c("gray50","gray50","gray50")) + 
  scale_shape_manual(name = "Comparison",
                     labels = c("Omnibus", "1 to 2", "2 to 3"),
                     values = c(3, 4, 5))

agree_small2 = subset(agreelong2, Effect == "Small")
w32 = ggplot(agree_small2) + 
  geom_line(aes(x=N, 
                y=percent, 
                group = comparison,
                linetype = comparison, 
                colour = comparison),
            size=0.75) + 
  labs(title="Small") +
  coord_cartesian(ylim = c(0,101)) + 
  ylab("Percent Agreement") + 
  theme +
  geom_point(data=agree_small,
             aes(x = N,
                 y = percent,
                 shape = comparison), 
             size = 3)+
  scale_linetype_manual(name = "Comparison", 
                        labels = c("Omnibus", "1 to 2", "2 to 3"),
                        values=c("solid", "solid","solid")) +
  scale_color_manual(name = "Comparison",
                     labels = c("Omnibus", "1 to 2", "2 to 3"),
                     values = c("gray50","gray50","gray50")) + 
  scale_shape_manual(name = "Comparison",
                     labels = c("Omnibus", "1 to 2", "2 to 3"),
                     values = c(3, 4, 5))

agree_none2 = subset(agreelong2, Effect == "None")
w42 = ggplot(agree_none2) + 
  geom_line(aes(x=N, 
                y=percent, 
                group = comparison,
                linetype = comparison, 
                colour = comparison),
            size=0.75) + 
  labs(title="None") +
  coord_cartesian(ylim = c(0,101)) + 
  ylab("Percent Agreement") + 
  theme +
  geom_point(data=agree_none,
             aes(x = N,
                 y = percent,
                 shape = comparison), 
             size = 3)+
  scale_linetype_manual(name = "Comparison", 
                        labels = c("Omnibus", "1 to 2", "2 to 3"),
                        values=c("solid", "solid","solid")) +
  scale_color_manual(name = "Comparison",
                     labels = c("Omnibus", "1 to 2", "2 to 3"),
                     values = c("gray50","gray50","gray50")) + 
  scale_shape_manual(name = "Comparison",
                     labels = c("Omnibus", "1 to 2", "2 to 3"),
                     values = c(3, 4, 5))

####put the graphs together####
legend2 = get_legend(w12)
wrow2 <- plot_grid( w42 + theme(legend.position="none"),
                   w32 + theme(legend.position="none"),
                   w22 + theme(legend.position="none"),
                   w12 + theme(legend.position="none"),
                   legend2,
                   hjust = -1,
                   nrow = 3
)
wrow2


