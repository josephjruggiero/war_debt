# Use extrafonts package and loadfonts() for LM Roman 10 
# Otherwise simply remove family from code

wardebtnames <- read.csv("wardebt-names.csv")
wardebtnames1 <- wardebtnames[!is.na(wardebtnames$pow_prev),]
# Can substitute wardebt for wardebtnames to create plot without names labeled

plotdata1 <- wardebtnames1
plotdata1$intE1 <- m1r_intE$byobs$int$int_eff
plotdata1$zstat1 <- m1r_intE$byobs$int$zstat
plotdata1$se1 <- m1r_intE$byobs$int$se_int_eff

# Load ggrepel package
# To pin down ggrepel
set.seed(1212)
plot1 <- ggplot(plotdata1, aes(x=year, y=intE1)) +
  geom_point(alpha=.8, aes(size=pow_prev, color=real)) +
  geom_point(data = plotdata1[!is.na(plotdata1$plotname),],
             shape = 1, color = "black", stroke= 1.1, 
             show.legend = FALSE, aes(size=pow_prev)) +
  scale_size_continuous(range = c(0, 6.5)) +
  scale_color_gradientn(colors = c("#5a93f4", "#23395e")) +
  labs(y = "Interaction Effect", x = "Year", col = "Rate", size = "Motive") +
  theme_classic() +
  theme(text = element_text(size = 12, family = "LM Roman 10")) +
  geom_line(data=plotdata1[!is.na(plotdata1$plotname),], aes(group = plotname)) +
  geom_text_repel(data=subset(plotdata1, intE1 > 1 & year == 1913), 
                  size=3, box.padding = unit(1.75, "lines"), 
                  family="LM Roman 10", hjust = .35, vjust=.1,
                  aes(label=plotname)) +
  geom_text_repel(data=subset(plotdata1, intE1 > .35 & year == 1990), 
                  size=3, box.padding = unit(1.75, "lines"), 
                  family="LM Roman 10", nudge_y = .1, nudge_x = -.1,
                  aes(label=plotname)) +
  geom_text_repel(data=subset(plotdata1, intE1 == max(plotdata1$intE1)), 
                  size=3, box.padding = unit(1.75, "lines"), 
                  family="LM Roman 10", nudge_x = 20, nudge_y = 5,
                  aes(label=plotname)) +
  geom_text_repel(data=subset(plotdata1, intE1 > .76 & year == 1914), 
                  size=3, box.padding = unit(1.75, "lines"), 
                  family="LM Roman 10", nudge_x = -.1,
                  aes(label=plotname)) +
  geom_text_repel(data=subset(plotdata1, intE1 > .39 & year == 1941), 
                  size=3, box.padding = unit(1.75, "lines"), 
                  family="LM Roman 10", nudge_y = .15, nudge_x = .6,
                  aes(label=plotname)) +
  geom_text_repel(data=subset(plotdata1, intE1 > .3 & year == 1885), 
                  size=3, box.padding = unit(1.75, "lines"), 
                  family="LM Roman 10", nudge_x = -.1,
                  aes(label=plotname)) +
  geom_text_repel(data=subset(plotdata1, intE1 > 0 & year == 1851), 
                  size=3, box.padding = unit(1.75, "lines"), 
                  family="LM Roman 10",
                  aes(label=plotname)) 
plot1
