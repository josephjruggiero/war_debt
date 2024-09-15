b <- .9
k <- 2
s <- .4

liqcons <- function(x) {
  y <- 1 + (1-b)/(b*x)*(1+((1+b)*k/s))
  return(y)
}

x <- seq(.01,1,.01)
y <- liqcons(x)

df <- tbl_df(data.frame(x,y,b,k,s))

loadfonts()

fig1 <- df %>%
  ggplot(aes(x=x, y=y)) +
  geom_line(aes(color = "Liquidity\nConstraint")) +
  geom_ribbon(aes(ymin=y, ymax=100, fill="War"), alpha=0.2) +
  geom_ribbon(aes(ymin=0, ymax=y, fill="Settlement"), alpha=0.2) +
  ylab(TeX('$\\theta$')) +
  xlab(TeX('$\\rho$')) +
  scale_x_continuous(breaks = seq(0, 1, by = .2)) +
  theme_classic() +
  theme(text = element_text(size = 12, family = "LM Roman 10"), 
        axis.title.y = element_text(angle = 0, vjust = 0.5),
        legend.title=element_blank()) +
  scale_color_manual(breaks = c("Liquidity\nConstraint"),
                     values = c("Liquidity\nConstraint" = "blue")) +
  scale_fill_manual(breaks = c("War", "Settlement"),
                    values = c("War" = "black", "Settlement" = "lightblue")) +
  guides(fill = guide_legend(title=NULL, reverse=FALSE)) +
  coord_cartesian(ylim = c(0, 30)) 
fig1

b <- .8
k <- .2
s <- .4
r <- .3

borrcons <- function(x) {
  y <- (x + (1-b)/b - (k/s)*(b+(1-b*(1-r))/(b*(1-b*(1+r)))))/x
  return(y)
}
lendcons <- function(x) {
  y <- (-s/x*(1+r)*(-b*x/(1-b)-(1+(1+b)/s*k))-b*k)/(s/x*(1+r)*(b*x/(1-b))-b/(1-b)*s)
}

y1 <- liqcons(x)
y2 <- borrcons(x)
y3 <- lendcons(x)

fig2_1 <- df %>%
  ggplot(aes(x=x, y=y1)) +
  geom_line(aes(color = "Liquidity")) +
  geom_line(aes(y=y2, color="Borrower's")) +
  geom_line(aes(y=y3, color="Lender's")) +
  geom_ribbon(aes(ymin=y3,ymax=100, fill="War"), alpha=0.2) +
  geom_ribbon(aes(ymin=0,ymax=y3, fill="Settlement"), alpha=0.2) +
  ylab(TeX('$\\theta$')) +
  xlab(TeX('$\\rho$')) +
  scale_x_continuous(breaks = seq(0, 1, by = .2)) +
  theme_classic() +
  theme(text = element_text(size = 12, family = "LM Roman 10"), 
        axis.title.y = element_text(angle = 0, vjust = 0.5),
        legend.title = element_text(size=10)) +
  scale_color_manual(name="Constraints:",
                     breaks = c("Liquidity", "Lender's", "Borrower's"),
                     values = c("Liquidity" = "blue", "Lender's" = "green", 
                                "Borrower's" = "red")) +
  scale_fill_manual(breaks = c("War", "Settlement"),
                    values = c("War" = "black", "Settlement" = "lightblue")) +
  guides(fill = guide_legend(title=NULL, reverse=FALSE)) +
  coord_cartesian(ylim = c(0, 30)) 
fig2_1

b2 <- .99
k2 <- 22
s2 <- .99
r2 <- .15

liqcons2 <- function(x) {
  y <- 1 + (1-b2)/(b2*x)*(1+((1+b2)*k2/s2))
  return(y)
}
borrcons2 <- function(x) {
  y <- (x + (1-b2)/b2 - (k2/s2)*(b2+(1-b2*(1-r2))/(b2*(1-b2*(1+r2)))))/x
  return(y)
}
lendcons2 <- function(x) {
  y <- (-s2/x*(1+r2)*(-b2*x/(1-b2)-(1+(1+b2)/s2*k2))-b2*k2)/(s2/x*(1+r2)*(b2*x/(1-b2))-b2/(1-b2)*s2)
}

y12 <- liqcons2(x)
y22 <- borrcons2(x)
y32 <- lendcons2(x)

fig2_2 <- df %>%
  ggplot(aes(x=x, y=y12)) +
  geom_line(aes(color = "Liquidity")) +
  geom_line(aes(y=y22, color="Borrower's")) +
  geom_line(aes(y=y32, color="Lender's")) +
  geom_ribbon(aes(ymin=y22,ymax=100, fill="War"), alpha=0.2) +
  geom_ribbon(aes(ymin=0,ymax=y22, fill="Settlement"), alpha=0.2) +
  ylab(TeX('$\\theta$')) +
  xlab(TeX('$\\rho$')) +
  scale_x_continuous(breaks = seq(0, 1, by = .2)) +
  theme_classic() +
  theme(text = element_text(size = 12, family = "LM Roman 10"), 
        axis.title.y = element_text(angle = 0, vjust = 0.5),
        legend.title = element_text(size=10)) +
  scale_color_manual(name="Constraints:",
                     breaks = c("Liquidity", "Lender's", "Borrower's"),
                     values = c("Liquidity" = "blue", "Lender's" = "green", 
                                "Borrower's" = "red")) +
  scale_fill_manual(breaks = c("War", "Settlement"),
                    values = c("War" = "black", "Settlement" = "lightblue")) +
  guides(fill = guide_legend(title=NULL, reverse=FALSE)) +
  coord_cartesian(ylim = c(0, 30)) 
fig2_2

get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}
fig2leg <- get_legend(fig2_2)
fig2_1alt <- fig2_1 + theme(legend.position="none")
fig2_2alt <- fig2_2 + theme(legend.position="none",
                            axis.title.y=element_blank(),
                            axis.text.y=element_blank(),
                            axis.ticks.y=element_blank())

grid.arrange(fig2_1alt,fig2_2alt, fig2leg, ncol = 3, widths=c(2.5,2.5,1))

b <- .99
k <- 22
s <- .99
r <- .15

drisk <- function(x) {
  y <- (b/(1+r)*(1/(1-b)*s-k)+s*(1+(1+b)/s*k))/((s*b*x)/(1-b))+1
  return(y)
}

y4 <- drisk(x)

fig3 <- df %>%
  ggplot(aes(x=x, y=y1)) +
  geom_line(aes(color = "Liquidity")) +
  geom_line(aes(y=y4), color="red", linetype = "dotted") +
  geom_line(aes(y=y3, color="Lender's")) +
  geom_ribbon(aes(ymin=y3,ymax=100, fill="War"), alpha=0.2) +
  geom_ribbon(aes(ymin=y4,ymax=y3, fill="No War, Default Risk"), alpha=0.2) +
  geom_ribbon(aes(ymin=y1,ymax=y4, fill="No War, No Risk"), alpha=0.2) +
  geom_ribbon(aes(ymin=0,ymax=y1, fill="No War, No Bond"), alpha=0.2) +
  ylab(TeX('$\\theta$')) +
  xlab(TeX('$\\rho$')) +
  scale_x_continuous(breaks = seq(0, 1, by = .2)) +
  theme_classic() +
  theme(text = element_text(size = 12, family = "LM Roman 10"), 
        axis.title.y = element_text(angle = 0, vjust = 0.5),
        legend.title = element_text(size=10)) +
  scale_color_manual(name="Constraints:",
                     breaks = c("Liquidity", "Lender's"),
                     values = c("Liquidity" = "blue", "Lender's" = "green")) +
  scale_fill_manual(breaks = c("War", "No War, Default Risk", "No War, No Risk", "No War, No Bond"),
                    values = c("War" = "black", "No War, Default Risk" = "red", 
                               "No War, No Risk" = "blue", "No War, No Bond" = "lightblue")) +
  guides(fill = guide_legend(title=NULL, reverse=FALSE)) +
  coord_cartesian(ylim = c(0, 30)) 
fig3

isocurv <- function(x) {
  y <- c/(x*s)+1
  return(y)
}

c <- 5.5
y5 <- isocurv(x)
c <- .35
y6 <- isocurv(x)
c <- 15
y7 <- isocurv(x)

fig4 <- df %>%
  ggplot(aes(x=x, y=y1)) +
  geom_line(aes(color = "Liquidity Const.")) +
  geom_line(aes(y=y4), color="red", linetype = "dotted") +
  geom_line(aes(y=y3, color="Lender's Const.")) +
  geom_ribbon(aes(ymin=y3,ymax=100, fill="War"), alpha=0.2) +
  geom_ribbon(aes(ymin=y4,ymax=y3, fill="Default Risk"), alpha=0.2) +
  geom_line(aes(y=y5, color="Iso-curves"), linetype = "dashed") +
  geom_line(aes(y=y6), color="darkred", linetype = "dashed") +
  geom_line(aes(y=y7), color="darkred", linetype = "dashed") +
  ylab(TeX('$\\theta$')) +
  xlab(TeX('$\\rho$')) +
  scale_x_continuous(breaks = seq(0, 1, by = .2)) +
  theme_classic() +
  theme(text = element_text(size = 12, family = "LM Roman 10"), 
        axis.title.y = element_text(angle = 0, vjust = 0.5),
        legend.title=element_blank()) +
  scale_color_manual(breaks = c("Liquidity Const.", "Lender's Const.", "Iso-curves"),
                     values = c("Liquidity Const." = "blue", "Lender's Const." = "green",
                                "Iso-curves" = "darkred")) +
  scale_fill_manual(breaks = c("War", "Default Risk"),
                    values = c("War" = "black", "Default Risk" = "red")) +
  guides(color = guide_legend(override.aes = list(linetype = c("solid", "solid", "dashed")))) +
  coord_cartesian(ylim = c(0, 30)) +
  annotate("text", x = 0.9, y = 15.5, 
           label = TeX('$E\\[shift\\] = \\bar{\\gamma}$', output="character"), 
           color = "black", size = 3.5, family = "LM Roman 10", parse = TRUE) +
  annotate("text", x = 0.9, y = 5.7, 
           label = TeX('$E\\[shift\\] = \\gamma$', output="character"), 
           color = "black", size = 3.5, family = "LM Roman 10", parse = TRUE) +
  annotate("text", x = 0.9, y = .1, 
           label = TeX('$E\\[shift\\] = \\underline{\\gamma}$', output="character"), 
           color = "black", size = 3.5, family = "LM Roman 10", parse = TRUE) +
  guides(fill = guide_legend(title=NULL, reverse=FALSE)) 
fig4

b <- .9
k <- .1
s <- .15
r <- .6
q <- .9
c <- .2
theta <- isocurv(x)

bondlhs <- function(x) {
  y <- (s/x)*(1+r)*((b*x*(theta-1))/(1-b) - (1+(1+b)/s*k))
  return(y)
}
bondrhs <- function(x) {
  y <- b*(1/(1-b)*theta*s-k)
  return(y)
}

lhs <- bondlhs(x)
rhs <- bondrhs(x)

fig5 <- df %>%
  ggplot(aes(x=x, y=lhs)) +
  geom_line(color = "blue") +
  geom_line(aes(y=rhs), color="green") +
  geom_vline(xintercept=.423, color="red", linetype="dashed") +
  ylab(TeX('$B$')) +
  xlab(TeX('$\\rho$')) +
  scale_x_continuous(breaks = seq(0, 1, by = .2)) +
  theme_classic() +
  theme(text = element_text(size = 12, family = "LM Roman 10"), 
        axis.title.y = element_text(angle = 0, vjust = 0.5),
        legend.key = element_rect(fill = "white", color = "white")) +
  coord_cartesian(ylim = c(0, 15)) +
  annotate("text", x = 0.92, y = 1.5, label = "Bonds necessary\nfor peace", 
           color = "blue", family = "LM Roman 10") +
  annotate("text", x = 0.92, y = 4.5, label = "Max amount of\nbonds available", 
           color = "darkgreen", family = "LM Roman 10") +
  annotate("text", x = 0.52, y = 15, label = "War threshold", 
           color = "darkred", family = "LM Roman 10")
fig5