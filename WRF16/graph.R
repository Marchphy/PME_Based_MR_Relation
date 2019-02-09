library(ggplot2)
library(reshape2)
library(gridExtra) 

################################################################
# Figure 1(a)
###############################################################
pos_WRF16_J2 = as.data.frame(fit_WRF16_J2, pars=c('B1', 'C1', 'gamma1', 'C2' ,'gamma2'))
pos_WRF16_J3 = as.data.frame(fit_WRF16_J3, pars=c('B', 'C1', 'gamma1', 'C2' ,'gamma2', 'C3','gamma3'))

pos_B1_WRF16_J2 = subset(pos_WRF16_J2 , select = c(B1))
pos_B1_WRF16_J2 = melt(pos_B1_WRF16_J2)
colnames(pos_B1_WRF16_J2)[1] = "Knots"

pos_B12_WRF16_J3 = subset(pos_WRF16_J3, select = c('B[1]', 'B[2]'))
pos_B12_WRF16_J3 = melt(pos_B12_WRF16_J3)
colnames(pos_B12_WRF16_J3)[1] <- "Knots"

posplot_B1_WRF16_J2 = ggplot() +
  geom_density(data = pos_B1_WRF16_J2, aes(x=value, color=Knots), alpha=0.25) +
  scale_color_manual(labels = c(expression(B[1])), values = c("royalblue2")) +
  coord_cartesian(xlim = c(0, 8)) + 
  ggtitle("J = 2") +  theme_minimal() +
  theme(legend.title=element_text(size=15), 
        legend.text=element_text(size=15),
        axis.text=element_text(size=15),
        axis.title=element_text(size=15),
        plot.title = element_text(hjust = 0.5, vjust = 0, size = 20),
        text=element_text(family="Helvetica", size=12),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"))

posplot_B12_WRF16_J3 = ggplot() +
  geom_density(data=pos_B12_WRF16_J3, aes(x=value, color=Knots), alpha=0.25) +
  scale_color_manual(labels = c(expression(B[1]), expression(B[2])), values = c("firebrick2", "royalblue2")) +
  coord_cartesian(xlim = c(0, 8)) + 
  ggtitle("J = 3") + theme_minimal() + ylab("") +
  theme(legend.title=element_text(size=15), 
        legend.text=element_text(size=15),
        axis.text=element_text(size=15),
        axis.title=element_text(size=15),
        plot.title = element_text(hjust = 0.5, vjust = 0, size = 20),
        text=element_text(family="Helvetica", size=12),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"))

grid.arrange(posplot_B1_WRF16_J2, posplot_B12_WRF16_J3, ncol=2)


################################################################
# Figure 1(b)
###############################################################
pos_gamma12_WRF16_J2 = subset(pos_WRF16_J2, select = c(gamma1, gamma2))
pos_gamma12_WRF16_J2 = melt(pos_gamma12_WRF16_J2)

pos_gamma123_WRF16_J3 = subset(pos_WRF16_J3, select = c(gamma1, gamma2, gamma3))
pos_gamma123_WRF16_J3 = melt(pos_gamma123_WRF16_J3)

posplot_gamma12_WRF16_J2 = ggplot() +
  geom_density(data = pos_gamma12_WRF16_J2, aes(x=value, color=variable), alpha=0.25) +
  scale_color_discrete(labels = c(expression(gamma[1]), expression(gamma[2]))) +
  coord_cartesian(xlim = c(0, 5)) + theme_minimal() +
  theme(legend.title=element_text(size=15), 
        legend.text=element_text(size=15),
        axis.text=element_text(size=15),
        axis.title=element_text(size=15),
        plot.title = element_text(hjust = 0.5, vjust = 0, size = 20),
        text=element_text(family="Helvetica", size=12),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black")) +
  labs(color='Index')


posplot_gamma123_WRF16_J3 = ggplot() +
  geom_density(data=pos_gamma123_WRF16_J3, aes(x=value, color=variable), alpha=0.25) +
  scale_color_discrete(labels = c(expression(gamma[1]), expression(gamma[2]), expression(gamma[3]))) +
  coord_cartesian(xlim = c(0, 5)) + theme_minimal() + ylab("") +
  theme(legend.title=element_text(size=15), 
        legend.text=element_text(size=15),
        axis.text=element_text(size=15),
        axis.title=element_text(size=15),
        plot.title = element_text(hjust = 0.5, vjust = 0, size = 20),
        text=element_text(family="Helvetica", size=12),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black")) +
  labs(color='Index') 


grid.arrange(posplot_gamma12_WRF16_J2, posplot_gamma123_WRF16_J3, ncol=2)

################################################################
# Figure 1(c)
###############################################################
pos_C12_WRF16_J2 = subset(pos_WRF16_J2, select = c(C1, C2))
pos_C12_WRF16_J2 = melt(pos_C12_WRF16_J2)

pos_C123_WRF16_J3 = subset(pos_WRF16_J3, select = c(C1, C2, C3))
pos_C123_WRF16_J3 = melt(pos_C123_WRF16_J3)

posplot_C12_WRF16_J2 = ggplot() +
  geom_density(data = pos_C12_WRF16_J2, aes(x=value, color=variable),alpha=0.25, n = 1024*2) +
  scale_color_discrete(labels = c(expression(C[1]), expression(C[2]))) +
  coord_cartesian(xlim = c(0, 10)) + theme_minimal() +
  theme(legend.title=element_text(size=15), 
        legend.text=element_text(size=15),
        axis.text=element_text(size=15),
        axis.title=element_text(size=15),
        plot.title = element_text(hjust = 0.5, vjust = 0, size = 20),
        text=element_text(family="Helvetica", size=12),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black")) +
  labs(color='Scale') 

posplot_C123_WRF16_J3 = ggplot() +
  geom_density(data=pos_C123_WRF16_J3, aes(x=value, color=variable), alpha=0.25, n = 2048*2) +
  scale_color_discrete(labels = c(expression(C[1]), expression(C[2]), expression(C[3]))) +
  coord_cartesian(xlim = c(0, 10)) + theme_minimal() + ylab("") +
  theme(legend.title=element_text(size=15), 
        legend.text=element_text(size=15),
        axis.text=element_text(size=15),
        axis.title=element_text(size=15),
        plot.title = element_text(hjust = 0.5, vjust = 0, size = 20),
        text=element_text(family="Helvetica", size=12),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black")) +
  labs(color='Scale') 

grid.arrange(posplot_C12_WRF16_J2, posplot_C123_WRF16_J3, ncol=2)


################################################################
# Figure 2
###############################################################
R_list = seq(0.02, 8, 0.02)

plot_WRF16_J2 = MR_plotData_J2(fit_WRF16_J2, R_list)
expect_M_WRF16_J2 = plot_WRF16_J2[[1]]
pred_M_WRF16_J2 = plot_WRF16_J2[[2]]

plot_WRF16_J3 = MR_plotData_J3(fit_WRF16_J3, R_list)
expect_M_WRF16_J3 = plot_WRF16_J3[[1]]
pred_M_WRF16_J3 = plot_WRF16_J3[[2]]


M_pureFe = exp((-0.4938 + sqrt(0.4938^2 - 4*0.0975*(0.7932-R_list)))/(2*0.0975))
upper_bound = data.frame(R = R_list[R_list>0.2 & R_list < 3.8], 
                         upper_bound = M_pureFe[R_list>0.2 & R_list < 3.8])

WRF16 = data.frame(R = R_list, expect_M = 1.6*R_list^1.8)



figure2 = ggplot() +
  # geom_path(data = expect_M_est_m2, aes(R, expect_M_mean),
  #           color = 'darkorange', size = 1) +
  geom_path(data = expect_M_WRF16_J3, aes(R, expect_M_mean),
            color = 'darkorange', size = 1) +
  # geom_ribbon(data = pred_M_est_m2, 
  #             aes(x = R, ymin = pred_M_Q16, ymax = pred_M_Q84),
  #             fill = 'darkorange', alpha = 0.3) + 
  geom_ribbon(data = pred_M_WRF16_J3, 
              aes(x = R, ymin = pred_M_Q16, ymax = pred_M_Q84),
              fill = 'darkorange', alpha = 0.3) + 
  geom_path(data = WRF16, aes(R, expect_M),
            color = "black", size = 1) +
  geom_ribbon(data = WRF16, 
              aes(x = R, ymin = expect_M-2.9, ymax = expect_M+2.9),
              fill = "grey", alpha = 0.6)  +
  geom_point(data=data, aes(x=R_obs, y=M_obs), 
             col = "grey10", alpha = 0.6, size = 0.5) +
  geom_errorbar(data = data,
                aes(x=R_obs, ymin=M_obs-tau_M, ymax=M_obs+tau_M),
                col = "grey20", alpha = 0.6, size = 0.2) +
  geom_errorbarh(data = data,
                 aes(y=M_obs, xmin=R_obs-tau_R, xmax=R_obs+tau_R),
                 col = "grey20", alpha = 0.6, size = 0.2) +
  geom_path(data = upper_bound, aes(x = R, y = upper_bound), color = "red", 
            size = 1, linetype = 2) +
  xlab(expression(paste("Radius (R"["Earth"],")"))) +
  ylab(expression(paste("Mass (M"["Earth"],")"))) + 
  coord_cartesian(ylim = c(-2,80), xlim = c(0.3, 6.5)) +
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=15,face="bold"),
        plot.title = element_text(hjust = 0.5),
        text=element_text(family="Helvetica", size=12, face = "bold"),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"))
