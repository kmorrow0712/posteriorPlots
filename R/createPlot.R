
# roxygen skeleton --------------------------------------------------------
#' Create posterior plot
#'
#' Generate a posterior ridgeline plot from wideframe text .txt or .csv
#' (cols = ROIs, rows = posteriors).
#'
#' ALL PARAMS SHOULD ENTERED IN QUOTES BECAUSE THEY ARE CHARACTERS!
#' Example: createPlot("path/to/file.txt", "Plot title", "X > Y", "Y > X")
#' If you do not want X-axis labels, enter NULL for both c and d values.
#'
#' For now this package only works with files that has no separator ""
#' It is best if you have pretty ROI names ahead of time, this package will print whatever the column values are from the input dataset.
#'
#'
#'
#' @param a wide dataframe of posteriors
#' @param b title of plot
#' @param c right-most (negative) x-axis label
#' @param d left-most (positive) x-axis label
#'
#' @importFrom dplyr mutate %>%
#' @importFrom ggplot2 ggsave ggplot aes geom_vline scale_y_continuous scale_x_continuous scale_fill_gradientn guides guide_colorbar sec_axis labs theme element_line element_text element_rect element_blank unit labs
#' @importFrom ggridges geom_density_ridges
#' @importFrom stats quantile reorder
#' @importFrom utils write.csv
#'
#'
#'
#' @return df exports long dataframe with P+ values
#' @return df exports P summary table
#' @return high resolution posterior plot (dpi = 800, height = 7, width = 9)
#'
#' @export createPlot




createPlot <- function(a,b,c,d) {
   # import dataset
  df <- utils::read.table(a, sep = "", header = T)
      print(paste("Dataset",a, "imported!"))
      print(paste("Your plot title will be: ", b))
      print(paste("Your left x-axis label will be: ", c))
      print(paste("Your right x-axis label will be: ", d))

   # gather ROI names and iterations for P+ calculations
  rois <- colnames(df)
  iterations <- length(df[,1])

  # transpose to long form
  print("Creating long dataset with P+ values...")
   df.long <- tidyr::tibble()
   df.long <- df %>%
     tidyr::gather(ROI) %>%
     mutate(index = rep(1:length(rois), each = iterations)) %>%         # create index for plotting
     dplyr::group_by(ROI) %>%
     mutate(mean = mean(value)) %>%                                     # calculate mean (not really needed right now but oh well)
     mutate(p = ((sum(value > 0)/iterations))) %>%                      # calculate P+ value for each ROI
     mutate(p.plot = p) %>%
     mutate(p.plot = replace(p, p.plot > 0.15 & p.plot < 0.85, NA))     # create plotting variable to fill ridges with NA/gray when out of range

   # create a quick summary table for plot labels
   print("Creating summary dataset...")
   P <- df.long %>%
     dplyr::select(ROI, index, mean, p) %>%
     unique() %>%
   dplyr::arrange(p)                                                    # sort by P+ values for plotting


   # get quantiles for approximate locations of x-axis labels
   print("Calculating quantiles for x-axis labels...")
   quantiles <- quantile(df.long$value)
   neg.lab.x <- min((df.long$value)/2)
   pos.lab.x <- max(df.long$value)/2
   lowerbound.x <- quantiles[[1]]
   upperbound.x <- quantiles[[5]]

   x.positions <- c(neg.lab.x, 0, pos.lab.x)

   # export dataframes for reference
   write.csv(P, "~/P.csv")
   write.csv(df.long, "~/df_long.csv")


   # plot
   print("Plotting...")
   plot <-
     ggplot2::ggplot(df.long,
                     aes(x = value,
                         y = as.numeric(reorder(index,p)),     # trick the system! Make ggplot think y values are numerics
                         group = ROI,
                         fill = p.plot)) +

     geom_density_ridges(quantile_lines = TRUE,               # quantiles
                         quantiles = 2,
                         scale = 1.75,
                         rel_min_height = .01,
                         color = "#404040",
                         size = .85) +
     geom_vline(xintercept = 0, alpha = .85, color = "black", size = 1) +

     scale_y_continuous(breaks = 1:length(P$ROI),
                        expand = c(0,0.1),
                        labels = P$ROI,
                        sec.axis = sec_axis(~.,
                                            breaks = 1:length(P$ROI),
                                            labels = format(round(P$p, 3),nsmall = 2))) +

     scale_x_continuous(breaks = x.positions, labels = round(x.positions,2)) +

     scale_fill_gradientn(limits = c(0,1),
                          colors = c("blue","cyan",
                                     "gray","yellow","red"),
                          values = c(0,0.15,
                                     0.150000001, 0.85,  #looks funny but best way so far to get gradient the way we want it
                                     0.850000001, 1.0),
                          breaks = c(0, 0.15, 0.85, 1)) +

     guides(fill = guide_colorbar(barwidth = 1.5,        # legend qualities
                                  barheight = 8,
                                  nbin = 50,
                                  frame.colour = "black",
                                  frame.linewidth = 1.5,
                                  ticks.colour = "black")) +

      # aesthetic junk, can be messed around with if needed.
     theme(
       plot.background = element_blank(),
       plot.margin = unit(c(0,0,2,0),"cm"),
       panel.background = element_blank(),
       panel.grid.major.y = element_line(color = "grey"),
       panel.grid.major.x = element_line(linetype = "dotted"),
       plot.title = element_text(size = 20, margin = unit(c(0,0.1,.1,02),"cm"), face = "plain", hjust = 0.5),
       legend.title = element_text(size = 12),
       legend.text = element_text(size = 10, angle = 0),
       legend.position = c(.91,.20),
       legend.background = element_blank(),
       legend.box.background = element_rect(colour = "black", size = .75),
       axis.title = element_text(size = 16),
       axis.text.y = element_text(size= 12, color = "black", margin = unit(c(0,-0.05,0,0.05),"cm"), angle = 0, vjust = 0),
       axis.text.y.right = element_text(size = 12, color = "black",margin = unit(c(0,0,0,-0.05),"cm"), angle = 0),
       axis.text.x = element_text(size = 12, color = "black", margin = unit(c(0.04,0,0,0),"cm")),
       axis.ticks.y = element_blank()) +

     # labels
     labs(
       x = NULL,
       y = NULL,
       title = b,             # takes on title label specified by user
       fill = "P+ ")

plot <- cowplot::ggdraw(plot) +
  cowplot::draw_text("P+", x = .97, y = .96, size = 16) +
  cowplot::draw_text(c, x = .55, y = .08, size = 16) +
  cowplot::draw_text(d, x = .75, y = .08, size = 16)

print("Positions of X labels are (.55, .08) and (.75, .08)")

ggsave(paste("~/",b,".png", sep = ""), height = 7, width = 9, dpi = 800)


print(paste("Finished! Plot saved as ",b,".png"))
}






