### Animation
results
for(i in 1:n.iteration){
  
  # creating a name for each plot file with leading zeros
  if (i < 10) {name = paste('000',i,'plot.png',sep='')}
  if (i < 100 && i >= 10) {name = paste('00',i,'plot.png', sep='')}
  if (i >= 100) {name = paste('0', i,'plot.png', sep='')}
  
  #saves the plot as a .png file in the working directory
  png(name)
  ggplot(results[i,], aes(x = tadro.x, y = tadro.y)) +
    geom_point() +
    geom_segment(aes(xend = head.x, yend = head.y), 
                 arrow = arrow(length=unit(0.1, "cm"), ends = "last", type = "closed"), color = "green") +
    geom_path(data = pool, aes(x = xx, y = yy)) +
    geom_segment(data = LShed.mark, aes(x = x1, y = y1, xend = x2, yend = y2)) +
    geom_segment(data = RShed.mark, aes(x = x1, y = y1, xend = x2, yend = y2)) +
    geom_path(data=light, aes(x = xx, y = yy), color = "orange") +
    labs(title = paste("Network ID:", Id, sep = " "), 
         subtitle = paste("iteration =", i, sep = "")) +
    theme_void()
  dev.off()
}
$ convert *.png -delay 5 -loop 0 binom.gif

