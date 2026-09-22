# Run after installing badgerstyle. Writes a visual reference in the current
# working directory. Values are illustrative, not an actual BTN data series.
library(ggplot2)
library(badgerstyle)

observations <- data.frame(
  year = 2018:2026,
  value = c(12, 14, 9, 13, 16, 18, 17, 19, 21)
)
base <- ggplot(observations, aes(year, value)) +
  badger_style() + badger_line(colour = badblue) +
  scale_x_continuous(breaks = seq(2018, 2026, 2)) +
  scale_y_continuous(limits = c(5, 30), breaks = seq(5, 30, 5)) +
  labs(x = NULL, y = "Illustrative value")

callout <- base + labs(title = "Dark callout") +
  badger_annotation(2020, 13, "Temporary\ndecline", style = "callout")
arrow <- base + labs(title = "White label with an arrow") +
  badger_annotation(2022, 25, "New high", xend = 2025.8, yend = 21.6)
below <- base + labs(title = "Callout below a point") +
  badger_annotation(2023, 15, "Recovery", style = "callout", pointer = "up")
notes <- data.frame(year = c(2018, 2026), value = c(12, 21), text = c("Start", "Latest"))
plain <- base + labs(title = "Plain text from a data frame") +
  badger_annotation(year, value, text, data = notes, nudge_y = 2, style = "text")

grDevices::png("badger-annotation-examples.png", width = 1800, height = 1100, res = 160)
gridExtra::grid.arrange(callout, arrow, below, plain, ncol = 2)
grDevices::dev.off()
