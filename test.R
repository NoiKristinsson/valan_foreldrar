library(XML)

vidveran <- readHTMLTable("vidvera.xls", encoding = "UTF-8")
vidveran <- as.data.frame(vidveran[4])
