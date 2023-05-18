# Rendering
library(quarto)

quarto_render("Assignment_444135_1705.qmd", execute_params = list(
  season = 1,
  most_pop_ep = "Fire and Blood",
  desc = "The North secedes from the Seven Kingdoms and proclaims Robb as king. With Jaime as the Starks' prisoner and Robert's two brothers, Stannis and Renly, each challenging Joffrey's claim to the throne, Tywin appoints Tyrion as acting King's Hand, while Tywin fights to defend Joffrey's reign. Jon attempts to desert the Night's Watch to avenge Ned and join Robb, but his Night's Watch brothers convince him to honor his oath. Jon joins an expedition to search for Benjen Stark beyond the Wall. Yoren, a Night's Watch recruiter, smuggles Arya out of King's Landing disguised as a boy, while Joffrey intends to crown Sansa his queen, despite executing her father. Daenerys's baby is born deformed and dead, and Drogo is left in a vegetative state by the witch's treacherous magic. Daenerys compassionately ends Drogo's life. She places the three dragon eggs on Drogo's funeral pyre and sets it afire, also burning the witch alive. Ignoring Jorah's pleas, she walks into the flames. When the embers die the following morning, Daenerys is found in the ashes, unharmed, flanked by three newly-hatched baby dragons. Jorah and other witnesses kneel before her.",
  printcode = FALSE,
  data = "Data/season_1.csv"
))

# Name change
for (i in c(2:8)) {
  data = paste0("Data/season_",i,".csv")
  season_data = read.csv(data)
  most_pop_ep = season_data[season_data$viewers == max(season_data$viewers),'title']
  season = i
  quarto_render("Assignment_444135_1705.qmd", execute_params = list(
    season = season,
    most_pop_ep = most_pop_ep,
    desc = season_data[season_data$title == most_pop_ep,"description"],
    printcode = FALSE,
    data = data
  ), output_file = paste0("Report season", season, ".html"))
}
