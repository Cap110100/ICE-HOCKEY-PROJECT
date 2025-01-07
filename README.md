-- About the project:

This project was inspired by my exploration of various ice hockey platforms. 
While the leaderboards on these platforms were informative, I noticed a lack of detailed player shot maps, 
on-ice tendencies, and shot conversion rates. Ice Scout Pro was created to address this gap by
presenting these insights in a creative and comprehensive manner, highlighting individual player
performance and team dynamics. Ice Scout Pro aims to develop an advanced analytical tool to
enhance the evaluation of team and player performance in Division I men's and women's ice
hockey. By leveraging detailed play-by-play and game-by-game data from the 2022, 2023, and
2024 seasons, the project provides coaches, scouts, and analysts with comprehensive insights into
player behavior and team dynamics. I have developed a Shiny web application specifically for coaches and analysts to facilitate team analysis, opposition analysis, and
scouting. The application and dashboard include team and player leaderboards, player performance
profiles, which feature player shot charts, on-ice tendencies, and shot conversion rates.

-- Data Extractiona dn Cleaning:

Data was collected through web scraping in Python and then rigorously cleaned and analyzed using
R. To analyze the data, I began by filtering out non-shot events and removing rows lacking home
team information to ensure the dataset was focused and accurate. I then selected essential columns,
including game details, coordinates, and shot types, to concentrate on the key aspects relevant to
shot analysis. To maintain consistency across different games and teams, I transformed the shot
coordinates so that all shots were directed towards a single goal. Following this, I grouped the data
by team, period, area of action, and shot type, allowing for the calculation of relevant statistics
such as shot frequencies and success rates within specific contexts. Finally, I sorted the results to
facilitate the analysis of shot patterns and tendencies, enabling the creation of detailed performance
profiles for both players and teams. These steps provided a comprehensive understanding of player
and team behavior on the ice.

-- Note:

I haven't addedd the code for extracting the data. 
