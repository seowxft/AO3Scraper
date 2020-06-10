## Statistics of Nirvana in Fire fanworks on AO3

Data scraped on 8 June 2020 with [radiolarian's AO3Scraper](https://github.com/radiolarian/AO3Scraper).

Read the [stats report here](https://seowxft.github.io/other/nirvana-in-fire-stats).

- Hardcode <url> in `ao3_work_ids.py` and run `python ao3_work_ids.py` to get csv file of AO3 fanwork ids.
- Output csv is then used to scrape fanwork data with `python ao3_get_fanfics.py <csv>`.

`nif_metadata.csv` contains all metadata from the above scrape, excluded for fanwork body data.
- Load `nif_metadata.csv` to run `nifStatsAnalysis.R` for statistics analysis.

<p align="center">
  <img src="https://github.com/seowxft/NiFAO3Scrape/blob/master/Figures/Rplot01.png" alt="WordCloud"/>
</p>


## Word cloud generator
txt files of fanwork body data are not included in the current dataset as it is basically fic. You may scrape them on your own from the source:
- Output csv from `ao3_get_fanfics.py` to run `python csv_to_txts.py <csv>`.
- Load txt files to run `nifWordCloud.R` to plot word cloud.

Top 300 most common words in Nirvana in Fire fanfic:
<p align="center">
  <img src="https://github.com/seowxft/NiFAO3Scrape/blob/master/Figures/Rplot15.jpg" alt="WordCloud"/>
</p>

## License
[radiolarian's AO3Scraper](https://github.com/radiolarian/AO3Scraper) is licensed under the Creative Commons Attribution-Non Commercial 4.0 International (CC BY-NC 4.0). Feel free to use the other analysis scripts and adapt it however you want, but never for commerical use.
