## Statistics for Nirvana in Fire fanworks on AO3

Data scraped on 8 June 2020 with [radiolarian's AO3Scraper](https://github.com/radiolarian/AO3Scraper) and [alexwlchan's ao3 API](https://github.com/alexwlchan/ao3).

Read the full [stats report here](https://seowxft.github.io/other/nirvana-in-fire-stats).

- Run `python ao3_work_ids.py` to get csv file of NiF AO3 English fanwork ids.
- Output csv is then used to scrape fanwork data with `python ao3_get_fanfics.py <csv>`.
- `nif_metadata.csv` contains all metadata from the above scrape, excluded for fanwork body data.

For author data (which is not included in [radiolarian's AO3Scraper](https://github.com/radiolarian/AO3Scraper)), [alexwlchan's ao3 API](https://github.com/alexwlchan/ao3) was used.
- Run `python ao3_get_author.py` with ids from `nif_metadata.csv` to get `nif_metaData_author.csv`

Load `nif_metaData.csv` and `nif_metaData_author.csv` in `nifStatsAnalysis.R` for statistics analysis.

<p align="center">
  <img src="https://github.com/seowxft/NiFAO3Scrape/blob/master/Figures/Rplot1.png" alt="WordCloud"/>
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
[radiolarian's AO3Scraper](https://github.com/radiolarian/AO3Scraper) is licensed under the Creative Commons Attribution-Non Commercial 4.0 International (CC BY-NC 4.0).[alexwlchan's ao3 API](https://github.com/alexwlchan/ao3) is licensed under the MIT license. Feel free to use the other scripts and adapt it however you want, but never for commerical use.
