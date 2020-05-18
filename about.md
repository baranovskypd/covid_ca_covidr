### <span style="font-family:Inconsolata;">About the app</span>

<span style="font-family:Lato;">This version of the app was made for the e-Rum2020 CovidR contest and is hosted on shinyapps.io. For the self-hosted version available indefinitely, please click [here](http://dataenthusiast.ca:3838/covid_ca/). There is no difference in how these versions work.</span>

<span style="font-family:Lato;">COVID-19 Canada Data Explorer was [recommended](https://www.macdonaldlaurier.ca/covid-19-canada-can-meet-global-challenge/) by the  [Macdonald-Laurier Institute](https://www.macdonaldlaurier.ca/) – one of Canada’s leading public policy think tanks - to track the progression of COVID-19 epidemic in Canada.</span>

### <span style="font-family:Inconsolata;">What some indicators mean</span>

<span style="font-family:Lato;"><u>Total cases</u> are all cases since the start of the epidemic, i.e. cumulative cases. For the number of people currently ill, see "Active cases".</span>

<span style="font-family:Lato;"><u>Cases per 100,000</u> indicator shows the overall [prevalence](https://www.britannica.com/science/prevalence) per 100,000 population since the pandemic started. For the share of people who are currently ill, see "Active cases per 100,000".</span>

<span style="font-family:Lato;"><u>Tests per 1,000</u>: Due to the lack of clarification of what the "numtested" variable stands for in the original dataset, it is not clear whether it means the number of <em>tests</em> performed, or the number of <em>people</em> tested per 1,000 population (keep in mind that one person can be tested multiple times). I would recommend a more conservative assumption - i.e. the number of tests.</span>

<span style="font-family:Lato;"><u>Case fatality rate</u> shows a percent of those who have died among the <em>diagnosed</em> cases. [Case fatality rate](https://www.britannica.com/science/case-fatality-rate) should not be confused with [mortality rate](https://en.wikipedia.org/wiki/Mortality_rate). For mortality rate, see "Mortality per 100,000".</span>

### <span style="font-family:Inconsolata;">Data</span>

<span style="font-family:Lato;">The [data](https://health-infobase.canada.ca/src/data/covidLive/covid19.csv) is downloaded from the Government of Canada [official COVID-19 page](https://www.canada.ca/en/public-health/services/diseases/2019-novel-coronavirus-infection.html).</span>

<span style="font-family:Lato;">To calculate "Cases per 100,000", “Active cases per 100,000”, "Mortality per 100,000", and "Tests done per 1,000", I used Statistics Canada population estimates for the first quarter of 2020. Source: Statistics Canada Data table  [17-10-0009](https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=1710000901).</span>

<span style="font-family:Lato;">Geospatial data used to render the map was retrieved [from Statistics Canada](http://www12.statcan.gc.ca/census-recensement/2011/geo/bound-limit/files-fichiers/2016/lpr_000b16a_e.zip). The polygons were then simplified to ensure quick rendering of the map. Due to time-consuming nature of these operations, they were performed [externally](https://gitlab.com/peterbar/covid_ca_base).</span>

### <span style="font-family:Inconsolata;">Data issues</span>

<span style="font-family:Lato;">The data prior to March 21, 2020 was truncated due to being, for most indicators, highly incomplete or missing entirely, which resulted in various errors when computing and visualizing epidemiological indicators.</span>

<span style="font-family:Lato;">The dataset contains very few negative numbers, which don't make sense in this context. These are almost certainly data entry errors on the part of the government, so I fixed this issue by converting all numbers in the dataset to their absolute values.</span>

<span style="font-family:Lato;">The map color palette may look different depending on the indicator and/or the date you have selected. This is due to some of the data being highly skewed, which causes <span style="font-family:Inconsolata;">leaflet::colorQuantile</span> to [fail](https://github.com/rstudio/leaflet/issues/94). I had to program around this issue by creating a function that switches to <span style="font-family:Inconsolata;">leaflet::colorNumeric</span> in such cases. <span style="font-family:Inconsolata;">colorNumeric</span> doesn't use quantiles to break down the data, so the resulting color scheme doesn't look as good.</span>

### <span style="font-family:Inconsolata;">About the author</span>

<span style="font-family:Lato;">My name is [Petr Baranovskiy](https://dataenthusiast.ca/?page_id=464), I am an R language enthusiast, and until recently I worked as a researcher with the University of Saskatchewan, where I specialized in economic policy analysis, economic and statistical modeling, energy policy, and the use of geospatial data for policy analysis. If you liked this app, please visit my blog at [dataenthusiast.ca](https://dataenthusiast.ca/) and follow me on [Twitter](https://twitter.com/PBaranovskiy).</span>

### <span style="font-family:Inconsolata;">Disclaimer</span>

<span style="font-family:Lato; font-size: 12px;">THE APPLICATION IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHOR OR COPYRIGHT HOLDER BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OR CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE APPLICATION OR THE USE OR OTHER DEALINGS IN THE APPLICATION.</span>