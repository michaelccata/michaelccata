---
title: Rock and Roll *is* Noise Pollution
date: 2016-06-18 00:00:00 -04:00
tags:
- PUBLIC
- POLICY
- "+"
- MUSIC
layout: bpost
post_title: Rock and Roll **is** Noise Pollution
sub_title: AC/DC was totally wrong.
---

Rowdy neighbors, honking traffic, or loud leafblowers are accepted as part of modern life. The nuissance seems random, inconsequential, and perhaps even silly. But noise pollution is, quite literally, a textbook example of an externality. You'll find it somewhere on the "The more you know" sidebar in the market failure chapter. But! Apparently, it's a [serious problem](http://www.ncbi.nlm.nih.gov/pmc/articles/PMC4336190/) that's [only getting worse](http://ehp.niehs.nih.gov/1307272/).

<iframe width="420" height="315" align="right" style = "margin-left: 3em; margin-bottom: 3em;" src="https://www.youtube.com/embed/X_IWlPHMziU?showinfo=0&autohide=1" frameborder="0" allowfullscreen></iframe>

AC/DC hit the subject with their 1981 ballad "Rock and Roll Ain't Noise Pollution." Inspiration for the song came from Londoners unhappy with a rowdy club in a residential neighborhood.

>“Nah, that was one that Angus came up with,” he says with a dismissive wave of his hand. “We were in London at the time and there were all those problems with the old Marquee Club because it was in a built-up area and there was this whole thing about noise pollution in the news, the environmental health thing that you couldn’t have your stereo up loud after 11 at night, it all came from that.”

Begs the question: what role does rock music play in noise pollution? How often do people really complain about loud things? To answer those curiosities I turned to Kansas City and their 311 data. 

<div id="chartContainer" style="
	width:35em;
	height:23.25em; 
	display: block;
    margin: 0 auto;">
<script src="http://d3js.org/d3.v3.min.js"></script>
  <script src="{{base}}/lib/dimple.v2.2.0.min.js"></script>
  <script type="text/javascript">
    var svg = dimple.newSvg("#chartContainer", 600, 400);
    d3.tsv("{{base}}/data/kcnoise.tsv", function (data) {
      var myChart = new dimple.chart(svg, data);
      myChart.setBounds(80, 30, 480, 330)
      myChart.addMeasureAxis("x", "Hits");
      myChart.addCategoryAxis("y", ["Source", "Complaint Type"]);
      myChart.addSeries("Group", dimple.plot.bar);
      myChart.addLegend(200, 10, 380, 20, "right");
      myChart.draw();
    });
  </script>
</div>

Loud music is ***certainly*** noise pollution and rock seems to be the top hated genre. It beats country, rap, and pop as the most identified genre when people issue complaints for loud music. **But** other sources of noise pollution are also a problem. Traffic, construction, trash collection, bar and club loiterers all registered as other sources of frustration. This jives with broader, national studies on the subject.

The data for this study came from KC - a town that has birthed a range of acts from Puddle of Mudd to Tech N9ne. Complaint records spanned three years and all days/hours of the week. Rock registered **3x** the frequency of other genres, so next time you're in KCMO don't play AC/DC.


### Methods

311 data is an easy entry point for cities to join the "open data" movement. Calls are subject to most state FOIA laws and almost every municipality has paid for technology that assists call center interactions and records the data in relational format. That's when Socrata steps in. They help move your data from the call center application into their platform for the public to access. It's a nice service to share things quickly, and Socrata has done a good job creating open APIs for access.

Cities don't share call center notes in their file transfers. There's too much text, and it's relatively unstructured, so it's not really worth posting. *But* that's where all the good data is - that's where people cite specific examples and stories that allow folks like me to do analysis deeper than noise complaints vs. unkept yards. 

Kansas City was the only local government that shared case-specific URLs in their Socrata dataset. Each URL pointed towards a call-specific webpage in their case management system. On that page included the call center agent's notes which were located in the the html tag "rc_descrlong."

The workflow was simple:

1. Ping the KC 311 Socrata API for records where "Noise Control" was flagged using the fromJSON argument in R to create the data frame
2. Create a new variable in that dataframe that uses the R library rvest which scrapes according to HTML tag
3. Build dictionaries for each classifier using terms and some regular expressions
4. Use those dictionaries as new variables to do the counting, and then sum the variable columns
5. Last step, I used a Dimple.JS chart and their API for a quick visualization in D3

All the code is below. A few notes:

* The scraping took about 20 minutes for around ~1,200 scrapes. The pull was fortunately not rate limited by KC 311's CRM system. It took awhile because that's a lot of webpages to open, read, pull, and store.
* R's rvest library is stupid simple to use and deploy for webscraping
* D3 is pretty great to use if you find something that's prebuilt, it just really limits your ability to customize.

<br>

## R Code


{% highlight r %}

### Import a bunch of packages
library("curl")
library("scales")
library("sqldf")
library("ggplot2")
library("jsonlite")
library("plyr")
library("dplyr")
library("scales")
library("tidyr")
library("dplyr")
library("mgcv")
library("magrittr")
library("jsonlite")
library("rvest")
library("stringr")
library("htmltools")
library("htmlwidgets")
library("metricsgraphics")
library("RColorBrewer")
library('xml2')


### Pull Kansas City 311 records regarding noise control from Socrata
base_table <- fromJSON(
"https://data.kcmo.org/resource/cyqf-nban.json?category=Noise%20Control&$limit=2000",
simplifyVector = TRUE,
simplifyDataFrame = TRUE
)

### Function to extract case number from URL 
substrRight <- function(x, n){
  substr(x, nchar(x) - n + 1, nchar(x))
}

### Create variable for case numbers
base_table$case_no <- substrRight(base_table$case_id, 10)

### Transform timestamp into date format
base_table$create_date <- as.Date(substr(base_table$create_date,1,10), "%Y-%m-%d")

### Scraper using the URL to pull the page source HTML, find the description node (rc_descrlong),
### pull the specific text, and then apply that onto the respective call record
### from Socrata in the dataframe
base_table$case_desc <- sapply(base_table$case_url, function(x)
  read_html(x) %>%
    html_nodes("rc_descrlong") %>%
    html_text())

### Creating the count columns by search/count function str_count for various
### descriptors in the complaint. THIS DICTIONARY IS FOR MAGNITUDE OF COMPLAINT
base_table$music <- str_count(base_table$case_desc, 'music|Music|guitar| bass | band | Band ')

base_table$rock <- str_count(base_table$case_desc, ' rock | Rock | heavy metal | punk | Heavy Metal | Punk')

base_table$rap <- str_count(base_table$case_desc, ' rap | hip hop | Rap | HipHop | Hip Hop | pop | Pop')

base_table$country <- str_count(base_table$case_desc,  ' country | Country | folk | Folk s')

base_table$djs <- str_count(base_table$case_desc, ' dj |electronic|disco' )

base_table$club <- str_count(base_table$case_desc, ' club ' )

base_table$bar <- str_count(base_table$case_desc, ' bar ')

base_table$trash <- str_count(base_table$case_desc, 'trash|garbage' )

base_table$construction <- str_count(base_table$case_desc, 'construction|tools| jack hammer |machine|Machinery|Machine|machinery|Tools|Construction' )

base_table$traffic <- str_count(base_table$case_desc, ' cars | Car | Cars | car | Truck | truck | Trucks | trucks | traffic | Traffic | honk | Honk | honking | Honking | bike | bikes | Bike | Bikes |bicycle|Bicycle|motorcycle|Motorcycles' )

base_table$people <- str_count(base_table$case_desc, ' yell | Yell | shout | Shout| shouting | Shouting | yelling | Yelling | argue | Argue | arguing | Arguing ' )

### Creating the count columns by search/count function str_count for various
### descriptors in the complaint. 
### THIS DICTIONARY IS MORE CONSERVATIVE - ONLY COUNTS HITS AS 1 OR ZERO PER CATEGORY
base_table$music <- str_detect(base_table$case_desc, 'music|Music|guitar| bass | band | Band ')

base_table$rock <- str_detect(base_table$case_desc, ' rock | Rock | heavy metal | punk | Heavy Metal | Punk')

base_table$rap <- str_detect(base_table$case_desc, ' rap | hip hop | Rap | HipHop | Hip Hop | pop | Pop')

base_table$country <- str_detect(base_table$case_desc,  ' country | Country | folk | Folk s')

base_table$djs <- str_detect(base_table$case_desc, ' dj |electronic|disco' )

base_table$club <- str_detect(base_table$case_desc, ' club |nightclub' )

base_table$bar <- str_detect(base_table$case_desc, ' bar ')

base_table$trash <- str_detect(base_table$case_desc, 'trash|garbage' )

base_table$construction <- str_detect(base_table$case_desc, 'construction|tools| jack hammer |machine|Machinery|Machine|machinery|Tools|Construction' )

base_table$traffic <- str_detect(base_table$case_desc, ' cars | Car | Cars | car | Truck | truck | Trucks | trucks | traffic | Traffic | honk | Honk | honking | Honking | bike | bikes | Bike | Bikes |bicycle|Bicycle|motorcycle|Motorcycles' )

base_table$people <- str_detect(base_table$case_desc, ' yell | Yell | shout | Shout| shouting | Shouting | yelling | Yelling | argue | Argue | arguing | Arguing ' )




### Creating a summary table to aggregate the tabs
summaryframe <- data.frame(colMeans(base_table[30:40]),colSums(base_table[30:40]))
{% endhighlight %}

<br>

## Dimple.js D3 script  
(kcnoise.tsv is a three column dataset based off summaryframe from the R code)


{% highlight js %}


<div id="chartContainer" style="
	width:35em;
	height:23.25em; 
	display: block;
    margin: 0 auto;">
<script src="http://d3js.org/d3.v3.min.js"></script>
  <script src="{{base}}/lib/dimple.v2.2.0.min.js"></script>
  <script type="text/javascript">
    var svg = dimple.newSvg("#chartContainer", 600, 400);
    d3.tsv("{{base}}/data/kcnoise.tsv", function (data) {
      var myChart = new dimple.chart(svg, data);
      myChart.setBounds(80, 30, 480, 330)
      myChart.addMeasureAxis("x", "Hits");
      myChart.addCategoryAxis("y", ["Source", "Complaint Type"]);
      myChart.addSeries("Specific", dimple.plot.bar);
      myChart.addLegend(200, 10, 380, 20, "right");
      myChart.draw();
    });
  </script>
</div>

{% endhighlight %}


