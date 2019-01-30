# Dashboards

This document briefly describes the various dashboards available.

## Overview

The overview dashboard is meant to quickly get an idea of what coverage has been received. It can be useful to spot anomalies to explore in other tabs of auritus.

The filters at the top allow you to change the what data is visualised below:

- The date range to use.
- The types of coverage sources, news, blogs or discussions to take into account.

Top level numbers are displayed below the filters.

- Articles: the number of articles.
- Media Outlets: Number of media outlets.
- Facebook Shares: The number of times the articles have been shared, liked, or commented on Facebook.
- Sentiment: Average sentiment score of the articles.
- Top Language: Most prominent language.
- Top Outlet: Media Outlet that gives the most coverage.
- Articles Length: Average number of words per article.
- Sophistication: Lexical diversity, a score indicating the sophistication of the language used in the coverage.

Note that the sentiment line graph is a three day rolling average. The word clouds below display outlets providing coverage and locations mentioned in the articles selected. Cliking on a term in either word cloud will display the articles in which the term has been found.

## Segments

On the segment's page one can drill down into the segments defined in `_auritus.yml`. At the top is the trend, the number of articles where each segment is found. Note that those are not mutually exclusive as an article may contain multiple segment. To the right of that a radar chart displays the total number of articles containing each segment.

Below that, media outlets and the segment they mention as well as the narrative, an experimental metric which attemtps at measuring which segment drives the narrative in the coverage. Since one article can contain multiple segment it is good to know which narrative is of primary interest to the article. It measures the latter by looking at which segment is contained in the first paragraph of the article. A segment being contained in the first paragraph is indicative that it is the primary topic of the said article.

## Networks

The networks are still at an experimental stage. They attempt to bring more depth in exploring the content of the articles by letting the user explore connections between entities and media outlets. For instance, one can see which personas are co-mentioned in articles, or which media outlet discusses which places.
