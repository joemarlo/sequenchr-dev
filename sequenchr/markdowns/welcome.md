# Learning causal inference

Causal inference is a topic that lends itself well to visualizing many of the core concepts. The goal of this webtool is to help build your intuition for key causal inference concepts via visualization. The tool attempts to follow a 'first principles' path by first describing the fundamental problem of causal inference and why randomization is often necessary. From there, visualizations of treatment effects, propensity scores, and regression discontinuity are included. All rely heavily on interaction, simulation, and visualization. The tool's components are independent of each other but they generally progress in complexity from left-to-right on the navigation bar.

This is not a comprehensive review. For more information on learning causal inference, try the following resources:
- [Regression and Other Stories](https://avehtari.github.io/ROS-Examples/)
- [Data Analysis Using Regression and Multilevel/Hierarchical Modesl](http://www.stat.columbia.edu/~gelman/arm/)
- [Nick Huntington-Klein's blog post illustrating causal inference](http://nickchk.com/causalgraphs.html)

The webtool is built for a desktop or laptop experience and works best on Safari or Chrome.

### Why is causal inference important?

"Correlation != causation" is a common refrain in statistics. We answer many research questions using advanced correlation measures but we *really* want to understand is the underlying causal mechanism. Does the vaccine reduce infection rates? Do higher taxes reduce inequality? Does an ad increase the number of customers? Update the plot on the right to see highly correlated but fraughtful relationships from real-life data.

***

The source code for this tool can be found on [Github](https://github.com/joemarlo/causal-tool).

<details><summary>Software thanks</summary>
<br>
The tool is made possible by the following software:
- **R**: R Core Team (2020). R: A language and environment for statistical computing. R Foundation for Statistical Computing, Vienna, Austria. URL https://www.R-project.org/.
- **Shiny**:  Winston Chang, Joe Cheng, JJ Allaire, Yihui Xie and Jonathan McPherson (2020). shiny: Web Application Framework for R. R package version 1.5.0. https://CRAN.R-project.org/package=shiny
- **tidyverse**: Wickham et al., (2019). Welcome to the tidyverse. Journal of Open Source Software, 4(43), 1686, https://doi.org/10.21105/joss.01686
- **arm**: Andrew Gelman and Yu-Sung Su (2018). arm: Data Analysis Using   Regression and Multilevel/Hierarchical Models. R package version 1.10-1. https://CRAN.R-project.org/package=arm
- **viridis**: Simon Garnier (2018). viridis: Default Color Maps from 'matplotlib'. R package version 0.5.1. https://CRAN.R-project.org/package=viridis
- **shinyjs**: Dean Attali (2020). shinyjs: Easily Improve the User Experience of Your Shiny Apps in Seconds. R package version 1.1. https://CRAN.R-project.org/package=shinyjs
- **shinyWidgets**: Victor Perrier, Fanny Meyer and David Granjon (2020). shinyWidgets: Custom Inputs Widgets for Shiny. R package version 0.5.3. https://CRAN.R-project.org/package=shinyWidgets
- **kableExtra**: Hao Zhu (2019). kableExtra: Construct Complex Table with 'kable' and Pipe Syntax. R package version 1.1.0. https://CRAN.R-project.org/package=kableExtra
- **pageviews**: Os Keyes and Jeremiah Lewis (2020). pageviews: An API Client for Wikimedia Traffic Data. R package version 0.5.0. https://CRAN.R-project.org/package=pageviews
- **Archive Team**: http://textfiles.com/underconstruction/. An ode to the early web

- **TraMineR**
For state sequences:

  Gabadinho, A., Ritschard, G., Müller, N. S., & Studer, M. (2011). Analyzing and Visualizing State
  Sequences in R with TraMineR. Journal of Statistical Software, 40(4), 1-37. DOI
  http://dx.doi.org/10.18637/jss.v040.i04.

For dissimilarity measures:

  Studer, M. & Ritschard, G. (2016). What matters in differences between life trajectories: A comparative
  review of sequence dissimilarity measures, Journal of the Royal Statistical Society, Series A, 179(2),
  481-511. DOI http://dx.doi.org/10.1111/rssa.12125

For discrepancy analysis:

  Studer, M., Ritschard, G., Gabadinho, A. & Müller, N.S. (2011), Discrepancy analysis of state sequences,
  Sociological Methods and Research, 40(3), 471-510. DOI http://dx.doi.org/10.1177/0049124111415372

  Studer, M., Ritschard, G., Gabadinho, A. & Müller, N.S. (2010), Discrepancy analysis of complex objects
  using dissimilarities, In Guillet, F., Ritschard, G., Zighed, D.A. & Briand, H. (eds) Advances in
  Knowledge Discovery and Management. Series: Studies in Computational Intelligence. Volume 292, pp. 3-19.
  Berlin: Springer.

For representative sequences:

  Gabadinho, A. & Ritschard, G. (2013), Searching for typical life trajectories applied to child birth
  histories, In R. Lévy & E. Widmer (eds.), Gendered life courses.  pp. 287-312. Vienna: LIT

For longitudinal characteristics and the complexity index:

  Gabadinho, A., Ritschard, G., Studer, M. & Müller, N.S. (2010), Indice de complexité pour le tri et la
  comparaison de séquences catégorielles, In Extraction et gestion des connaissances (EGC 2010), Revue des
  nouvelles technologies de l'information RNTI. Vol. E-19, pp. 61-66.

For the precarity index:

  Ritschard, G., Bussi, M. & O'Reilly, J. (2018), An Index of Precarity for Measuring Early Employment
  Insecurity, In Ritschard, G. & Studer, M. (eds) Sequence Analysis and Related Approaches: Innovative
  Methods and Applications. Series: Life Course Research and Social Policies. Vol. 10, pp. 279-295. Cham:
  Springer. DOI http://dx.doi.org/10.1007/978-3-319-95420-2_16

For event sequences:

  Ritschard, G., Bürgin, R. & Studer, M. (2013), Exploratory Mining of Life Event Histories, In J.J. McArdle
  & G. Ritschard (eds.), Contemporary Issues in Exploratory Data Mining in the Behavioral Sciences.  pp.
  221-253. New York: Routledge

  Bürgin, R. & Ritschard, G. (2014), A decorated parallel coordinate plot for categorical longitudinal data,
  The American Statistician 68(2), 98-103.  DOI http://dx.doi.org/10.1080/00031305.2014.887591

</details><br>
