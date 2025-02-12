# Project 4: Who Is Who -- Entity Resolution

### [Project Description](doc/project4_desc.md)

Term: Spring 2017

+ Team 11
+ Projec title: Comparison of Algorithms on Author Disambiguation
+ Team members
	+ Ziwei Meng
	+ Yuxin Zhu 
	+ Bo Peng
	+ Song Wang
	+ Shuyi Tang
+ Project summary: In this project, we studied and implemented algorithms in Paper1 and Paper5. Paper 1 suggested a single-link agglomerative clusering algorithm according to the number of overlappling coauthor names. Paper 5 discussed about an error-driven algorithm based on hierarchical clustering  with scoring function and more features including number of overlapping bigram and trigram, cosine similarity and edit distance. Moreover, we compared performance of two algorithms on accuracy and f1. We found that the algorithm in Paper 5 performed better than the one in Paper1 when dealing with relatively large dataset. For instance, algorithm in Paper5 offered much better results than Paper1 when tackling datasets of which author names are 'CChen', 'JLee', 'JSmith' and 'SLee' with the number of observations 801, 1419, 927 and 1464 respectively. 

  <br/>Our comparing result is shown as below.

![alt tag](https://github.com/TZstatsADS/Spr2017-proj4-team-11/blob/master/figs/result%20form.png)

+ We usd Python to realize the algorithm and in order to run TrainingAndPredict.ipynb successfully please install the packages 'numpy','pandas', 'scipy', 'matplotlib', 'cvxopt' and 'seaborn' in Python first.

+ Since the feature files are too large to upload into GitHub, the feature files for datasets 'JLee', 'SLee' and 'YChen' are not available in our GitHub. Please download feature files for these datasets through links below.

  <br/>JLee:   https://s3.amazonaws.com/ads-proj4-features/feature5.csv
  <br/>SLee:   https://s3.amazonaws.com/ads-proj4-features/feature13.csv
  <br/>YChen:  https://s3.amazonaws.com/ads-proj4-features/feature14.csv
  
  
**Contribution statement**: ([default](doc/a_note_on_contributions.md)) All team members contributed equally in all stages of this project. All team members approve our work presented in this GitHub repository including this contributions statement. 

Following [suggestions](http://nicercode.github.io/blog/2013-04-05-projects/) by [RICH FITZJOHN](http://nicercode.github.io/about/#Team) (@richfitz). This folder is orgarnized as follows.

```
proj/
├── lib/
├── data/
├── doc/
├── figs/
└── output/
```

Please see each subfolder for a README file.
