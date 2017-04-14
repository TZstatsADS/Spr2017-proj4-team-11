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
+ Project summary: In this project, we studied and implemented algorithms in Paper1 and Paper5. Paper 1 suggested a single-link agglomerative clusering algorithm according to the number of overlappling coauthor names. Paper 5 discussed about an error-driven algorithm with scoring function and more features including number of overlapping bigram and trigram, cosine similarity and edit distance. Moreover, we compared performance of two algorithms on accuracy and f1. We found that the algorithm in Paper 5 can produce much better accuracy and f1 than the one in Paper1 when dealing with relatively large dataset. For instance, algorithm in Paper5 offered much better results than Paper1 when tackling dataset in which author names are 'JLee', 'JSmith', 'SLee' and 'YChen' with the number of observations 1419, 927, 1464 and 1265 respectively.


+ We usd Python to realize the algorithm and in order to run Algorithms.py successfully please install the packages 'numpy','pandas' and 'scipy' in Python first.
	
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
