## Introduction

_Note: The following is a reproduction of a report written for the Mary Tavy hydro-electric power station on a commissioned project_

Changes in cluster membership can inform an understanding of past and current demographic influences. Moreover, these insights can help forecast how economic, political, and geographic changes will affect power consumption and demand. This report examines changes in power demand at 535 substations between January 2013, 2014, and 2015. Clusters within a corpus of substations based on power demand, and energy profiling will be used to facilitate understanding, and both intra- and inter-cluster changes over the years will be analysed. 
<p align="center">
<img width="452" alt="image" src="https://github.com/SarvSin/powerconsumption/assets/117599272/bdf3e168-b5b2-44da-98a8-241ded995b23">
</p>
<h6 align="center">Figure 1: Power consumption across clusters in January 2013</h6>

## Analysis

Hierarchical clustering is a distance-based algorithm that groups objects of interest into segments of similar characteristics. This algorithm will be utilised to create an optimal division of the substation population to aid in overall demand profiling. Firstly, the raw power consumption data is scaled for each substation to create segmentation based on demand patterns. This processed data is then input into the hierarchical clustering algorithm, which requires measure of dissimilarity between sets of objects in the population data. This is obtained by using the metric ‘Manhattan’ as a measure of distance between pairs of objects, and the linkage criterion ‘Ward’. 
<p align="center">
<img width="352" alt="image" src="https://github.com/SarvSin/powerconsumption/assets/117599272/2c1efcaf-0ca9-43e6-a447-ee3a906a8fc4">
</p>

An agglomerative hierarchical clustering is performed, and a dendrogram is plotted to aid in choosing the number of clusters. In this case, 4 clusters are obtained, and substations are labelled accordingly (See the diagram). The characteristics dataset is combined with information regarding cluster membership of each substation, and cluster-wise general characteristics are analysed. The process of cleaning, scaling, and allocating each substation to one of four clusters is performed for 2014 and 2015 datasets. Finally, yearly trends in overall power demand, and cluster membership changes over the years are explored. 

## Results 

Clustering the substations into 4 segments has facilitated understanding of common demand profiles among the 535 substations in consideration. Upon labelling substations, and aggregating quantitative information over clusters, the results were noted and have been given in table 1. 

<p align="center">
 <img width="452" alt="image" src="https://github.com/SarvSin/powerconsumption/assets/117599272/cae5427e-af7c-4940-a079-38937a1b80d5">
</p>
<h6 align="center"> Table 1: Cluster characteristics</h6>

On average, around 90% of the customer base for substations in cluster 3 is non-domestic, or industrial/commercial. This cluster will hereafter be referred to as industry-dominant cluster. 
Cluster 2 on the other hand counts approximately 90% of its customers in the domestic sphere. This cluster will be called domestic-dominant cluster. Cluster 1 encompasses substations delivering power to all types of customers – domestic, industrial, and commercial. Specifically, 70% of its customers are non-domestic. This cluster will be known as the diversified cluster. Cluster 4 consists mainly of substations delivering to domestic consumers, at around 80%. In addition, 75% of the substations within this cluster are pole mounted, i.e., delivering power to relatively isolated rural areas. Hence, this cluster will be called the rural cluster. 

In 2014, the following major trends were observed in terms of relative cluster-membership shifts – around 22% of substations in the diversified cluster had moved from the domestic-dominant cluster, almost 7% of substations in domestic-dominant cluster were previously in diversified cluster, and 6% of substations in diversified cluster had changed membership from industry-dominant cluster (See Figure 2). In absolute terms, 31 substations changed membership from domestic-dominant cluster to diversified cluster in 2014, whereas 18 substations moved from diversified cluster to domestic-dominant cluster. 7 substations also moved from industry-dominant cluster to diversified cluster in 2014. 

<p align="center">
              <img width="400" alt="image" src="https://github.com/SarvSin/powerconsumption/assets/117599272/f1bff488-101d-49f8-b311-d947e84c8b4b">
</p>
                                               <h6 align="center"> Figure 2: Changes in cluster membership from 2013 to 2014</h6>

The large movement from domestic and industry-dominant clusters to diversified cluster implies a trend towards greater diversification in power usage by customers of industry, and domestic-dominant substations. In particular, the significant shift of substations from domestic-dominant cluster to diversified cluster suggests growth of an industrial customer base in a domestic region. This could be due to development of an industrial township, with industrial parks being built around residential areas. Domestic-dominant substations in this area would increasingly need to deliver to industrial and commercial customers, hence resembling power demand patterns of substations belonging to diversified cluster. In the latter cluster, substations serve a clientele consisting 70% of industrial/commercial customers. 

<p align="center">
 <img width="452" alt="image" src="https://github.com/SarvSin/powerconsumption/assets/117599272/2b1c9c96-d3e2-4bb5-ac10-87fa89421a9e">
</p>
                                  <h6 align="center">Figure 3: Changes in cluster membership from 2014 to 2015 </h6>

In 2015, of the thirty-one substations which moved from the domestic-dominant to diversified cluster, all but one returned to domestic-dominant cluster. This fluctuating behaviour implies a transition period for substations in a particular region. Here patterns of power demand are gradually, but steadily changing to accommodate a transforming customer base. Since such changes do not occur over-night, or in this case within a year, it is possible that patterns of power demand at these substations resemble characteristics of both diversified and domestic clusters, i.e., these substations are approximately equidistant from centres of diversified and domestic-dominant clusters. 

A tectonic shift in cluster memberships occurs in 2015, as almost 25% of substations in domestic and industry-dominant clusters are now composed of substations that were previously in the diversified cluster (See Figure 3). In absolute terms, 82 substations from the diversified cluster moved to domestic-dominant cluster in 2015, of which 30 were reverting. Additionally, 20 substations moved from the diversified to industry-dominant cluster. These changes imply greater specialisation of substations in the diversified cluster. The movement of 20 substations from diversified into industry-dominant cluster suggests a strong trend towards patterns of demand resembling those of industrial customers, which supports the hypothesis above of industrialisation of a region. In addition, the movement of 52 substations from diversified to domestic-dominant cluster implies delivery of power to a denser domestic customer base. This points to a rapid expansion in industrial activity being accompanied by growth in population of residents in the region. 

<p align="center">
 <img width="452" alt="image" src="https://github.com/SarvSin/powerconsumption/assets/117599272/3577a788-0479-41a3-9382-4378e56d4664">
</p>
                                        <h6 align="center"> Figure 4: Power demand across clusters in January 2014 </h6>

Indeed, these changes in economic development have triggered significant changes in power demand for substations in the diversified cluster over 2013-2015. With patterns of demand changing gradually, substations in the diversified cluster exchanged membership with domestic and industry-dominant clusters over 2013 and 2014, until 2015 when a large number finally shifted to said clusters. A trend of economic transformation of towns with medium-scale industry and domestic settlement, into large townships with concentrated residential and large-scale industrial sectors seems to have contributed to a fall in power demand for remaining substations in the diversified cluster. These substations can be supposed to deliver power to regions with small-scale industry – the pattern of power consumption is the same, but the magnitude of demand is lower. 

Hence, the demand for the diversified cluster has dropped from being stable around 130kV during weekdays, to 111kV in 2015 (See Figures 1, 4 and 5). The demand for substations in domestic-dominant cluster has remained stable at around 110kV on weekends. Similarly, size of power demand in industry-dominant cluster has remained stable within a range between 90kV and 110 kV on weekdays. There also exists a weekly pattern of rising demand over the month of January across all clusters and years. This can either be interpreted as the influence of increasing economic activity as festive season passes, which usually lasts from mid/late December into the first week of January; or, if the substations are in a region where temperatures are minimum in 2nd to 4th weeks of January, the effect of rising power consumption to keep commercial, residential places warm through electrical heating appliances.

<p align="center">
                <img width="380" alt="image" src="https://github.com/SarvSin/powerconsumption/assets/117599272/29862e29-de19-4f93-ac50-a77e86e38153">
</p>
                                                <h6 align="center"> Figure 5: Power demand across clusters in January 2015 </h6>


## Conclusions

In this report, it was shown how heavy development in a previously mid-sized industrial town can affect power demand patterns over the years. The onset of such changes is slow to realise, which causes substations to exhibit vacillating behaviour as they switch clusters, and a significant overlap of segments emerges. It can be forecast that increasing industrial development across a wider region will prompt a period of fluctuating behaviour in the diversified cluster, followed by greater specialisation of substations within this cluster into domestic or industrial purposes. Identifying key behavioural trends can help interpret changes in the future. 

