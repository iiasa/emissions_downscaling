# Downscaling method description

The downscaling method used in this project is mainly based on the convergence method of *van Vuurren et al.(2007)*.  

## Calculation of Emission Intensity

The emission intensity(EI) is calculated using emission level(E) and GDP as follow: 

$$ 
EI_{t} = E_{t}/GDP_{t} 
$$

The paper did not explicitly give the above equation but it can be inferred from the given equation: 

$$
E_{C,t} = E_{C,0}\frac{POP_{C,t}}{POP_{C,0}}\frac{GDP_{pcC,t}}{GDP_{pcP,0}}\frac{EI_{C,t}}{EI_{C,0}}
$$

Which can become: 

$$
E_{C,t} = E_{C,0}\frac{POP_{C,t}}{POP_{C,0}}\frac{GDP_{pcC,t}}{GDP_{pcP,0}}\frac{\frac{E_{C,t}}{GDP_{C,t}}}{\frac{E_{C,0}}{GDP_{C,0}}}
$$


## Calculation of Emission Intensity Growth Rate

$$
EI_{\_grC} = (\frac{EI_{R,CY}}{EI_{C,BY}})^{1/(CY-BY)}
$$

## Calculation of Preliminary Emission Intensity  

$$
EI^*_{C,t} = EI^*_{C,t-1}EI_{\_grC} 
$$

So emission level(E) can be calculated as:

$$
E^*_{C,t} = EI^*_{C,t}GDP_{C,t}
$$

## Calculation of Difference Between the Regional Emissions numbers and the Sum of Country Emissions

$$
Diff_{R,t} = EI_{R,t}GDP_{R,t} - \sum_{C}EI^*_{C,t}GDP_{C,t}
$$
Which becomes: 
$$
Diff_{R,t} = E_{R,t} - \sum_{C}E^*_{C,t}
$$

## Calculation of Difference share

$$
E_{\_shC,t} = \frac{GDP_{pcC,t}POP_{C,t}EI^*_{C,t}}{\sum_{CinR}GDP_{pcC,t}POP_{C,t}EI^*_{C,t}}
$$

Which becomes:

$$
E_{\_shC,t} = \frac{GDP_{C,t}EI^*_{C,t}}{\sum_{CinR}GDP_{C,t}EI^*_{C,t}}
$$

Which becomes:

$$
E_{\_shC,t} = \frac{E^*_{C,t}}{\sum_{CinR}E^*_{C,t}}
$$

## Calculation of final emission level 

$$
E_{C,t} = E^*_{C,t} + E^*_{C,t}Diff_{R}E_{\_shC,t}
$$