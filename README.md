# MB2_NTL_BUG
First R project for IPG

Satellite: Suomi NPP
Sensor: VIIRS
Product: VCMSLCFG (Cloud Masked, Stray Light Corrected)
Timeframe: Monthly 2014-201904
Tile: 2

RQ: To investigate changes in nightlight radiance pattern clustering and texture in Bulgaria

#Calculated (global) stats and delta to previous month:
1. Mean
2. St.dev
3. Moran I

#Calculated (local) GLCMs:
1. Homogeneity
2. Correlation

#Assumptions: Bulgaria experienced sustained economic growth since 2014 but a continuous shrinking population
  Therefore, randomness (urban sprawl) should decrease, while radiance intensity would increase
  Globally: Moran's I; st.dev (randomness shall decrease), while mean radiation shall increase

*NOT SURE GLCM IS APPRIORIATE FOR NTL, probably not
  
  Texturally: GLCM_homogeneity shall decrease, while GLCM collinearity shall increase
INDEPENDENT = Moran's I ; st.dev | GLCM Correlation
DEPENDENT = mean_rad | GLCM Homogeneity

H0: NTL of Bulgaria does not show significant pattern change between 2014 to 2019
H1: NTL of Bulgaria follows the assumption of decreasing disorder, but brightening
