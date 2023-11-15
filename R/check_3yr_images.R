# check_3yr_images.R
rm(list=ls())
library(stringr)
library(raster)
library(dplyr)

dhs_df <- read.csv("./data/interim/dhs_est_iwi.csv")  %>% 
  dplyr::select(dhs_id, lat, lon, country, image_file, image_file_annual, image_file_5k_3yr)  
  #filter to row with known negative value for test
  #filter(round(lat,2)==-34.19 & round(lon,2)==22.11)

# Error in (function (classes, fdef, mtable)  : 
#             unable to find an inherited method for function ‘select’ for signature ‘"data.frame"’
          


#variables representing tif data structure and which layers have which years
year_groups <- c("2002-2004","2005-2007","2008-2010","2011-2013")
original_r <- c(35,43,51,59)
original_g <- c(34,42,50,58)
original_b <- c(33,41,49,57)
five_k_3yr_r  <- c(9,15,21,27)
five_k_3yr_g  <- c(8,14,20,26)
five_k_3yr_b  <- c(7,13,19,25)
#for annual rows, just look at the first year of the group
annual_r <- c(15,33,51,69)
annual_g <- c(14,32,50,68)
annual_b <- c(13,31,49,67)

################################################################################
# Loop through dhs points, plotting images for review
################################################################################
# Set up the layout for three side-by-side plots
par(mfrow = c(1, 3))
for (i in 50:nrow(dhs_df)) {
  #uncomment to test
  #i=50
  # Read the images
  i_original <- raster::brick(dhs_df$image_file[i])
  i_annual   <- raster::brick(dhs_df$image_file_annual[i])
  i_5k_3yr   <- raster::brick(dhs_df$image_file_5k_3yr[i])   
  
  #scale the images, using collection 1 scaling
  i_original_scaled <- (i_original/.0001)
  i_annual_scaled <- (i_annual/.0001)
  i_5k_3yr_scaled <- (i_5k_3yr/.0001)
  
  #Loop through years
  for (j in 1:length(year_groups)) {
    #uncomment to test
    #j=1
        # Plot the three images side by side
    plotRGB(i_original_scaled, r = original_r[j], original_g[j], original_b[j], 
            main = paste("Original",year_groups[j], dhs_df$dhs_id[i]),
            stretch="hist")
    plotRGB(i_annual_scaled, r = annual_r[j], annual_g[j], annual_b[j], 
            main = paste("Annual",year_groups[j],dhs_df$dhs_id[i]),
            stretch="hist")
    plotRGB(i_5k_3yr_scaled, five_k_3yr_r[j], five_k_3yr_g[j], five_k_3yr_b[j],
            main = paste("5k 3yr",year_groups[j],dhs_df$dhs_id[i]),
            stretch="hist")
    #row that fails:  DHS id 6103
    # class      : RasterBrick 
    # dimensions : 167, 167, 27889, 36  (nrow, ncol, ncell, nlayers)
    # resolution : 0.0002688717, 0.0002699141  (x, y)
    # extent     : 9.75937, 9.804272, 4.048103, 4.093178  (xmin, xmax, ymin, ymax)
    # crs        : +proj=longlat +datum=WGS84 +no_defs 
    # source     : memory
    # names      : X00151_1, X00151_2, X00151_3, X00151_4, X00151_5, X00151_6, X00151_7, X00151_8, X00151_9, X00151_10, X00151_11, X00151_12, X00151_13, X00151_14, X00151_15, ... 
    # min values :      0.0,      0.0,      0.0,      0.0,      0.0,      0.0,      0.0,      0.0,      0.0,       0.0,       0.0,       0.0,       0.0,       0.0,       0.0, ... 
    # max values :   2162.0,   2679.0,   3125.0,   4721.5,  11871.0,  11680.5,   3011.0,   3293.0,   3485.0,    5298.0,   20000.0,   20000.0,    2473.0,    2807.0,    2986.0, ...
    
    #cellStats(i_5k_3yr, stat="min")  
    # [1]  0.00000  0.00000  0.00000  0.00000  0.00000  0.00000  0.00000  0.00000  0.00000
    # [10]  0.00000  0.00000  0.00000  0.00000  0.00000  0.00000  0.00000  0.00000  0.00000
    # [19]  0.00000  0.00000  0.00000  0.00000  0.00000  0.00000 -0.02785  0.00000  0.00000
    # [28]  0.00000  0.00000  0.00000  0.03210  0.07310  0.05490  0.21180  0.14600  0.07570
    # 
    
    #cellStats(i_5k_3yr_scaled, stat="min")
    # [1]    0.0    0.0    0.0    0.0    0.0    0.0    0.0    0.0    0.0    0.0    0.0
    # [12]    0.0    0.0    0.0    0.0    0.0    0.0    0.0    0.0    0.0    0.0    0.0
    # [23]    0.0    0.0 -278.5    0.0    0.0    0.0    0.0    0.0  321.0  731.0  549.0
    # [34] 2118.0 1460.0  757.0    
    
    #look at values 
    #getValues(i_5k_3yr_scaled)
    # X00050_1 X00050_2 X00050_3 X00050_4 X00050_5 X00050_6  X00050_7  X00050_8  X00050_9 X00050_10 X00050_11
    # [1,]    921.0   1170.5   1221.0   2217.0   1904.0   1527.5  771.0000 1034.0000  979.0000 2160.5000 1733.0000
    # [2,]    813.0   1033.0   1132.0   1895.0   1732.0   1310.0  687.0000  905.0000  889.0000 1811.0000 1492.5000
    # [3,]   1088.0   1395.0   1459.0   2091.0   1871.0   1462.0  941.0000 1243.2500 1289.9999 2001.0000 1724.0000
    # [4,]   1372.0   1725.0   1839.0   2255.0   1987.0   1585.0 1187.5000 1554.9999 1582.0000 2141.0000 1806.0000
    # [5,]   1185.0   1547.0   1715.0   2489.0   2481.0   1928.0  956.0000 1250.4999 1269.0000 2398.5000 2013.0000
    # [6,]    943.0   1241.0   1352.0   2382.0   2297.0   1711.0  823.0000 1018.5000 1070.0000 2377.0000 2154.0000
    # [7,]    716.0   1001.0   1138.0   2303.0   2175.0   1605.0  631.2500  939.0000  978.5000 2311.5000 2023.0000
    # [8,]    889.0   1139.0   1292.0   2294.0   2194.0   1842.0  747.0000  987.0000 1093.0000 2087.0000 2033.0000
    # [9,]    919.0   1224.0   1440.0   2449.0   2304.0   1935.0  842.0000 1118.0000 1145.0000 1985.9999 1874.0000
    # [10,]    922.0   1258.0   1511.0   2496.0   2653.0   2097.0  982.5000 1318.5000 1451.0000 2492.7500 2218.4999
    # [11,]    833.0   1143.0   1392.0   2429.0   2307.0   1788.0  842.5000 1174.1666 1328.7500 2171.5000 1892.5001
    # [12,]    816.0   1110.0   1332.0   2390.0   2149.0   1695.0  758.5000  963.3333 1101.0000 1962.5001 1585.7500
    # [13,]    746.0   1032.0   1151.0   2190.0   1947.0   1468.0  734.5000  955.5000 1000.0000 2015.5001 1683.5000
    # [14,]    682.5    947.5   1031.5   2044.0   1809.0   1341.0  646.5000  872.0000  951.0000 1935.5001 1601.5001
    # [15,]    733.5   1030.0   1195.5   2101.5   2152.5   1711.0  711.0000  948.1667 1108.0000 1952.8334 1994.5000
    # [16,]    683.0    927.0   1036.0   2018.0   1982.0   1472.0  615.0000  874.0000  972.5000 1985.9999 1843.0001
    # [17,]    585.5    802.5    830.0   2090.0   1574.5   1128.0  575.0000  781.0000  797.3333 1976.6666 1497.0000
    # [18,]    744.5    923.5    959.0   2199.0   1773.5   1280.0  696.0000  885.0000  900.0000 2036.0000 1648.0000
    # [19,]    757.5    966.0    977.5   2040.5   1759.5   1307.5  724.5000  876.0000  909.0000 1901.6001 1703.9999
    # [20,]    683.0    917.0    953.5   1952.0   1728.5   1318.5  682.5000  809.0000  818.0000 1726.0000 1585.0000
    # [21,]    586.0    810.0    876.0   1997.0   1649.0   1151.0  601.0000  753.0000  790.0000 1733.0000 1426.0000
    # [22,]    633.0    883.0   1001.0   2092.0   1851.0   1344.0  623.5000  850.5000  944.0000 1844.5000 1747.9999
    # [23,]    671.0    927.0   1002.0   2039.0   1847.0   1374.0  634.0000  872.4000  920.3333 1799.0001 1763.0000
    # [24,]    693.0    970.0   1116.0   2099.0   1903.0   1423.0  666.0000  872.0000  999.0000 1866.0000 1672.0000
    # [25,]    785.0   1094.0   1308.0   2375.0   2474.0   1808.0  746.5000  974.0000 1116.0000 2204.5000 2231.5000
    # [26,]    684.0   1016.0   1239.0   2600.0   2749.0   1806.0  691.5000  938.5000 1090.5000 2583.9999 2620.5000
    # [27,]    667.0   1014.0   1168.0   2671.0   2837.0   1827.0  628.0000  909.0000 1023.5000 2680.0001 2692.0000
    # X00050_12  X00050_13 X00050_14 X00050_15 X00050_16  X00050_17 X00050_18 X00050_19 X00050_20 X00050_21
    # [1,] 1288.0000   797.0000 1058.0000 1090.5000 2202.0000  1861.5000 1260.0000     768.0    1015.0    1090.0
    # [2,] 1178.0000   678.5000  873.5000  943.5000 1921.0000  1659.9999 1202.0000     683.5     894.5     977.0
    # [3,] 1339.0000   831.2500 1095.3333 1180.0000 2002.0001  1767.0000 1320.4999     864.5    1066.0    1228.5
    # [4,] 1405.5000  1064.5000 1385.7500 1465.0001 2130.4999  1925.0000 1452.7500     982.5    1247.5    1379.5
    # [5,] 1506.4999   840.0000 1140.0000 1234.0000 2346.9999  1892.0000 1316.9999     789.0    1048.5    1158.0
    # [6,] 1446.0000   692.0000  976.2000 1076.5000 2282.2499  1986.5000 1366.5000     718.5     983.5    1124.0
    # [7,] 1367.0000   659.0000  969.2500 1127.7500 2388.5000  2310.0001 1604.5000     740.0     986.0    1128.5
    # [8,] 1575.0000   729.0000 1008.0000 1136.0000 2112.5001  2064.5000 1624.0001     794.0     975.0    1105.5
    # [9,] 1562.0001   798.0000 1039.0000 1156.0000 1950.9999  1962.0000 1589.9999     825.0    1031.5    1179.0
    # [10,] 1825.5000   904.0000 1204.0000 1376.0000 2229.0000  2146.0000 1797.0000     904.0    1132.0    1370.0
    # [11,] 1520.0000   846.0000 1122.0000 1336.0000 1971.0000  1959.9999 1657.0000     884.0    1125.0    1344.0
    # [12,] 1240.0000   793.5000 1027.0000 1131.0000 1879.9999  1673.0000 1378.9999     825.0    1054.0    1221.0
    # [13,] 1258.0000   749.0000  930.5000 1001.5000 1801.5000  1604.0000 1236.0000     780.0     979.0    1057.0
    # [14,] 1218.5000   654.0000  875.5000  953.5000 1861.5000  1588.5000 1216.0000     719.0     938.0    1050.0
    # [15,] 1542.0000   710.5000  939.3334 1073.0000 1939.0000  1983.0000 1493.5000     747.0     983.0    1128.0
    # [16,] 1389.0000   631.0000  873.6667  964.2500 1932.9999  1791.5000 1348.5000     699.0     893.0    1002.0
    # [17,] 1123.0000   607.5000  828.0000  895.2500 2012.5000  1601.0000 1178.5000     653.0     825.0     972.0
    # [18,] 1216.0000   719.5000  863.6667  916.0000 2037.9999  1752.0000 1228.0000     794.0     869.0     917.0
    # [19,] 1234.0000   742.0000  903.0000  937.0000 1902.0000  1763.0000 1260.0000     787.0     950.0     976.0
    # [20,] 1259.0000   676.5000  871.3333  936.5000 1775.6666  1578.0000 1228.5000     703.5     892.0     958.5
    # [21,] 1079.0000   666.0000  912.0000 1014.1667 1863.3333  1642.5000 1216.0000     713.5     933.5    1063.0
    # [22,] 1342.0001   792.7500 1042.5000 1137.5000 1953.0000  1946.0000 1446.0000     851.0    1053.0    1221.0
    # [23,] 1363.0000   749.5000  984.0000 1107.0000 1882.0000  1914.9999 1463.0000     782.5     962.0    1160.0
    # [24,] 1314.0000   743.0000  986.5000 1114.5833 1914.0001  1859.0000 1469.0000     711.5     913.0    1061.0
    # [25,] 1615.9999   789.0000 1052.7500 1220.0000 2212.0000  2278.5001 1735.0000     730.5     967.0    1116.5
    # [26,] 1723.3333   645.0000  929.0000 1117.3333 2511.0000  2660.9999 1729.9999     637.0     875.5    1091.0
    # [27,] 1680.0000   658.0000  949.2500 1097.0000 2552.5001  2691.0001 1778.0000     626.0     841.0    1068.0
    # X00050_22 X00050_23 X00050_24 X00050_25 X00050_26 X00050_27 X00050_28 X00050_29 X00050_30 X00050_31
    # [1,]    2387.0    1899.0    1429.5  682.0000  910.0000  922.0000  2380.000 1796.0000 1206.0000  609.0000
    # [2,]    2154.0    1819.0    1296.0  700.0000  935.0000 1006.0000  2201.000 1967.9999 1367.0000  747.0000
    # [3,]    2154.0    1993.5    1508.0  765.0000 1070.0000 1168.0000  2181.000 2053.0000 1550.0000  872.0000
    # [4,]    2183.5    2032.0    1574.0  869.0000 1166.0000 1297.0001  2165.500 2078.0000 1574.0000  990.5715
    # [5,]    2483.0    2194.0    1512.5  783.4000 1020.5000 1112.2500  2380.000 2145.5000 1499.5000  759.5000
    # [6,]    2472.5    2275.0    1590.0  730.0000 1013.0000 1090.0000  2414.500 2345.5000 1588.9999  722.6667
    # [7,]    2404.5    2287.5    1784.0  683.0000  954.5000 1083.0000  2292.000 2346.9999 1751.0000  711.0000
    # [8,]    2132.5    2113.5    1613.0  678.7500  890.5000 1007.6667  2052.500 1879.5000 1446.5000  670.0000
    # [9,]    2061.0    2105.5    1703.5  759.6666  970.0000 1063.0000  1989.333 1878.6667 1503.0000  734.0000
    # [10,]    2218.0    2304.0    1944.0  795.0000 1113.0000 1235.0000  2143.000 2151.0001 1735.0000  784.2500
    # [11,]    2128.0    2126.0    1766.0  779.0000 1027.0000 1200.0000  1936.000 1922.5000 1604.5000  759.6666
    # [12,]    2036.0    1913.0    1578.0  725.0000  934.0000 1077.0000  1857.000 1667.5000 1398.6667  664.6250
    # [13,]    2073.0    1782.0    1337.0  688.0000  890.5000  955.0000  1938.750 1580.0001 1234.7500  625.8333
    # [14,]    1987.0    1721.0    1367.0  679.5000  893.8333  978.6667  1986.000 1750.7499 1302.0000  592.3333
    # [15,]    1987.0    2067.0    1651.0  686.2500  957.0000 1077.0000  1987.000 2057.0000 1536.0001  714.7500
    # [16,]    2064.0    1918.0    1439.0  627.0000  871.5000 1000.5000  2001.500 1880.5000 1355.5001  649.6250
    # [17,]    2149.0    1697.0    1252.0  622.5000  856.5000  940.0000  2201.000 1746.8333 1230.0000  571.2750
    # [18,]    2236.0    1807.0    1278.0  738.0000  934.5000 1005.5000  2165.500 1892.0000 1347.4999  797.0000
    # [19,]    2047.0    1824.0    1282.0  704.5000  939.0000 1027.0000  2037.000 1904.5000 1348.0000  705.0000
    # [20,]    1865.5    1688.5    1238.5  626.0000  853.0000  992.0000  1906.000 1780.0000 1322.0000  616.3333
    # [21,]    1955.0    1717.5    1311.5  675.0000  888.0000 1016.0000  1961.000 1777.0000 1318.0000  638.5000
    # [22,]    2030.5    2059.5    1548.5  789.0000 1035.0000 1235.5000  2017.000 2099.0001 1586.5000  812.0000
    # [23,]    1966.0    2029.5    1647.5  730.0000  997.0000 1142.0000  1965.000 2128.0000 1694.9999  740.5000
    # [24,]    1952.0    1800.0    1406.5  675.0000  934.0000 1052.0000  1981.000 1913.0000 1471.0000  712.7500
    # [25,]    2226.0    2139.5    1566.5  760.0000 1017.0000 1202.0000  2267.000 2270.0000 1676.9999  772.8333
    # [26,]    2571.0    2343.5    1634.5  664.5000 1006.5000 1215.0000  2644.000 2934.5000 1838.5001  892.0000
    # [27,]    2717.0    2581.5    1621.0  638.0000  929.0000 1112.0000  2763.500 3084.0001 1873.5000 1041.7500
    # X00050_32 X00050_33 X00050_34 X00050_35 X00050_36
    # [1,]  880.8750  932.3333 2507.0000 1967.0001 1340.0000
    # [2,] 1052.6667 1190.8750 2414.5000 2265.0000 1625.7501
    # [3,] 1187.6000 1349.0000 2406.6667 2255.8333 1691.5555
    # [4,] 1328.2000 1519.6666 2502.9999 2357.2500 1764.0001
    # [5,] 1089.8571 1320.4999 2832.1251 2671.0001 1840.0000
    # [6,] 1019.0000 1220.5000 2665.6666 2736.0001 1864.0000
    # [7,]  988.6666 1141.0000 2469.5000 2571.3333 1878.0001
    # [8,]  901.3333  974.0000 2117.9999 2007.5715 1536.0001
    # [9,]  969.3334 1067.0000 1991.5000 1907.6666 1546.5000
    # [10,] 1077.5000 1235.0000 2231.0001 2161.3333 1772.0000
    # [11,]  993.5000 1156.3333 1950.9999 1938.0000 1610.0000
    # [12,]  894.5000 1017.5000 1904.5000 1760.0000 1479.4999
    # [13,]  852.4000  899.1250 1991.7083 1650.9999 1296.0000
    # [14,]  845.0000  914.3333 2036.3334 1783.6666 1367.0000
    # [15,]  953.6666 1044.8334 2043.2501 1944.2500 1491.0001
    # [16,]  904.0000  972.5000 2096.0000 1859.0000 1363.5001
    # [17,]  843.8750  900.5000 2250.2001 1743.0000 1247.5000
    # [18,] 1078.0000 1190.0000 2180.3333 1880.6666 1420.0000
    # [19,]  954.6000 1042.5000 2107.5000 1896.5000 1412.9999
    # [20,]  852.0000  980.0000 1939.0000 1813.0000 1380.5000
    # [21,]  850.6000  979.7500 1914.0001 1739.9999 1323.0000
    # [22,] 1050.0000 1231.8000 2110.2500 2177.5000 1670.0000
    # [23,]  983.3334 1147.5000 2017.0000 2154.0000 1722.3333
    # [24,]  954.8333 1075.4500 2020.9999 1971.5001 1567.3333
    # [25,] 1058.0000 1194.5000 2340.5625 2322.8334 1728.1666
    # [26,] 1246.6667 1481.5000 2803.7500 2958.3332 2235.3999
    # [27,] 1469.0000 1780.3334 2858.5714 3331.5000 2678.0000
    
    # 
    # Pause and wait for user input
    cat(paste(dhs_df$iso3[i],
              dhs_df$lat[i],
              dhs_df$lon[i],
              year_groups[j], "Press enter for next year group..."))
    readline(prompt = "")
        
  }
  # Pause and wait for user input
  cat("Press enter to proceed to the next DHS point...")
  readline(prompt = "")
}
 # Reset the layout
par(mfrow = c(1, 1))

################################################################################
# Loop through dhs images to identify negative values.  Write to csv after
################################################################################
neg_values_list <- list()
list_index <-  0

for (i in 1:nrow(dhs_df)) {
  if (i %% 100 == 0) {
    print(paste("[",format(Sys.time(), "%Y-%m-%d %H:%M:%S"),"] iteration ",i))
  }
  #uncomment to test
  #i=1
  # Read the images
  i_original <- raster::brick(dhs_df$image_file[i])
  i_annual   <- raster::brick(dhs_df$image_file_annual[i])
  i_5k_3yr   <- raster::brick(dhs_df$image_file_5k_3yr[i])   
  
  #check the original images
  min_values <- cellStats(i_original, stat="min")
  neg_values <- which(min_values < 0, arr.ind=TRUE)
  for (k in seq_along(neg_values)) {
    #uncomment to test
    #k=1
    list_index <- list_index + 1
    neg_values_list[[list_index]] <- list(
      dhs_id = dhs_df$dhs_id[i],
      image = dhs_df$image_file[i],
      image_type = "original",
      layer = neg_values[k],
      nvalue = min_values[neg_values[k]]   
      )
  }
  
  #check the 3 yr images
  min_values <- cellStats(i_5k_3yr, stat="min")
  neg_values <- which(min_values < 0, arr.ind=TRUE)
  for (k in seq_along(neg_values)) {
    list_index <- list_index + 1
    neg_values_list[[list_index]] <- list(
      dhs_id = dhs_df$dhs_id[i],
      image = dhs_df$image_file_5k_3yr[i],
      image_type = "3yr",
      layer = neg_values[k],
      nvalue = min_values[neg_values[k]]   
      )
  }
  
  #check the annual images
  min_values <- cellStats(i_annual, stat="min")
  neg_values <- which(min_values < 0, arr.ind=TRUE)
  for (k in seq_along(neg_values)) {
    list_index <- list_index + 1
    neg_values_list[[list_index]] <- list(
      dhs_id = dhs_df$dhs_id[i],
      image = dhs_df$image_file_annual[i],
      image_type = "annual",
      layer = neg_values[k],
      nvalue = min_values[neg_values[k]] 
      )
  }
}
#convert list to dataframe
neg_values_df <- do.call(rbind.data.frame,neg_values_list)

#write the file
write.csv(neg_values_df,"./data/interim/images_w_neg_values.csv",row.names=FALSE)

#add additional variables for analysis
dhs_neg_df <- neg_values_df %>% 
  left_join(dhs_df,by=join_by(dhs_id)) %>% 
  mutate(year_group=case_when(image_type=="annual" & layer %in% 1:6   ~ "2000",
                              image_type=="annual" & layer %in% 7:12  ~ "2001",
                              image_type=="annual" & layer %in% 13:18 ~ "2002",
                              image_type=="annual" & layer %in% 19:24 ~ "2003",
                              image_type=="annual" & layer %in% 25:30 ~ "2004",
                              image_type=="annual" & layer %in% 31:36 ~ "2005",
                              image_type=="annual" & layer %in% 37:42 ~ "2006",
                              image_type=="annual" & layer %in% 43:48 ~ "2007",
                              image_type=="annual" & layer %in% 49:54 ~ "2008",
                              image_type=="annual" & layer %in% 55:60 ~ "2009",
                              image_type=="annual" & layer %in% 61:66 ~ "2010",
                              image_type=="annual" & layer %in% 67:72 ~ "2011",
                              image_type=="annual" & layer %in% 73:78 ~ "2012",
                              image_type=="annual" & layer %in% 79:84 ~ "2013",
                              image_type=="3yr" & layer %in% 1:6 ~ "1999-2001",
                              image_type=="3yr" & layer %in% 7:12 ~ "2002-2004",
                              image_type=="3yr" & layer %in% 13:18 ~ "2005-2007",
                              image_type=="3yr" & layer %in% 19:24 ~ "2008-2010",
                              image_type=="3yr" & layer %in% 25:30 ~ "2011-2013",
                              image_type=="3yr" & layer %in% 31:36 ~ "2014-2016",
                              image_type=="original" & layer %in% 1:8 ~ "1990-1992",
                              image_type=="original" & layer %in% 9:16 ~ "1993-1995",
                              image_type=="original" & layer %in% 17:24 ~ "1996-1998",
                              image_type=="original" & layer %in% 25:32 ~ "1999-2001",
                              image_type=="original" & layer %in% 33:40 ~ "2002-2004",
                              image_type=="original" & layer %in% 41:48 ~ "2005-2007",
                              image_type=="original" & layer %in% 49:56 ~ "2008-2010",
                              image_type=="original" & layer %in% 57:64 ~ "2011-2013",
                              image_type=="original" & layer %in% 65:72 ~ "2014-2016")) %>% 
  mutate(band=case_when(image_type=="annual" & layer %in% c(1,7,13,19,25,31,37,43,49,55,61,67,73,79) ~ "BLUE",
                        image_type=="annual" & layer %in% c(2,8,14,20,26,32,38,44,50,56,62,68,74,80) ~ "GREEN",
                        image_type=="annual" & layer %in% c(3,9,15,21,27,33,39,45,51,57,63,69,75,81) ~ "RED",
                        image_type=="annual" & layer %in% c(4,10,16,22,28,34,40,46,52,58,64,70,76,82) ~ "NIR",
                        image_type=="annual" & layer %in% c(5,11,17,23,29,35,41,47,53,59,65,71,77,83) ~ "SWIR1",
                        image_type=="annual" & layer %in% c(6,12,18,24,30,36,42,48,54,60,66,72,78,84) ~ "SWIR2",
                        image_type=="3yr" & layer %in% c(1,7,13,19,25,31) ~ "BLUE",
                        image_type=="3yr" & layer %in% c(2,8,14,20,26,32) ~ "GREEN",
                        image_type=="3yr" & layer %in% c(3,9,15,21,27,33) ~ "RED",
                        image_type=="3yr" & layer %in% c(4,10,16,22,28,34) ~ "NIR",
                        image_type=="3yr" & layer %in% c(5,11,17,23,29,35) ~ "SWIR1",
                        image_type=="3yr" & layer %in% c(6,12,18,24,30,36) ~ "SWIR2",           
                        image_type=="original" & layer %in% c(1,9,17,25,33,41,49,57,65,73) ~ "BLUE",
                        image_type=="original" & layer %in% c(2,10,18,26,34,42,50,58,66,74) ~ "GREEN",
                        image_type=="original" & layer %in% c(3,11,19,27,35,43,51,59,67,75) ~ "RED",
                        image_type=="original" & layer %in% c(4,12,20,28,36,44,52,60,68,76) ~ "NIR",
                        image_type=="original" & layer %in% c(5,13,21,29,37,45,53,61,69,77) ~ "SWIR1",
                        image_type=="original" & layer %in% c(6,14,22,30,38,46,54,62,70,78) ~ "SWIR2",
                        image_type=="original" & layer %in% c(67,15,23,31,39,47,55,63,71,79) ~ "TEMP",
                        image_type=="original" & layer %in% c(8,16,24,32,40,48,56,64,72,80) ~ "NL")
         )

#write larger analysis file
write.csv(dhs_neg_df,"./data/interim/images_w_neg_values_extended.csv",row.names=FALSE)

########################

dhs_df <- dhs_df %>% 
  filter(image_file_5k_3yr %in% c(
    "./data/dhs_tifs_c1_5k_3yr/cameroon_2004/00137.tif",
    "./data/dhs_tifs_c1_5k_3yr/cameroon_2004/00133.tif",
    "./data/dhs_tifs_c1_5k_3yr/cameroon_2004/00151.tif"
  ))

dhs_df %>% 
  filter(country=="cameroon")

c1 <- raster::brick("./data/dhs_tifs_5k_3yr/cameroon_2004/00409.tif")
c2 <- raster::brick("./data/dhs_tifs_5k_3yr/cameroon_2004/00070.tif")
c3 <- raster::brick("./data/dhs_tifs_5k_3yr/cameroon_2004/00067.tif")
c4 <- raster::brick("./data/dhs_tifs_5k_3yr/cameroon_2004/00062.tif")

c5 <- raster::brick("./data/dhs_tifs_c1_5k_3yr/cameroon_2004/00137.tif")  #2005:2007 bands 7-9
c6 <- raster::brick("./data/dhs_tifs_c1_5k_3yr/cameroon_2004/00133.tif") #2005:2007  bands 7-9
c7 <- raster::brick("./data/dhs_tifs_c1_5k_3yr/cameroon_2004/00151.tif") #2005:2007  bands 7-9

c5_initial <- raster::brick("./data/dhs_tifs/cameroon_2004/00431.tif")  #2005:2007   bands 41-43
c6_initial <- raster::brick("./data/dhs_tifs/cameroon_2004/00404.tif") #2005:2007    bands 41-43
c7_initial <- raster::brick("./data/dhs_tifs/cameroon_2004/00414.tif") #2005:2007    bands 41-43


c5_rgb <-  c5[[c(7,8,9)]]
c5_scaled <- ((c5_rgb + .2)/.0000275)
raster::plotRGB(c5_scaled,r=3,g=2,b=1,stretch="hist")						 
dev.off()	


c5_initial_rgb <-  c5_initial[[c(41,42,43)]]
c5_initial_scaled <- ((c5_initial_rgb)/.0001)
raster::plotRGB(c5_initial_scaled,r=3,g=2,b=1,stretch="hist")		
#put vertical/horizontal lines to create approximatly 5k box for comparisons
w = ncol(c5_initial_scaled)
abline(v=w/3,col="White")
abline(v=1 * w/4,col="White")
abline(h=w/3,col="White")
abline(h=2 * w/3,col="White")
dev.off()	



n1_c1_rgb <-  n1_c1[[c(1,2,3)]]
n1_c1_scaled <- (n1_c1_rgb/.0001)
raster::plotRGB(n1_c1_scaled,r=3,g=2,b=1,stretch="hist")						 
dev.off()	



n1 <- raster::brick("./data/dhs_tifs_5k_3yr/niger_1998/00074.tif") #2008:2010
n1_c2_v3 <- raster::brick("./data/dhs_tifs_c1_5k_3yr/niger_1998/00074.tif")

c1_c1 <- raster::brick("./data/dhs_tifs_c1_5k_3yr/cameroon_2004/00409.tif")
c2_c1 <- raster::brick("./data/dhs_tifs_c1_5k_3yr/cameroon_2004/00070.tif")
c3_c1 <- raster::brick("./data/dhs_tifs_c1_5k_3yr/cameroon_2004/00067.tif")
c4_c1 <- raster::brick("./data/dhs_tifs_c1_5k_3yr/cameroon_2004/00062.tif")
n1_v3 <- raster::brick("./data/dhs_tifs_5k_3yr_v3/niger_1998/00074.tif")

n1_rgb <-  n1[[c(1,2,3)]]
n1_scaled <- ((n1_rgb + .2)/.0000275)
raster::plotRGB(n1_scaled,r=3,g=2,b=1,stretch="hist")						 
dev.off()					 


n1_v3_rgb <-  n1_v3[[c(1,2,3)]]
n1_v3_scaled <- ((n1_v3_rgb + .2)/.0000275)
raster::plotRGB(n1_v3_scaled,r=3,g=2,b=1,stretch="hist")						 
dev.off()	

n1_c1_rgb <-  n1_c1[[c(1,2,3)]]
n1_c1_scaled <- (n1_c1_rgb/.0001)
raster::plotRGB(n1_c1_scaled,r=3,g=2,b=1,stretch="hist")						 
dev.off()		


n1_200810_rgb <-  n1[[c(13,14,15)]]
n1_200810_scaled <- ((n1_200810_rgb + .2)/.0000275)
raster::plotRGB(n1_200810_scaled,r=3,g=2,b=1,stretch="hist")						 
dev.off()	

n1_c1_200810_rgb <-  n1_c1[[c(13,14,15)]]
n1_c1_200810_scaled <- (n1_c1_200810_rgb/.0001)
raster::plotRGB(n1_c1_200810_scaled,r=3,g=2,b=1,stretch="hist")						 
dev.off()		

######
c1_rgb <-  c1[[c(1,2,3)]]
c1_scaled <- ((c1_rgb + .2)/.0000275)
raster::plotRGB(c1_scaled,r=3,g=2,b=1,stretch="hist")						 
dev.off()					 

c1_c1_rgb <-  c1_c1[[c(1,2,3)]]
c1_c1_scaled <- (c1_c1_rgb/.0001)
raster::plotRGB(c1_c1_scaled,r=3,g=2,b=1,stretch="hist")						 
dev.off()					

######
c2_rgb <-  c2[[c(1,2,3)]]
c2_scaled <- ((c2_rgb + .2)/.0000275)
raster::plotRGB(c2_scaled,r=3,g=2,b=1,stretch="hist")						 
dev.off()					 

c2_c1_rgb <-  c2_c1[[c(1,2,3)]]
c2_c1_scaled <- (c2_c1_rgb/.0001)
raster::plotRGB(c2_c1_scaled,r=3,g=2,b=1,stretch="hist")						 
dev.off()
######==
  c3_rgb <-  c3[[c(1,2,3)]]
c3_scaled <- ((c3_rgb + .2)/.0000275)
raster::plotRGB(c3_scaled,r=3,g=2,b=1,stretch="hist")						 
dev.off()					 

c3_c1_rgb <-  c3_c1[[c(1,2,3)]]
c3_c1_scaled <- c3_c1_rgb /.0001
raster::plotRGB(c3_c1_scaled,r=3,g=2,b=1,stretch="hist")						 
dev.off()
######==
  c4_rgb <-  c4[[c(1,2,3)]]
c4_scaled <- ((c4_rgb + .2)/.0000275)
raster::plotRGB(c4_scaled,r=3,g=2,b=1,stretch="hist")						 
dev.off()					 

c4_c1_rgb <-  c4_c1[[c(1,2,3)]]
c4_c1_scaled <- c4_c1_rgb/.0001
raster::plotRGB(c4_c1_scaled,r=3,g=2,b=1,stretch="hist")						 
dev.off()
######==
  
most_least_likely_v <- c('./data/dhs_tifs_5k_3yr/niger_1998/00074.tif',
                         './data/dhs_tifs_5k_3yr/niger_1998/00072.tif',
                         './data/dhs_tifs_5k_3yr/niger_1998/00084.tif',
                         './data/dhs_tifs_5k_3yr/niger_1998/00074.tif',
                         './data/dhs_tifs_5k_3yr/niger_1998/00072.tif',
                         './data/dhs_tifs_5k_3yr/niger_1998/00084.tif',
                         './data/dhs_tifs_5k_3yr/niger_1998/00183.tif',
                         './data/dhs_tifs_5k_3yr/niger_1998/00106.tif',
                         './data/dhs_tifs_5k_3yr/niger_1998/00182.tif',
                         './data/dhs_tifs_5k_3yr/niger_1998/00056.tif',
                         './data/dhs_tifs_5k_3yr/niger_1998/00053.tif',
                         './data/dhs_tifs_5k_3yr/niger_1998/00059.tif',
                         './data/dhs_tifs_5k_3yr/niger_1998/00106.tif',
                         './data/dhs_tifs_5k_3yr/niger_1998/00176.tif',
                         './data/dhs_tifs_5k_3yr/niger_1998/00040.tif',
                         './data/dhs_tifs_5k_3yr/comoros_2012/00204.tif',
                         './data/dhs_tifs_5k_3yr/comoros_2012/00194.tif',
                         './data/dhs_tifs_5k_3yr/comoros_2012/00201.tif',
                         './data/dhs_tifs_5k_3yr/angola_2006/00027.tif',
                         './data/dhs_tifs_5k_3yr/angola_2006/00001.tif',
                         './data/dhs_tifs_5k_3yr/lesotho_2004/00214.tif',
                         './data/dhs_tifs_5k_3yr/lesotho_2004/00211.tif',
                         './data/dhs_tifs_5k_3yr/lesotho_2004/00192.tif',
                         './data/dhs_tifs_5k_3yr/senegal_1997/00225.tif',
                         './data/dhs_tifs_5k_3yr/senegal_1997/00218.tif',
                         './data/dhs_tifs_5k_3yr/ethiopia_2000/00224.tif',
                         './data/dhs_tifs_5k_3yr/tanzania_1999/00160.tif',
                         './data/dhs_tifs_5k_3yr/tanzania_1999/00161.tif',
                         './data/dhs_tifs_5k_3yr/zimbabwe_1999/00132.tif',
                         './data/dhs_tifs_5k_3yr/namibia_2000/00061.tif',
                         './data/dhs_tifs_5k_3yr/namibia_2000/00062.tif',
                         './data/dhs_tifs_5k_3yr/namibia_2000/00061.tif',
                         './data/dhs_tifs_5k_3yr/senegal_1997/00225.tif',
                         './data/dhs_tifs_5k_3yr/senegal_1997/00218.tif',
                         './data/dhs_tifs_5k_3yr/cameroon_2004/00409.tif',
                         './data/dhs_tifs_5k_3yr/cameroon_2004/00070.tif',
                         './data/dhs_tifs_5k_3yr/cameroon_2004/00067.tif',
                         './data/dhs_tifs_5k_3yr/cameroon_2004/00062.tif',
                         './data/dhs_tifs_5k_3yr/namibia_2000/00190.tif',
                         './data/dhs_tifs_5k_3yr/namibia_2000/00185.tif',
                         './data/dhs_tifs_5k_3yr/namibia_2000/00195.tif',
                          './data/dhs_tifs_5k_3yr/nigeria_2003/00328.tif',
                          './data/dhs_tifs_5k_3yr/nigeria_2003/00326.tif',
                          './data/dhs_tifs_5k_3yr/nigeria_2003/00320.tif',
                          './data/dhs_tifs_5k_3yr/chad_2014/00087.tif',
                          './data/dhs_tifs_5k_3yr/chad_2014/00106.tif',
                          './data/dhs_tifs_5k_3yr/chad_2014/00090.tif',
                          './data/dhs_tifs_5k_3yr/niger_1998/00106.tif',
                         './data/dhs_tifs_5k_3yr/niger_1998/00176.tif',
                          './data/dhs_tifs_5k_3yr/namibia_2000/00190.tif',
                          './data/dhs_tifs_5k_3yr/namibia_2000/00195.tif',
                          './data/dhs_tifs_5k_3yr/namibia_2000/00185.tif',
                          './data/dhs_tifs_5k_3yr/guinea_1999/00071.tif',
                          './data/dhs_tifs_5k_3yr/guinea_1999/00069.tif',
                          './data/dhs_tifs_5k_3yr/guinea_1999/00074.tif',
                          './data/dhs_tifs_5k_3yr/nigeria_2003/00328.tif',
                          './data/dhs_tifs_5k_3yr/nigeria_2003/00326.tif',
                          './data/dhs_tifs_5k_3yr/nigeria_2003/00351.tif',
                          './data/dhs_tifs_5k_3yr/rwanda_2005/00236.tif',
                          './data/dhs_tifs_5k_3yr/rwanda_2005/00240.tif',
                          './data/dhs_tifs_5k_3yr/rwanda_2005/00229.tif',
                          './data/dhs_tifs_5k_3yr/niger_1998/00106.tif',
                          './data/dhs_tifs_5k_3yr/niger_1998/00176.tif',
                          './data/dhs_tifs_5k_3yr/niger_1998/00040.tif',
                          './data/dhs_tifs_5k_3yr/guinea_1999/00078.tif',
                         './data/dhs_tifs_5k_3yr/guinea_1999/00090.tif',
                         './data/dhs_tifs_5k_3yr/chad_2014/00220.tif',
                         './data/dhs_tifs_5k_3yr/nigeria_2003/00351.tif',
                         './data/dhs_tifs_5k_3yr/chad_2014/00220.tif',
                         './data/dhs_tifs_5k_3yr/uganda_2000/00050.tif',
                         './data/dhs_tifs_5k_3yr/uganda_2000/00055.tif',
                         './data/dhs_tifs_5k_3yr/uganda_2000/00054.tif',
                         './data/dhs_tifs_5k_3yr/nigeria_2003/00167.tif',
                         './data/dhs_tifs_5k_3yr/tanzania_1999/00012.tif',
                         './data/dhs_tifs_5k_3yr/nigeria_2003/00220.tif',
                         './data/dhs_tifs_5k_3yr/angola_2006/00055.tif',
                         './data/dhs_tifs_5k_3yr/angola_2006/00056.tif',
                         './data/dhs_tifs_5k_3yr/guinea_1999/00084.tif',
                         './data/dhs_tifs_5k_3yr/niger_1998/00168.tif',
                         './data/dhs_tifs_5k_3yr/nigeria_2003/00328.tif',
                         './data/dhs_tifs_5k_3yr/niger_1998/00106.tif',
                         './data/dhs_tifs_5k_3yr/ethiopia_2000/00473.tif',
                         './data/dhs_tifs_5k_3yr/ethiopia_2000/00472.tif',
                         './data/dhs_tifs_5k_3yr/madagascar_1997/00115.tif',
                         './data/dhs_tifs_5k_3yr/chad_2014/00220.tif',
                         './data/dhs_tifs_5k_3yr/chad_2014/00220.tif',
                         './data/dhs_tifs_5k_3yr/chad_2014/00220.tif',
                        './data/dhs_tifs_5k_3yr/cameroon_2004/00168.tif',
                        './data/dhs_tifs_5k_3yr/cameroon_2004/00030.tif',
                        './data/dhs_tifs_5k_3yr/cameroon_2004/00097.tif',
                        './data/dhs_tifs_5k_3yr/ghana_1999/00038.tif',
                        './data/dhs_tifs_5k_3yr/ghana_1999/00097.tif',
                        './data/dhs_tifs_5k_3yr/ghana_1999/00055.tif',
                        './data/dhs_tifs_5k_3yr/chad_2014/00221.tif',
                        './data/dhs_tifs_5k_3yr/chad_2014/00219.tif',
                        './data/dhs_tifs_5k_3yr/chad_2014/00221.tif',
                        './data/dhs_tifs_5k_3yr/benin_1996/00012.tif',
                        './data/dhs_tifs_5k_3yr/benin_1996/00001.tif',
                        './data/dhs_tifs_5k_3yr/benin_1996/00003.tif',
                        './data/dhs_tifs_5k_3yr/zambia_2007/00256.tif',
                        './data/dhs_tifs_5k_3yr/zambia_2007/00266.tif',
                        './data/dhs_tifs_5k_3yr/zambia_2007/00276.tif',
                        './data/dhs_tifs_5k_3yr/mali_1996/00158.tif',
                        './data/dhs_tifs_5k_3yr/mali_1996/00157.tif',
                        './data/dhs_tifs_5k_3yr/chad_2014/00220.tif',
                        './data/dhs_tifs_5k_3yr/chad_2014/00220.tif',
                        './data/dhs_tifs_5k_3yr/chad_2014/00220.tif',
                        './data/dhs_tifs_5k_3yr/benin_1996/00001.tif',
                        './data/dhs_tifs_5k_3yr/benin_1996/00003.tif',
                        './data/dhs_tifs_5k_3yr/benin_1996/00008.tif',
                        './data/dhs_tifs_5k_3yr/zambia_2007/00168.tif',
                        './data/dhs_tifs_5k_3yr/zambia_2007/00171.tif',
                        './data/dhs_tifs_5k_3yr/angola_2006/00027.tif',
                        './data/dhs_tifs_5k_3yr/eswatini_2006/00005.tif',
                        './data/dhs_tifs_5k_3yr/eswatini_2006/00003.tif',
                        './data/dhs_tifs_5k_3yr/eswatini_2006/00151.tif',
                        './data/dhs_tifs_5k_3yr/lesotho_2004/00118.tif',
                        './data/dhs_tifs_5k_3yr/nigeria_2003/00351.tif',
                        './data/dhs_tifs_5k_3yr/chad_2014/00220.tif',
                        './data/dhs_tifs_5k_3yr/chad_2014/00220.tif',
                        './data/dhs_tifs_5k_3yr/chad_2014/00220.tif',
                        './data/dhs_tifs_5k_3yr/chad_2014/00220.tif',
                        './data/dhs_tifs_5k_3yr/angola_2006/00001.tif') 
#exclude duplicates
most_least_likely_v <-  unique(most_least_likely_v)

#get other DHS attributes, limit to those in vector above
dhs_df <- read.csv("./data/interim/dhs_est_iwi.csv") %>% 
  filter(image_file_5k_3yr %in% most_least_likely_v) %>% 
  select(dhs_id, lat, lon, country, image_file, image_file_annual, image_file_5k_3yr)

year_group_v <- c('2002:2004', '2005:2007', '2008:2010', '2011:2013', '2014:2016')

for (i in 1:nrow(dhs_df)) {
  i=1
  for (oda_year_group in year_group_v) {
    oda_year_group = '2002:2004'
    bands_to_select5k <- case_when(							  
      oda_year_group == '2002:2004' ~ 1:3,
      oda_year_group == '2005:2007' ~ 7:9,
      oda_year_group == '2008:2010' ~ 13:15,
      oda_year_group == '2011:2013' ~ 19:21,
      oda_year_group == '2014:2016' ~ 25:27)
    
    # iterate over all image bands to construct their names
    image_5k_name <-dhs_df[i,"image_file_5k_3yr"]
    #image_name = './data/dhs_tifs_5k_3yr/chad_2014/00220.tif'
    image_short_name <- gsub(pattern=".*/(\\d{5})\\.tif$","\\1", x=image_5k_name)
    
    layers5k=paste(image_short_name, bands_to_select5k,"_")
    for(i in 1:3) {
      band_ <- bands_to_select5k[i]
      layers5k=paste0(layers5k,", \'",gsub(pattern=".*/(\\d{5})\\.tif$","\\1", x=image_5k_name)
                  ,"_",band_)
    }
    #remove leading comma space
    layers5k=paste0(sub("^, ", "", layers5k),"'")
    
    #read the image
    im <- terra::rast(image_5k_name, lyrs=c(!!sym(layers5k)))  
    
    
    im <- terra::rast(image_5k_name, lyrs=c("00118_1", "00118_2", "00118_3"))  
    # class       : SpatRaster 
    # dimensions  : 167, 167, 36  (nrow, ncol, nlyr)
    # resolution  : 0.0003075762, 0.0002699141  (x, y)
    # extent      : 27.44171, 27.49308, -29.33524, -29.29016  (xmin, xmax, ymin, ymax)
    # coord. ref. : lon/lat WGS 84 (EPSG:4326) 
    # source      : 00211.tif 
    # names       : 00211_1, 00211_2, 00211_3, 00211_4, 00211_5, 00211_6, ... 
    
    
    im <- terra::rast(image_5k_name,
                      lyrs=c("00211_1", "00211_2", "00211_3"))  
    # Error: [rast] cannot open this file as a SpatRaster: /mimer/NOBACKUP/groups/globalpoverty1/cindy/eoml_ch_wb/data/dhs_tifs_5k_3yr/lesotho_2004/00211.tif
    # In addition: Warning message:
    #   In new_CppObject_xp(fields$.module, fields$.pointer, ...) :
    #   GDAL Error 4: `/mimer/NOBACKUP/groups/globalpoverty1/cindy/eoml_ch_wb/data/dhs_tifs_5k_3yr/lesotho_2004/00211.tif' not recognized as a supported file format.
    
    im <- terra::rast("/mimer/NOBACKUP/groups/globalpoverty1/cindy/eoml_ch_wb/data/dhs_tifs_5k_3yr_v3/lesotho_2004/00211.tif")
    
    # class       : SpatRaster 
    # dimensions  : 167, 167, 36  (nrow, ncol, nlyr)
    # resolution  : 0.0003075762, 0.0002699141  (x, y)
    # extent      : 27.44171, 27.49308, -29.33524, -29.29016  (xmin, xmax, ymin, ymax)
    # coord. ref. : lon/lat WGS 84 (EPSG:4326) 
    # source      : 00211.tif 
    # names       : 00211_1, 00211_2, 00211_3, 00211_4, 00211_5, 00211_6, ... 
    

    #rescale to original setting for RGB printing
    im <- ((im + .2)/.0000275)
    
    #plot
    # Output or intermediate results for review
    print(paste("5k:",image_5k_name,oda_year_group))
    raster::plotRGB(im,r=3,g=2,b=1,stretch="hist")
    
    #pause until I review
    cat("Press Enter to continue...")
    readline()
    
    #now show larger footprint file
    
    
  }
}





# Validate the download of annual images, comparing them to 3-year images
#raster library
#lat/lon  -11.491992 43.38610
comoros_old_r <- raster::brick("./data/dhs_tifs/comoros_2012/00149.tif")
# class      : RasterBrick 
# dimensions : 224, 224, 50176, 80  (nrow, ncol, ncell, nlayers)
# resolution : 0.0005473618, 0.0005398282  (x, y)
# extent     : 43.32489, 43.4475, -11.55244, -11.43152  (xmin, xmax, ymin, ymax)
# crs        : +proj=longlat +datum=WGS84 +no_defs 
# source     : 00149.tif 
# names      : X00149_1, X00149_2, X00149_3, X00149_4, X00149_5, X00149_6, X00149_7, X00149_8, X00149_9, X00149_10, X00149_11, X00149_12, X00149_13, X00149_14, X00149_15, ... 
comoros_old_rgb <-  comoros_old_r[[c(1,2,3)]]
comoros_old_scaled <- comoros_old_rgb/.0001
raster::plotRGB(comoros_old_scaled,r=3,g=2,b=1)
dev.off()
raster::plotRGB(comoros_old_scaled,r=3,g=2,b=1,stretch="lin")
dev.off()
raster::plotRGB(comoros_old_scaled,r=3,g=2,b=1,stretch="hist")

comoros_annual_r <- raster::brick("./data/dhs_tifs_annual/comoros_2012/00222.tif")
# class      : RasterBrick 
# dimensions : 167, 167, 27889, 84  (nrow, ncol, ncell, nlayers)
# resolution : 0.0002736804, 0.0002699141  (x, y)
# extent     : 43.36329, 43.40899, -11.51453, -11.46945  (xmin, xmax, ymin, ymax)
# crs        : +proj=longlat +datum=WGS84 +no_defs 
# source     : 00222.tif 
# names      : X00222_1, X00222_2, X00222_3, X00222_4, X00222_5, X00222_6, X00222_7, X00222_8, X00222_9, X00222_10, X00222_11, X00222_12, X00222_13, X00222_14, X00222_15, ... 


comoros_annual_terra_r <- terra::rast("./data/dhs_tifs_annual/comoros_2012/00222.tif",
                                      lyrs=c("00222_1","00222_2","00222_3"))
terra::cellSize(comoros_annual_terra_r)
# class       : SpatRaster 
# dimensions  : 167, 167, 1  (nrow, ncol, nlyr)
# resolution  : 0.0002736804, 0.0002699141  (x, y)
# extent      : 43.36329, 43.40899, -11.51453, -11.46945  (xmin, xmax, ymin, ymax)
# coord. ref. : lon/lat WGS 84 (EPSG:4326) 
# source(s)   : memory
# name        :     area 
# min value   : 891.4497 
# max value   : 891.5878 


#year 2000
comoros_annual_rgb <-  comoros_annual_r[[c(1,2,3)]]
comoros_annual_scaled <- comoros_annual_rgb/.0000275
dev.off()
raster::plotRGB(comoros_annual_scaled,r=3,g=2,b=1,stretch="hist")

#year 2001
comoros_annual_rgb <-  comoros_annual_r[[c(7,8,9)]]
comoros_annual_scaled <- comoros_annual_rgb/.0000875
dev.off()
raster::plotRGB(comoros_annual_scaled,r=3,g=2,b=1,stretch="hist")

#year 2002
comoros_annual_rgb <-  comoros_annual_r[[c(13,14,15)]]
comoros_annual_scaled <- comoros_annual_rgb/.0000275
dev.off()
raster::plotRGB(comoros_annual_scaled,r=3,g=2,b=1,stretch="hist")

#year 2003
comoros_annual_rgb <-  comoros_annual_r[[c(19,20,21)]]
comoros_annual_scaled <- comoros_annual_rgb/.0000275
dev.off()
raster::plotRGB(comoros_annual_scaled,r=3,g=2,b=1,stretch="hist")

#year 2004
comoros_annual_rgb <-  comoros_annual_r[[c(25,26,27)]]
comoros_annual_scaled <- comoros_annual_rgb/.0000275
raster::plotRGB(comoros_annual_scaled,r=3,g=2,b=1,stretch="hist")

#year 2005
comoros_annual_rgb <-  comoros_annual_r[[c(31,32,33)]]
comoros_annual_scaled <- comoros_annual_rgb/.0000275
dev.off()
raster::plotRGB(comoros_annual_scaled,r=3,g=2,b=1,stretch="hist")

#year 2006
comoros_annual_rgb <-  comoros_annual_r[[c(37,38,39)]]
comoros_annual_scaled <- comoros_annual_rgb/.0000275
dev.off()
raster::plotRGB(comoros_annual_scaled,r=3,g=2,b=1,stretch="hist")

#year 2007
comoros_annual_rgb <-  comoros_annual_r[[c(43,44,45)]]
comoros_annual_scaled <- comoros_annual_rgb/.0000275
dev.off()
raster::plotRGB(comoros_annual_scaled,r=3,g=2,b=1,stretch="hist")

#year 2008
comoros_annual_rgb <-  comoros_annual_r[[c(49,50,51)]]
comoros_annual_scaled <- comoros_annual_rgb/.0000275
dev.off()
raster::plotRGB(comoros_annual_scaled,r=3,g=2,b=1,stretch="hist")

#year 2009
comoros_annual_rgb <-  comoros_annual_r[[c(55,56,57)]]
comoros_annual_scaled <- comoros_annual_rgb/.0000275
dev.off()
raster::plotRGB(comoros_annual_scaled,r=3,g=2,b=1,stretch="hist")

#year 2010
comoros_annual_rgb <-  comoros_annual_r[[c(61,62,63)]]
comoros_annual_scaled <- comoros_annual_rgb/.0000275
dev.off()
raster::plotRGB(comoros_annual_scaled,r=3,g=2,b=1,stretch="hist")

#year 2011
comoros_annual_rgb <-  comoros_annual_r[[c(67,68,69)]]
comoros_annual_scaled <- comoros_annual_rgb/.0000275
dev.off()
raster::plotRGB(comoros_annual_scaled,r=3,g=2,b=1,stretch="hist")

#year 2012
comoros_annual_rgb <-  comoros_annual_r[[c(73,74,75)]]
comoros_annual_scaled <- comoros_annual_rgb/.0000275
dev.off()
raster::plotRGB(comoros_annual_scaled,r=3,g=2,b=1,stretch="hist")

#year 2013
comoros_annual_rgb <-  comoros_annual_r[[c(79,80,81)]]
comoros_annual_scaled <- comoros_annual_rgb/.0000275
dev.off()
raster::plotRGB(comoros_annual_scaled,r=3,g=2,b=1,stretch="hist")

#get list of all dhs points and their tif file locations
dhs_iwi <- read.csv("./data/interim/dhs_est_iwi.csv")

#lat/lon -34.463232 19.54247
safrica_r <- raster::brick("./data/dhs_tifs/south_africa_2016/00743.tif")
safrica_rgb <-  safrica_r[[c(1,2,3)]]
safrica_scaled <- safrica_rgb/.0001
raster::plotRGB(safrica_scaled,r=3,g=2,b=1)
dev.off()
raster::plotRGB(safrica_scaled,r=3,g=2,b=1,stretch="lin")
dev.off()
raster::plotRGB(safrica_scaled,r=3,g=2,b=1,stretch="hist")

#lat/lon -34.418873 19.18893
safrica2_r <- raster::brick("./data/dhs_tifs/south_africa_2016/00694.tif")
safrica2_rgb <-  safrica2_r[[c(1,2,3)]]
safrica2_scaled <- safrica2_rgb/.0001
raster::plotRGB(safrica2_scaled,r=3,g=2,b=1)
dev.off()
raster::plotRGB(safrica2_scaled,r=3,g=2,b=1,stretch="lin")
dev.off()
raster::plotRGB(safrica2_scaled,r=3,g=2,b=1,stretch="hist")

#Sierra Leone
sl_old_terra_r <- terra::rast("./data/dhs_tifs/sierra_leone_2008/00013.tif",
                                 lyrs=c("00013_1","00013_2","00013_3"))
sl_old_rgb <-  sl_old_terra_r[[c(1,2,3)]]
sl_old_scaled <- sl_old_rgb/.0001
dev.off()
raster::plotRGB(sl_old_scaled,r=3,g=2,b=1,stretch="hist")

sl_annual_terra_r <- terra::rast("./data/dhs_tifs_annual/sierra_leone_2008/00161.tif",
                                      lyrs=c("00161_1","00161_2","00161_3"))
terra::cellSize(sl_annual_terra_r)
sqrt(891.2482)
#year 2000
sl_annual_rgb <-  sl_annual_terra_r[[c(1,2,3)]]
sl_annual_scaled <- sl_annual_rgb/.0000275
dev.off()
raster::plotRGB(sl_annual_scaled,r=3,g=2,b=1,stretch="hist")


sl_3yr_terra_r <- terra::rast("./data/dhs_tifs_5k_3yr/sierra_leone_2008/00161.tif",
                                 lyrs=c("00161_1","00161_2","00161_3"))
terra::cellSize(sl_3yr_terra_r)
sqrt(891.2482)
#years 1999-2001
sl_3yr_rgb <-  sl_3yr_terra_r[[c(1,2,3)]]
sl_3yr_scaled <- sl_3yr_rgb/.0000275
dev.off()
raster::plotRGB(sl_3yr_scaled,r=3,g=2,b=1,stretch="hist")

#46422	Sierra_leone	SLE	8.40611	-10.67349	0	0	2013	./data/dhs_tifs_annual/sierra_leone_2008/00161.tif
#./data/dhs_tifs/sierra_leone_2008/00013.tif

sl_3yr_v2_terra_r <- terra::rast("./data/dhs_tifs_5k_3yr_v2/sierra_leone_2008/00161.tif",
                              lyrs=c("00161_1","00161_2","00161_3"))
terra::cellSize(sl_3yr_v2_terra_r)
sqrt(891.2482)
#years 1999-2001
sl_3yr_v2_rgb <-  sl_3yr_v2_terra_r[[c(1,2,3)]]
sl_3yr_v2_scaled <- sl_3yr_v2_rgb/.0000275
dev.off()
raster::plotRGB(sl_3yr_v2_scaled,r=3,g=2,b=1,stretch="hist")


comoros_5k_3yr_r <- raster::brick("./data/dhs_tifs_5k_3yr_v2/comoros_2012/00222.tif")
# class      : RasterBrick 
# dimensions : 167, 167, 27889, 36  (nrow, ncol, ncell, nlayers)
# resolution : 0.0002736804, 0.0002699141  (x, y)
# extent     : 43.36329, 43.40899, -11.51453, -11.46945  (xmin, xmax, ymin, ymax)
# crs        : +proj=longlat +datum=WGS84 +no_defs 
# source     : 00222.tif 
# names      : X00222_1, X00222_2, X00222_3, X00222_4, X00222_5, X00222_6, X00222_7, X00222_8, X00222_9, X00222_10, X00222_11, X00222_12, X00222_13, X00222_14, X00222_15, ... 


comoros_5k_3yr_terra_r <- terra::rast("./data/dhs_tifs_5k_3yr_v2/comoros_2012/00222.tif")
# class       : SpatRaster 
# dimensions  : 167, 167, 36  (nrow, ncol, nlyr)
# resolution  : 0.0002736804, 0.0002699141  (x, y)
# extent      : 43.36329, 43.40899, -11.51453, -11.46945  (xmin, xmax, ymin, ymax)
# coord. ref. : lon/lat WGS 84 (EPSG:4326) 
# source      : 00222.tif 
# names       : 00222_1, 00222_2, 00222_3, 00222_4, 00222_5, 00222_6, ... 

terra::cellSize(comoros_5k_3yr_terra_r)
# class       : SpatRaster 
# dimensions  : 167, 167, 1  (nrow, ncol, nlyr)
# resolution  : 0.0002736804, 0.0002699141  (x, y)
# extent      : 43.36329, 43.40899, -11.51453, -11.46945  (xmin, xmax, ymin, ymax)
# coord. ref. : lon/lat WGS 84 (EPSG:4326) 
# source(s)   : memory
# name        :     area 
# min value   : 891.4497 
# max value   : 891.5878 


#year 1999-2001
comoros_5k_3yr_rgb <-  comoros_5k_3yr_terra_r[[c(1,2,3)]]
comoros_5k_3yr_scaled <- ((comoros_5k_3yr_rgb /.0000275)+ 0.2)
dev.off()
raster::plotRGB(comoros_5k_3yr_scaled,r=3,g=2,b=1,stretch="hist")

#year 2002-2004
comoros_5k_3yr_rgb <-  comoros_5k_3yr_terra_r[[c(7,8,9)]]
comoros_5k_3yr_scaled <- ((comoros_5k_3yr_rgb /.0000275)+ 0.2)
dev.off()
raster::plotRGB(comoros_5k_3yr_scaled,r=3,g=2,b=1,stretch="hist")

#year 2005-2007
comoros_5k_3yr_rgb <-  comoros_5k_3yr_terra_r[[c(13,14,15)]]
comoros_5k_3yr_scaled <- ((comoros_5k_3yr_rgb /.0000275) + 0.2)
dev.off()
raster::plotRGB(comoros_5k_3yr_scaled,r=3,g=2,b=1,stretch="hist")


