import ee

class GeeExporter:

                  

    def __init__(self, filterpoly: ee.Geometry, start_year: int,
                 end_year: int, ms_bands: list) -> None:
        """
        Class for handling Landsat data in GEE
        :param filterpoly: ee.Geometry
        :param start_date: str, string representation of start date
        :param end_date:  str, string representation of end date
        :param ms_bands: list of multispectral bands to keep from the collections

        """
        self.filterpoly = filterpoly
        self.start_year = start_year
        self.end_year = end_year
        self.start_date = f'{start_year}-01-01'
        self.end_date = f'{end_year}-12-31'

        self.l8 = self.init_coll('LANDSAT/LC08/C02/T1_L2', self.start_date, self.end_date) # 30 x 30 m/px
        self.l7 = self.init_coll('LANDSAT/LE07/C02/T1_L2', self.start_date, self.end_date) # 30 x 30 m/px
        self.l5 = self.init_coll('LANDSAT/LT05/C02/T1_L2', self.start_date, self.end_date) # 30 x 30 m/px

        self.merged = self.l5.merge(self.l7).merge(self.l8).sort('system:time_start')
        self.merged = self.merged.map(self.mask_qaclear).select(ms_bands)

    def init_coll(self, name: str, start_date: str, end_date: str) -> ee.ImageCollection:
        """
        Creates a standardised ee.ImageCollection containing images of desired points
        between the desired start and end dates.
        :param name: str, name of collection
        :param start_date: str, string representation of start date
        :param end_date: str, string representation of end date
        :return: ee.ImageCollection
        """
        img_col = ee.ImageCollection(name).filterBounds(self.filterpoly).filterDate(start_date, end_date)

        is_landsat_8 = name.startswith('LANDSAT/LC08')
        if is_landsat_8:
            return img_col.map(self.rename_l8).map(self.rescale_l8)
        else:
            return img_col.map(self.rename_l57).map(self.rescale_l57)

    @staticmethod
    def decode_qamask(img: ee.Image) -> ee.Image:
        """
        Decodes 'QA_PIXEL' band in Landsat image
        :param img: ee.Image, Landsat 5/7/8 image containing 'QA_PIXEL' band
        :return: ee.Image, contains 5 bands of masks
        Pixel QA Bit Flags (universal across Landsat 5/7/8)
        Bit  Attribute
                            
        0    Fill
        1    Dilated Cloud
        2    Cirrus (Only used by Landsat 8)
        3    Cloud
        4    Cloud Shadow
        5    Snow
        6    Clear
                                                  
                                                       
        7    Water
                                                           
        """
                               
        qa = img.select('QA_PIXEL')
        
        dilated_cloud = qa.bitwiseAnd(2).neq(0)  # 0 = not dilated cloud, 1 = dilated cloud
        dilated_cloud = dilated_cloud.updateMask(dilated_cloud).rename(['pxqa_dilated_cloud'])

                                                          
        cloud = qa.bitwiseAnd(8).eq(0)  # 0 = cloud, 1 = not cloud
        cloud = cloud.updateMask(cloud).rename(['pxqa_cloud'])
                                         

        cloud_shadow = qa.bitwiseAnd(16).eq(0)  # 0 = shadow, 1 = not shadow
        cloud_shadow = cloud_shadow.updateMask(cloud_shadow).rename(['pxqa_cloudshadow'])

        snow = qa.bitwiseAnd(32).eq(0)  # 0 = snow, 1 = not snow
        snow = snow.updateMask(snow).rename(['pxqa_snow'])
        
        clear = qa.bitwiseAnd(64).neq(0)  # 0 = not clear, 1 = clear
        clear = clear.updateMask(clear).rename(['pxqa_clear'])

        water = qa.bitwiseAnd(128).neq(0)  # 0 = not water, 1 = water
        water = water.updateMask(water).rename(['pxqa_water'])

        masks = ee.Image.cat([dilated_cloud, cloud, cloud_shadow, snow, clear, water])
        return masks

    @staticmethod
    def mask_qaclear(img: ee.Image) -> ee.Image:
        """
        Masks out unusable pixels
        :param img: ee.Image, Landsat 5/7/8 image containing 'QA_PIXEL' band
        :return: ee.Image, input image with dilated cloud, cloud-shadow, snow, and cloud
            pixels masked out
        """
        qam = GeeExporter.decode_qamask(img)
        dilated_cloud_mask = qam.select('pxqa_dilated_cloud')
        cloudshadow_mask = qam.select('pxqa_cloudshadow')
        snow_mask = qam.select('pxqa_snow')
        cloud_mask = qam.select('pxqa_cloud')
        return img.updateMask(dilated_cloud_mask).updateMask(cloudshadow_mask).updateMask(snow_mask).updateMask(cloud_mask)

    @staticmethod
    def rename_l8(img: ee.Image) -> ee.Image:
        """
        Renames bands for a Landsat 8 image
        :param img: ee.Image, Landsat 8 image
        :return: ee.Image, with bands renamed
        See: https://developers.google.com/earth-engine/datasets/catalog/LANDSAT_LC08_C02_T1_L2
        Name          Scale      Offset Description
        SR_B1         2.75e-05   -0.2   Band 1 (Ultra Blue) surface reflectance, 0.435-0.451 um
        SR_B2         2.75e-05   -0.2   Band 2 (Blue) surface reflectance, 0.452-0.512 um
        SR_B3         2.75e-05   -0.2   Band 3 (Green) surface reflectance, 0.533-0.590 um
        SR_B4         2.75e-05   -0.2   Band 4 (Red) surface reflectance, 0.636-0.673 um
        SR_B5         2.75e-05   -0.2   Band 5 (Near Infrared) surface reflectance, 0.851-0.879 um
        SR_B6         2.75e-05   -0.2   Band 6 (Shortwave Infrared 1) surface reflectance, 1.566-1.651 um
        SR_B7         2.75e-05   -0.2   Band 7 (Shortwave Infrared 2) surface reflectance, 2.107-2.294 um
        SR_QA_AEROSOL                   Aerosol attributes, see Aerosol QA table
        ST_B10        0.00341802 149    Band 10 brightness temperature (Kelvin), 10.60-11.19 um
        ST_ATRAN      0.0001            Atmospheric Transmittance.
        ST_CDIST      0.01              Pixel distance to cloud.
        ST_DRAD       0.001             Downwelled Radiance.
        ST_EMIS       0.0001            Emissivity estimated from ASTER GED.
        ST_EMSD       0.0001            Emissivity standard deviation.
        ST_QA         0.01              Uncertainty of the Surface Temperature band.
        ST_TRAD       0.001             Thermal band converted to radiance.
        ST_URAD       0.001             Upwelled Radiance.
        QA_PIXEL                        Pixel quality attributes generated from the CFMASK algorithm,
                                            see Pixel QA table
        QA_RADSAT                       Radiometric saturation QA, see Radiometric Saturation QA table
        """

        newnames = ['AEROS', 'BLUE', 'GREEN', 'RED', 'NIR', 'SWIR1', 'SWIR2', 'SR_QA_AEROSOL', 'TEMP1', 
                    'ST_ATRAN', 'ST_CDIST', 'ST_DRAD', 'ST_EMIS', 'ST_EMSD', 'ST_QA', 'ST_TRAD', 
                    'ST_URAD', 'QA_PIXEL', 'QA_RADSAT']
        return img.rename(newnames)

    @staticmethod
    def rescale_l8(img: ee.Image) -> ee.Image:
        """
        Rescales Landsat 8 image to common scale
        :param img: ee.Image, Landsat 8 image, with bands already renamed
            by rename_l8()
        :return: ee.Image, with bands rescaled
        """
        opt = img.select(['AEROS', 'BLUE', 'GREEN', 'RED', 'NIR', 'SWIR1', 'SWIR2'])
        therm = img.select(['TEMP1'])
        atmos_tran = img.select(['ST_ATRAN'])
        cdist = img.select(['ST_CDIST'])
        radiance = img.select(['ST_DRAD', 'ST_TRAD', 'ST_URAD'])
        emissivity = img.select(['ST_EMIS', 'ST_EMSD'])
        st_qa = img.select(['ST_QA'])
        masks = img.select(['SR_QA_AEROSOL', 'QA_PIXEL', 'QA_RADSAT'])

        opt = opt.multiply(0.0000275).add(-0.2)
        therm = therm.multiply(0.00341802).add(149)
        atmos_tran = atmos_tran.multiply(0.0001)
        cdist = cdist.multiply(0.01)
        radiance = radiance.multiply(0.001)
        emissivity = emissivity.multiply(0.0001)
        st_qa = st_qa.multiply(0.01)

        scaled = ee.Image.cat([opt, therm, atmos_tran, cdist, radiance, emissivity, st_qa, masks]).copyProperties(img)
        # system properties are not copied
        scaled = scaled.set('system:time_start', img.get('system:time_start'))
        return scaled
    
    @staticmethod
    def rename_l57(img: ee.Image) -> ee.Image:
        """
        Renames bands for a Landsat 5/7 image
        :param img: ee.Image, Landsat 5/7 image
        :return: ee.Image, with bands renamed
        See: https://developers.google.com/earth-engine/datasets/catalog/LANDSAT_LT05_C02_T1_L2
             https://developers.google.com/earth-engine/datasets/catalog/LANDSAT_LE07_C02_T1_L2
        Name             Scale  Offset Description
        SR_B1            0.0001 -0.2   Band 1 (blue) surface reflectance, 0.45-0.52 um
        SR_B2            0.0001 -0.2   Band 2 (green) surface reflectance, 0.52-0.60 um
        SR_B3            0.0001 -0.2   Band 3 (red) surface reflectance, 0.63-0.69 um
        SR_B4            0.0001 -0.2   Band 4 (near infrared) surface reflectance, 0.77-0.90 um
        SR_B5            0.0001 -0.2   Band 5 (shortwave infrared 1) surface reflectance, 1.55-1.75 um
        SR_B7            0.0001 -0.2   Band 7 (shortwave infrared 2) surface reflectance, 2.08-2.35 um
        SR_ATMOS_OPACITY 0.001         Atmospheric opacity; < 0.1 = clear; 0.1 - 0.3 = average; > 0.3 = hazy
        SR_CLOUD_QA                    Cloud quality attributes, see SR Cloud QA table. Note:
                                           pixel_qa is likely to present more accurate results
                                           than sr_cloud_qa for cloud masking. See page 14 in
                                           the LEDAPS product guide.
        ST_B6            0.1    149    Band 6 brightness temperature (Kelvin), 10.40-12.50 um
        ST_ATRAN         0.0001        Atmospheric Transmittance.
        ST_CDIST         0.01          Pixel distance to cloud.
        ST_DRAD          0.001         Downwelled Radiance.
        ST_EMIS          0.0001        Emissivity estimated from ASTER GED.
        ST_EMSD          0.0001        Emissivity standard deviation.
        ST_QA            0.01          Uncertainty of the Surface Temperature band.
        ST_TRAD          0.001         Thermal band converted to radiance.
        ST_URAD          0.001         Upwelled Radiance.
        QA_PIXEL                       Pixel quality attributes generated from the CFMASK algorithm,
                                           see Pixel QA table
        QA_RADSAT                      Radiometric saturation QA, see Radiometric Saturation QA table
        """
        newnames = ['BLUE', 'GREEN', 'RED', 'NIR', 'SWIR1', 'SWIR2', 'SR_ATMOS_OPACITY', 'SR_CLOUD_QA',
                                                                                        
                    'TEMP1', 'ST_ATRAN', 'ST_CDIST', 'ST_DRAD', 'ST_EMIS', 'ST_EMSD', 'ST_QA', 'ST_TRAD',
                    'ST_URAD', 'QA_PIXEL', 'QA_RADSAT']
        return img.rename(newnames)

    @staticmethod
    def rescale_l57(img: ee.Image) -> ee.Image:
        """
        Rescales Landsat 8 image to common scale
        :param img: ee.Image, Landsat 5/7 image, with bands already renamed
            by rename_l57()
        :return: ee.Image, with bands rescaled
        """
        opt = img.select(['BLUE', 'GREEN', 'RED', 'NIR', 'SWIR1', 'SWIR2'])
        atmos = img.select(['SR_ATMOS_OPACITY'])
        therm = img.select(['TEMP1'])
        atmos_tran = img.select(['ST_ATRAN'])
        cdist = img.select(['ST_CDIST'])
        radiance = img.select(['ST_DRAD', 'ST_TRAD', 'ST_URAD'])
        emissivity = img.select(['ST_EMIS', 'ST_EMSD'])
        st_qa = img.select(['ST_QA'])
        masks = img.select(['SR_CLOUD_QA', 'QA_PIXEL', 'QA_RADSAT'])

        opt = opt.multiply(0.0000275).add(-0.2)
        atmos = atmos.multiply(0.001)
        therm = therm.multiply(0.00341802).add(149)
        atmos_tran = atmos_tran.multiply(0.0001)
        cdist = cdist.multiply(0.01)
        radiance = radiance.multiply(0.001)
        emissivity = emissivity.multiply(0.0001)
        st_qa = st_qa.multiply(0.01)

        scaled = ee.Image.cat([opt, atmos, therm, atmos_tran, cdist, radiance, emissivity, st_qa, masks]).copyProperties(img)
        # system properties are not copied
        scaled = scaled.set('system:time_start', img.get('system:time_start'))
        return scaled

    def get_timeseries_image(self, span_length):
        """
        Produce a sequential collection where each image represents a non-overlapping time period.
        The time series starts at 'start_year', ends at 'end_year' and each image is a median 
        composite of 'span_length' number of years.
        The sequence will have a length of (end_year - start_year) // span_length.
        :param start_year: int, earliest year to include in the time series
        :param end_year: int, last possible year to include in the time series.
            Can be left out depending on the 'span_length'.
        :param span_length: int, number of years covered by a single frame
        :return: ee.ImageCollection, collection with a time series of images
        """

        # Create list of tuples containing start and end year for each timespan
        start_years = ee.List.sequence(self.start_year, self.end_year, 
                                       span_length)
        end_years = ee.List.sequence(self.start_year + span_length - 1, 
                                     self.end_year, span_length)
        spans = start_years.zip(end_years)

        # Define inner function
        def get_span_image(span: ee.List) -> ee.Image:
            """
            Get image with median band values between two dates. Created as an 
            inner function of 'get_images' since the GEE API doesn't allow maped 
            functions to access client side variables. This is a functional work-
            around.
            :param span: ee.List, tuple containing two integer values representing
            the start and end year of the timespan.
            :return: ee.Image, image representation for the given time period
            """
            
            # Excplicilty cast mapped value as list. Extract start and end date
            span = ee.List(span)
            start_date = ee.Date.fromYMD(span.get(0), 1, 1)
            end_date = ee.Date.fromYMD(span.get(1), 12, 31)
            
            # Get time span median values for multispectral bands
            img = self.merged.filterDate(start_date, end_date).median()

            # Clip image to remove unnecessary regions
            img = img.clip(self.filterpoly)

            return img.set('system:time_start', start_date.millis())

        # Create one image per time span
        span_image_collection = ee.ImageCollection.fromImages(spans.map(get_span_image))
                                                                          
        # Converts collection of span_images to a single multi-band image 
        # containing all of the bands of every span_image in the collection
        out_image = span_image_collection.toBands()

        return out_image


