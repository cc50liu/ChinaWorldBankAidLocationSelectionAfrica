import ee

class GeeExporter:

    NULL_TOKEN = 0
    DMSP_END = ee.Date('2013-12-31')
    VIIRS_START = ee.Date('2013-01-01')

    def __init__(self, filterpoly: ee.Geometry, start_year: int,
                 end_year: int, ms_bands: list,
                 include_nl: bool = True) -> None:
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
        self.include_nl = include_nl

        self.l7 = self.init_coll('LANDSAT/LE07/C02/T1_L2', self.start_date, self.end_date)
                
        self.merged = self.l7.sort('system:time_start')
        self.merged = self.merged.map(self.mask_qaclear).select(ms_bands)
        
        # Adds background to use for missing Landsat images
        background = ee.Image([self.NULL_TOKEN] * len(ms_bands)).rename(ms_bands)
        self.background = background.cast(dict(zip(ms_bands, ['float'] * len(ms_bands))))

        # Include both datasets used for nightlight images
        if include_nl:
            self.dmsp = self.init_coll('NOAA/DMSP-OLS/NIGHTTIME_LIGHTS', self.start_date,'2013-12-31')
            self.viirs = self.init_coll('NOAA/VIIRS/DNB/MONTHLY_V1/VCMCFG', '2013-01-01', self.end_date)


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

        is_nightlights = not name.startswith('LANDSAT')
        if is_nightlights:
            return img_col.select([0], ['NIGHTLIGHTS'])

        is_landsat_8 = name.startswith('LANDSAT/LC08')
        if is_landsat_8:
            return img_col.map(self.rename_l8).map(self.rescale_l8)
        else:
            return img_col.map(self.rename_l57).map(self.rescale_l57)

    @staticmethod
    def decode_qamask(img: ee.Image) -> ee.Image:
        """
        Decodes 'pixel_qa' band in Landsat image
        :param img: ee.Image, Landsat 5/7/8 image containing 'pixel_qa' band
        :return: ee.Image, contains 5 bands of masks
        Pixel QA Bit Flags (universal across Landsat 5/7/8)
        Bit  Attribute
        0    Fill
        1    Clear
        2    Water
        3    Cloud Shadow
        4    Snow
        5    Cloud
        """
        qa = img.select('pixel_qa')
        clear = qa.bitwiseAnd(2).neq(0)  # 0 = not clear, 1 = clear
        clear = clear.updateMask(clear).rename(['pxqa_clear'])

        water = qa.bitwiseAnd(4).neq(0)  # 0 = not water, 1 = water
        water = water.updateMask(water).rename(['pxqa_water'])

        cloud_shadow = qa.bitwiseAnd(8).eq(0)  # 0 = shadow, 1 = not shadow
        cloud_shadow = cloud_shadow.updateMask(cloud_shadow).rename(['pxqa_cloudshadow'])

        snow = qa.bitwiseAnd(16).eq(0)  # 0 = snow, 1 = not snow
        snow = snow.updateMask(snow).rename(['pxqa_snow'])

        cloud = qa.bitwiseAnd(32).eq(0)  # 0 = cloud, 1 = not cloud
        cloud = cloud.updateMask(cloud).rename(['pxqa_cloud'])

        masks = ee.Image.cat([clear, water, cloud_shadow, snow, cloud])
        return masks

    @staticmethod
    def mask_qaclear(img: ee.Image) -> ee.Image:
        """
        Masks out unusable pixels
        :param img: ee.Image, Landsat 5/7/8 image containing 'pixel_qa' band
        :return: ee.Image, input image with cloud-shadow, snow, cloud, and unclear
            pixels masked out
        """
        qam = GeeExporter.decode_qamask(img)
        cloudshadow_mask = qam.select('pxqa_cloudshadow')
        snow_mask = qam.select('pxqa_snow')
        cloud_mask = qam.select('pxqa_cloud')
        return img.updateMask(cloudshadow_mask).updateMask(snow_mask).updateMask(cloud_mask)

    @staticmethod
    def rename_l57(img: ee.Image) -> ee.Image:
        """
        Renames bands for a Landsat 5/7 image
        :param img: ee.Image, Landsat 5/7 image
        :return: ee.Image, with bands renamed
        See: https://developers.google.com/earth-engine/datasets/catalog/LANDSAT_LE07_C01_T1_L2
        Name            Scale Factor Description
        SR_B1            2.75e-05     Band 1 (blue) surface reflectance
        SR_B2            2.75e-05     Band 2 (green) surface reflectance
        SR_B3            2.75e-05     Band 3 (red) surface reflectance
        SR_B4            2.75e-05     Band 4 (near infrared) surface reflectance
        SR_B5            2.75e-05     Band 5 (shortwave infrared 1) surface reflectance
        SR_B7            2.75e-05     Band 7 (shortwave infrared 2) surface reflectance
        SR_ATMOS_OPACITY 0.001        A general interpretation of atmospheric opacity 
        SR_CLOUD_QA                   Cloud Quality Assessment
        ST_B6            0.00341802   Band 6 surface temperature. 
        ST_ATRAN         0.0001       Atmospheric Transmittance. 
        ST_CDIST         0.01         Pixel distance to cloud. If 'PROCESSING_LEVEL' is set to 'L2SR', this band is fully masked out.
        ST_DRAD          0.001        Downwelled Radiance. 
        ST_EMIS          0.0001       Emissivity estimated from ASTER GED.
        ST_EMSD          0.0001       Emissivity standard deviation.
        ST_QA            0.01         Uncertainty of the Surface Temperature band.
        ST_TRAD          0.001        Thermal band converted to radiance.
        ST_URAD          0.001        Upwelled Radiance. 
        QA_PIXEL                      Pixel quality attributes generated from the CFMASK algorithm.
        QA_RADSAT

        """
        newnames = ['BLUE', 'GREEN', 'RED', 'NIR', 'SWIR1', 'SWIR2','SR_ATMOS_OPACITY',
                    'SR_CLOUD_QA', 'ST_B6', 'ST_ATRAN', 'ST_CDIST', 'ST_DRAD', 'ST_EMIS', 
                    'ST_EMSD', 'ST_QA', 'ST_TRAD', 'ST_URAD', 'pixel_qa', 'QA_RADSAT']
        return img.rename(newnames)

    @staticmethod
    def rescale_l57(img: ee.Image) -> ee.Image:
        """
        Rescales Landsat 5/7 image to common scale
        :param img: ee.Image, Landsat 5/7 image, with bands already renamed
            by rename_l57()
        :return: ee.Image, with bands rescaled
        """
        opt = img.select(['BLUE', 'GREEN', 'RED', 'NIR', 'SWIR1', 'SWIR2'])
        mask  = img.select(['pixel_qa'])
        #scale only the bands we will use and return
        opt = opt.multiply(0.0000275)
 
        # include only the bands we will use
        scaled = ee.Image.cat([opt, mask]).copyProperties(img)
        
        scaled = scaled.set('system:time_start', img.get('system:time_start'))
        return scaled

    def get_timeseries_image(self, span_length):
        """
        Produce a sequential collection where each image represents a non-overlapping time period.
        The time series starts at 'start_year', ends at 'end_year' and each image is a composite of
        'span_length' number of years.
        :param start_year: int, earliest year to include in the time series
        :param end_year: int, last possible year to include in the time series.
            Can be left out depending on the 'span_length'.
        :param span_length: int, number of years for each
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
            
            # Explicilty cast mapped value as list. Extract start and end date
            span = ee.List(span)
            start_date = ee.Date.fromYMD(span.get(0), 1, 1)
            end_date = ee.Date.fromYMD(span.get(1), 12, 31)
            
            # Get time span median values for multispectral bands
            img = self.merged.filterDate(start_date, end_date).median()

            # Add background values for pixel locations without any images
            img = ee.ImageCollection([self.background, img]).mosaic()

            # Add nightlight band to image representation
            if self.include_nl:
                img = img.addBands(self.composite_nl(start_date, end_date))

            # Clip image to remove unnecessary regions
            img = img.clip(self.filterpoly)

            return img.set('system:time_start', start_date.millis())

        # Create one image per time span
        span_images = ee.ImageCollection.fromImages(spans.map(get_span_image))

        # Converts collection of span_images to a single multi-band image 
        # containing all of the bands of every span_image in the collection
        out_image = span_images.toBands()

        return out_image


    def composite_nl(self, start_date: ee.Date, end_date: ee.Date) -> ee.Image:
        """
        Creates a median-composite nightlights (NL) image.
        :param start_date: ee.Date, ee.Date representation of start date
        :param end_date: ee.Date, ee.Date representation of end date
        :return: ee.Image, containing a single band named 'NIGHTLIGHTS'
        """

        # For this study, always use DMSP
        nl_satellite = self.dmsp
       
        # Explicity cast to ee.ImageCollection
        nl_satellite = ee.ImageCollection(nl_satellite)

        # Return as median image
        return nl_satellite.filterDate(start_date, end_date).median()
