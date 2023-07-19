import ee

class GeeExporter:

    NULL_TOKEN = 0
    DMSP_END = ee.Date('2013-12-31')

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

        if include_nl:
            self.dmsp = self.init_coll('NOAA/DMSP-OLS/NIGHTTIME_LIGHTS', self.start_date,'2013-12-31')

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

        return img_col.map(self.rescale_l7)

    @staticmethod
    def mask_qaclear(img: ee.Image) -> ee.Image:
        """
        Masks out unusable pixels
        :param img: ee.Image, LANDSAT/LE07/C02/T1_L2 image containing 'QA_PIXEL' band
        :return: ee.Image, input image with cloud-shadow, snow, cloud, and unclear
            pixels masked out
            
        Bitmask for QA_PIXEL
        Bit 0: Fill
        Bit 1: Dilated Cloud
        Bit 2: Unused
        Bit 3: Cloud
        Bit 4: Cloud Shadow
        Bit 5: Snow
        Bit 6: Clear
            0: Cloud or Dilated Cloud bits are set
            1: Cloud and Dilated Cloud bits are not set
        Bit 7: Water           
        Also has confidence bits, which I am not using here
        """        
        # Get the QA_PIXEL band
        qa = img.select('QA_PIXEL')

        # Create masks for clouds, cloud shadows, and snow
        cloud_mask = qa.bitwiseAnd(1 << 3).Or(qa.bitwiseAnd(1 << 1))
        cloud_shadow_mask = qa.bitwiseAnd(1 << 4)
        snow_mask = qa.bitwiseAnd(1 << 5)

        # Combine masks
        mask = cloud_mask.Or(cloud_shadow_mask).Or(snow_mask)

        # Return the image with masked pixels
        return img.updateMask(mask.Not())


    @staticmethod
    def rescale_l7(img: ee.Image) -> ee.Image:
        """
        Scales Landsat 7 image to common scale
        :param img: ee.Image, Landsat 7 image
        :return: ee.Image, with bands rescaled
        """
        opt = img.select(['SR_B1','SR_B2','SR_B3','SR_B4','SR_B5','SR_B7'])
        mask  = img.select(['QA_PIXEL'])
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
