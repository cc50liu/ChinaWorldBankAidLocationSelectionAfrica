import ee

class GeeExporter:

    NULL_TOKEN = 0

    def __init__(self, filterpoly: ee.Geometry, start_year: int,
                 end_year: int, ms_bands: list,
                 include_nl: bool = False) -> None:
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

        self.l8 = self.init_coll('LANDSAT/LC08/C02/T1_L2', self.start_date, self.end_date)
        self.l7 = self.init_coll('LANDSAT/LE07/C02/T1_L2', self.start_date, self.end_date)
        self.l5 = self.init_coll('LANDSAT/LT05/C02/T1_L2', self.start_date, self.end_date)

        self.merged = self.l5.merge(self.l7).merge(self.l8).sort('system:time_start')
        self.merged = self.merged.select(ms_bands)

        # Adds background to use for missing Landsat images
        background = ee.Image([self.NULL_TOKEN] * len(ms_bands)).rename(ms_bands)
        self.background = background.cast(dict(zip(ms_bands, ['float'] * len(ms_bands))))

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
            return img_col.map(self.rename_l8).map(self.rescale).map(self.mask_qaclear8)
        else:
            return img_col.map(self.rename_l57).map(self.rescale).map(self.mask_qaclear57)

    @staticmethod
    def mask_qaclear57(img: ee.Image) -> ee.Image:
        """
        Masks out unusable pixels
        :param img: ee.Image, Landsat 5/7/8 image containing 'SR_CLOUD_QA' band
        :return: ee.Image, input image with cloud-shadow, snow, cloud, and unclear
            pixels masked out

        Bitmask for SR_CLOUD_QA

        Bit 0: Dark Dense Vegetation (DDV)
        Bit 1: Cloud
        Bit 2: Cloud Shadow
        Bit 3: Adjacent to Cloud
        Bit 4: Snow
        Bit 5: Water 

        """        
        # Get the SR_CLOUD_QA band
        qa = img.select('SR_CLOUD_QA')

        # Create masks for clouds, cloud shadows, and snow
        cloud_mask = qa.bitwiseAnd(1 << 1)
        cloud_shadow_mask = qa.bitwiseAnd(1 << 2)
        snow_mask = qa.bitwiseAnd(1 << 4)

        # Combine masks
        mask = cloud_mask.Or(cloud_shadow_mask).Or(snow_mask)

        # Return the image with masked pixels
        return img.updateMask(mask.Not())        

    @staticmethod
    def mask_qaclear8(img: ee.Image) -> ee.Image:
        """
        Masks out unusable pixels
        :param img: ee.Image, Landsat 5/7/8 image containing 'QA_PIXEL' band
        :return: ee.Image, input image with cloud-shadow, snow, cloud, and unclear
            pixels masked out
            
        Bitmask for QA_PIXEL (which I renamed to SR_CLOUD_QA for processing here)
        Bit 0: Fill
        Bit 1: Dilated Cloud
        Bit 2: Cirrus (L8) 
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
        qa = img.select('SR_CLOUD_QA')

        # Create masks for clouds, cloud shadows, and snow
        cloud_mask = qa.bitwiseAnd(1 << 3).Or(qa.bitwiseAnd(1 << 1)).Or(qa.bitwiseAnd(1 << 2))
        cloud_shadow_mask = qa.bitwiseAnd(1 << 4)
        snow_mask = qa.bitwiseAnd(1 << 5)

        # Combine masks
        mask = cloud_mask.Or(cloud_shadow_mask).Or(snow_mask)

        # Return the image with masked pixels
        return img.updateMask(mask.Not())           

    @staticmethod
    def rename_l8(img: ee.Image) -> ee.Image:
        """
        Renames bands for a Landsat 8 image
        :param img: ee.Image, Landsat 8 image
        :return: ee.Image, with bands renamed
        See: https://developers.google.com/earth-engine/datasets/catalog/LANDSAT_LC08_C02_T1_L2
        Name       Scale Factor Description
        SR_B1            2.75e-05     Band 1 (ultra blue, coastal aerosol) surface reflectance
        SR_B2            2.75e-05     Band 2 (blue) surface reflectance
        SR_B3            2.75e-05     Band 3 (green) surface reflectance 
        SR_B4            2.75e-05     Band 4 (red) surface reflectance 
        SR_B5            2.75e-05     Band 5 (near infrared) surface reflectance  
        SR_B6            2.75e-05     Band 6 (shortwave infrared 1) surface reflectance  
        SR_B7            2.75e-05     Band 7 (shortwave infrared 2) surface reflectance   
        several more here - included in newnames as their standard values         
        
        Note: I am renaming QA_PIXEL to SR_CLOUD_QA for consistency with L57
        """
        
        newnames = ['SR_B1', 'BLUE', 'GREEN', 'RED', 'NIR', 'SWIR1', 'SWIR2',
                    'SR_QA_AEROSOL', 'ST_B10', 'ST_ATRAN', 'ST_CDIST', 'ST_DRAD', 'ST_EMIS', 
                    'ST_EMSD', 'ST_QA', 'ST_TRAD', 'ST_URAD', 'SR_CLOUD_QA', 'QA_RADSAT']
        return img.rename(newnames)

    @staticmethod
    def rename_l57(img: ee.Image) -> ee.Image:
        """
        Renames bands for a Landsat 5/7 image
        :param img: ee.Image, Landsat 5/7 image
        :return: ee.Image, with bands renamed
        See: https://developers.google.com/earth-engine/datasets/catalog/LANDSAT_LT05_C02_T1_SR
             https://developers.google.com/earth-engine/datasets/catalog/LANDSAT_LE07_C02_T1_L2
        Name            Scale Factor Description
        SR_B1            2.75e-05     Band 1 (blue) surface reflectance
        SR_B2            2.75e-05     Band 2 (green) surface reflectance
        SR_B3            2.75e-05     Band 3 (red) surface reflectance
        SR_B4            2.75e-05     Band 4 (near infrared) surface reflectance
        SR_B5            2.75e-05     Band 5 (shortwave infrared 1) surface reflectance                                                                                     
        SR_B7            2.75e-05     Band 7 (shortwave infrared 2) surface reflectance
        several more here - included in newnames as their standard values 
        """
        newnames = ['BLUE', 'GREEN', 'RED', 'NIR', 'SWIR1', 'SWIR2',
                    'SR_ATMOS_OPACITY', 'SR_CLOUD_QA', 'ST_B6', 'ST_ATRAN', 'ST_CDIST', 
                    'ST_DRAD', 'ST_EMIS', 'ST_EMSD', 'ST_QA', 'ST_TRAD', 'ST_URAD', 
                    'QA_PIXEL', 'QA_RADSAT']
        return img.rename(newnames)

    @staticmethod
    def rescale(img: ee.Image) -> ee.Image:
        """
        Rescales Landsat 5, 7, or 8 image to common scale
        :param img: ee.Image, Landsat 5/7/8 image, with bands already renamed
            by rename_l57() or rename_l8()
        :return: ee.Image, optical and qa bands only, with bands rescaled
        """
        opt = img.select(['BLUE', 'GREEN', 'RED', 'NIR', 'SWIR1', 'SWIR2'])
        masks = img.select(['SR_CLOUD_QA'])

        opt = opt.multiply(0.0000275).add(-0.2)

        scaled = ee.Image.cat([opt, masks]).copyProperties(img)
        # system properties are not copied
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
            
            # Excplicilty cast mapped value as list. Extract start and end date
            span = ee.List(span)
            start_date = ee.Date.fromYMD(span.get(0), 1, 1)
            end_date = ee.Date.fromYMD(span.get(1), 12, 31)
            
            # Get time span median values for multispectral bands
            img = self.merged.filterDate(start_date, end_date).median()

            # Add background values for pixel locations without any images
            img = ee.ImageCollection([self.background, img]).mosaic()

            # Clip image to remove unnecessary regions
            img = img.clip(self.filterpoly)

            return img.set('system:time_start', start_date.millis())

        # Create one image per time span
        span_images = ee.ImageCollection.fromImages(spans.map(get_span_image))

        # Converts collection of span_images to a single multi-band image 
        # containing all of the bands of every span_image in the collection
        out_image = span_images.toBands()

        return out_image

