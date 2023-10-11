import os
import shutil
import requests
import ee
import multiprocessing
import logging
import pandas as pd

from retry import retry
from functools import partial

from gee_exporter_collection2 import GeeExporter

DEFAULT_MS_BANDS = ['BLUE', 'GREEN', 'RED', 'NIR', 'SWIR1', 'SWIR2']

logging.basicConfig()

def export_images(survey_df: pd.DataFrame,
                  save_dir: str,
                  ms_bands: list = None,
                  include_nl: bool = False,
                  scale: int = 30,
                  export_tile_diameter: int = 167,  # image dimensions = 167px * 167px ~ 5km2
                  start_year: int = 1999,
                  end_year: int = 2016,
                  span_length: int = 3
                  ):
    """
    Downloads a .tif-file for each geographical location in a survey depicting a 
    time-series of images of the surronding area centered at the location.
    :param survey_df: pd.DataFrame, contains columns ['lat', 'lon', 'country', 'year']
    :param save_dir: str, path to directory for storing .tif-files
    :param ms_bands: list, the multispectral bands exported. Defaults to
        DEFAULT_MS_BANDS = ['BLUE', 'GREEN', 'RED', 'NIR', 'SWIR1', 'SWIR2']
    :param include_nl: bool, include nightlight images in the export
    :param scale: int, pixel scale in meters
    :param export_tile_diameter: int, side length of image in pixels
    :param start_year: int, start of the time series
    :param end_year: int, end of the time series
    :param span_length: int, the step size in years for the time series
    """
    if ms_bands is None:
        ms_bands = DEFAULT_MS_BANDS

    # Get bounding box covering all points in survey
    survey_df['geometry'] = survey_df.apply(lambda row: ee.Geometry.Point([row['lon'], row['lat']]), axis=1)
    country_roi = ee.Geometry.MultiPoint(list(survey_df['geometry']))

    # Pad bounding box to ensure that the country image covers the full clusters on the edge of the box
    buffer_dist = scale * export_tile_diameter / 2 # scale in m/px * diameter in px = buffer in m
    country_bbox = country_roi.buffer(buffer_dist).bounds()

    # Create an image covering the entire country which we can sample patches from
    gee_exporter = GeeExporter(filterpoly=country_bbox, start_year=start_year, end_year=end_year, 
                             ms_bands=ms_bands, include_nl=include_nl)

    # Create image covering entire country
    country_img = gee_exporter.get_timeseries_image(span_length)

    # See https://docs.python.org/3/library/functools.html#functools.partial
    download_sample_img_x = partial(download_sample_img, country_img=country_img, save_dir=save_dir, 
                             buffer_dist=buffer_dist, tile_diameter=export_tile_diameter)

    # Get samples as list, since multiprocessing doesn't work with dataframes
    survey_clusters = [row for _, row in survey_df.iterrows()]

    pool = multiprocessing.Pool(15)
    pool.map(download_sample_img_x, survey_clusters)
    pool.close()
    pool.join()


@retry(tries=10, delay=2, backoff=2)
def download_sample_img(sample_row, country_img, save_dir, buffer_dist, tile_diameter):
    """
    Samples and downloads a .tif-file from the country image for the given location.
    :param sample_row: pd.Series, representation of the sample location
    :param country_img: ee.Image, the image covering all points in the survey from 
    which the sample is cropped.
    :param save_dir: str, path to directory for storing .tif-file
    :param buffer_dist: int, pixel scale in meters
    :param tile_diameter: int, side length of image in pixels
    :param start_year: int, start of the time series
    :param end_year: int, end of the time series
    :param span_length: int, the step size in years for the time series
    :return: dict, maps task name tuple (export_folder, country, year, chunk) to ee.batch.Task
    """

    index = sample_row.name

    # Generate the desired image from the given point.
    point = sample_row['geometry']
    sample_region = point.buffer(buffer_dist).bounds()
    sample_img = country_img.clip(sample_region)

    # Fetch the URL from which to download the image.

    url = sample_img.getDownloadURL({
        'region': sample_region,
        'dimensions': f'{tile_diameter}x{tile_diameter}',
        'filePerBand': False,
        'format': 'GEO_TIFF'
    })

    # Handle downloading the actual pixels.
    r = requests.get(url, stream=True)
    if r.status_code != 200:
        raise r.raise_for_status()

    file_name = f'{index:05d}.tif'
    file_path = os.path.join(save_dir, file_name)
    with open(file_path, 'wb') as out_file:
        shutil.copyfileobj(r.raw, out_file)

