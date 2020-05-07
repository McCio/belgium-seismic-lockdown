import time

import requests_cache

from settings import CACHE_FOLDER
from fs_cache import FSCache

__all__ = [
    'SESSION',
]


def make_throttle_hook(timeout=1.0):
    """
    Returns a response hook function which sleeps for `timeout` seconds if
    response is not cached
    """

    def hook(response, *args, **kwargs):
        if not getattr(response, 'from_cache', False):
            # print(f"Got {response.url}")
            time.sleep(timeout)
        return response

    return hook


SESSION = requests_cache.CachedSession('.cache', backend=FSCache(location=CACHE_FOLDER))
SESSION.hooks = {'response': make_throttle_hook(0.1)}
SESSION.headers['User-Agent'] = 'Mozilla/5.0 (X11; Linux x86_64)' \
                                ' AppleWebKit/537.36 (KHTML, like Gecko)' \
                                ' Chrome/80.0.3987.163 Safari/537.36'
