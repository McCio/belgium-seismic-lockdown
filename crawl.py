import datetime
import os
import pytz

from settings import START_DATE, CSV_FOLDER, LAST_DAY_FILE
from requests_session import SESSION

TZ_BELGIAN = pytz.timezone('Europe/Brussels')
TZ_UTC = pytz.utc
TODAY = datetime.date.today()


def get_csv_url_for(station_code: str, day: datetime.date):
    year = day.year
    year_start = datetime.date(year=year, month=1, day=1)
    day_delta = day - year_start
    padded_day_of_year = f"{day_delta.days + 1:0>3}"
    return f"http://seismologie.be/data/csv/SEIS-{year}-{padded_day_of_year}-{station_code}.csv"


def get_csv_for(station_code: str, day: datetime.date):
    csv_url = get_csv_url_for(station_code, day)
    with SESSION.get(csv_url) as f:
        if f.ok:
            return f.content.decode().split()
        return []


def iter_data_for(station_code: str, day: datetime.date):
    day_data = get_csv_for(station_code, day)
    if len(day_data) > 0:
        utc_second = TZ_UTC.localize(datetime.datetime(year=day.year, month=day.month, day=day.day))
        # belgian_second = TZ_BELGIAN.normalize(utc_second)
        second = datetime.timedelta(seconds=1)
        for row in day_data[1:]:
            data = [utc_second.isoformat()]  # , belgian_second.isoformat()]
            data.extend(row.split(','))
            yield data
            utc_second += second
            # belgian_second += second


def get_events_url_on(day: datetime.date):
    return f"http://seismologie.be/data/csv/events_{day.isoformat()}.json"


def get_events_on(day: datetime.date):
    json_url = get_events_url_on(day)
    with SESSION.get(json_url) as f:
        if f.ok:
            return f.json()
        return {}


def save_month(station_code: str, month: datetime.date, force: bool = False):
    day = month.replace(day=1)
    filename = f"{station_code}-{day:%Y-%m}.csv"
    path = os.path.join(CSV_FOLDER, filename)
    if os.path.exists(path) and not force:
        while day.month == month.month and day < TODAY:
            day += datetime.timedelta(days=1)
        return day
    print(f"Loading {day:%Y-%m}", end='... ')
    try:
        with open(path, 'w') as f:
            # f.write(f"utc_second,belgian_second,min,max\n")
            f.write(f"utc_second,min,max\n")
            while day.month == month.month and day < TODAY:
                for row in iter_data_for(station_code, day):
                    f.write(','.join(row))
                    f.write('\n')
                get_events_on(day)
                day += datetime.timedelta(days=1)
    except (InterruptedError, KeyboardInterrupt) as _:
        print('ERROR')
        os.remove(path)
        exit(42)
    except BaseException as e:
        print('ERROR (' + type(e).__name__ + ')')
        os.remove(path)
    else:
        print('OK')
    return day


def del_future_cache(station: str, from_day: datetime.date):
    del_cache = from_day
    print(f"Deleting from cache from {from_day.isoformat()} to 31 days after", end='... ')
    while del_cache - from_day < datetime.timedelta(days=31):
        SESSION.cache.delete_url(get_events_url_on(del_cache))
        SESSION.cache.delete_url(get_csv_url_for(station, del_cache))
        del_cache += datetime.timedelta(days=1)
    print("OK")


def download_station(station: str):
    print(" "*36 + f"[ {station} ]")
    os.makedirs(CSV_FOLDER, exist_ok=True)
    last_day = datetime.date.today()
    last_day_file = os.path.join(CSV_FOLDER, station + '-' + LAST_DAY_FILE)
    if os.path.exists(last_day_file):
        with open(last_day_file) as ld:
            try:
                r = datetime.date.fromisoformat(ld.read())
            except Exception as e:
                raise e
            else:
                last_day = r
    del_future_cache(station, last_day)
    current_day = datetime.date(**START_DATE)
    while current_day < TODAY:
        current_day = save_month(station, current_day, force=(current_day.month == last_day.month
                                                              and current_day.year == last_day.year))
    with open(last_day_file, 'w') as ld:
        ld.write(current_day.isoformat())


if __name__ == '__main__':
    download_station('UCCS')
    download_station('MEMS')
    download_station('OSTB')
