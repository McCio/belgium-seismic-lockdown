import datetime
import os
import pytz

from settings import START_DATE, CSV_FOLDER, LAST_DAY_FILE
from requests_session import SESSION

TZ_BELGIAN = pytz.timezone('Europe/Brussels')
TZ_UTC = pytz.utc
TODAY = datetime.date.today()
DELTA_SECOND = datetime.timedelta(seconds=1)
DELTA_DAY = datetime.timedelta(days=1)


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
        for row in day_data[1:]:
            data = [utc_second.isoformat()]  # , belgian_second.isoformat()]
            data.extend(row.split(','))
            yield data
            utc_second += DELTA_SECOND
            # belgian_second += second


def get_events_url_on(day: datetime.date):
    return f"http://seismologie.be/data/csv/events_{day.isoformat()}.json"


def get_events_on(day: datetime.date):
    json_url = get_events_url_on(day)
    with SESSION.get(json_url) as f:
        if f.ok:
            return f.json()
        return {}


def get_month_file(station: str, month: datetime.date):
    filename = f"{station}-{month:%Y-%m}.csv"
    return os.path.join(CSV_FOLDER, filename)


def save_month(station_code: str, month: datetime.date, force: bool = False):
    day = month.replace(day=1)
    path = get_month_file(station_code, day)
    if os.path.exists(path) and not force:
        while day.month == month.month and day < TODAY:
            day += DELTA_DAY
        return day
    print(f"Loading {day:%Y-%m}", end='... ', flush=True)
    try:
        with open(path, 'w') as f:
            # f.write(f"utc_second,belgian_second,min,max\n")
            f.write(f"utc_second,min,max\n")
            while day.month == month.month and day < TODAY:
                for row in iter_data_for(station_code, day):
                    f.write(','.join(row))
                    f.write('\n')
                get_events_on(day)
                day += DELTA_DAY
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
        del_cache += DELTA_DAY
    print("OK")


def get_last_day_file(station: str):
    return os.path.join(CSV_FOLDER, station + '-' + LAST_DAY_FILE)


def load_last_day(station: str):
    last_day = datetime.date.today()
    last_day_file = get_last_day_file(station)
    if os.path.exists(last_day_file):
        with open(last_day_file) as ld:
            try:
                r = datetime.date.fromisoformat(ld.read())
            except Exception as e:
                raise e
            else:
                last_day = r
    return last_day


def save_last_day(station: str, last_day: datetime.date):
    last_day_file = get_last_day_file(station)
    with open(last_day_file, 'w') as ld:
        ld.write(last_day.isoformat())


def download_station(station: str):
    print(" "*36 + f"[ {station} ]")
    os.makedirs(CSV_FOLDER, exist_ok=True)
    last_day = load_last_day(station)
    del_future_cache(station, last_day)
    current_day = datetime.date(**START_DATE)
    while current_day < TODAY:
        current_day = save_month(station, current_day, force=(current_day.month == last_day.month
                                                              and current_day.year == last_day.year))
    save_last_day(station, current_day)


def get_whole_file(station: str):
    filename = f"{station}.csv"
    return os.path.join(CSV_FOLDER, filename)


def compress_station(station: str):
    print(" "*36 + f"[ {station} ]")
    last_day = load_last_day(station)
    current_day = datetime.date(**START_DATE)
    with open(get_whole_file(station), 'w') as whole:
        whole.write(f"min,max\n")
        while current_day < last_day:
            month_file = get_month_file(station, current_day)
            utc_second = TZ_UTC.localize(datetime.datetime(
                year=current_day.year,
                month=current_day.month,
                day=current_day.day
            ))  # type: datetime.datetime
            print(f"Compressing {current_day:%Y-%m}", end='... ', flush=True)
            with open(month_file) as m:
                for line in m:
                    parts = line.strip().split(',')
                    if len(parts) != 3:
                        print('ERROR')
                        print(f"Unknown meaning of line: {line}")
                        raise ValueError
                    if parts[0] == 'utc_second':
                        continue
                    linetime = datetime.datetime.fromisoformat(parts[0])
                    if linetime < utc_second:
                        print('ERROR')
                        print(f"Expected {utc_second.isoformat()} found {linetime.isoformat()}")
                        raise ValueError
                    else:  # linetime >= utc_second
                        while linetime > utc_second:
                            whole.write("NA,NA\n")
                            utc_second += DELTA_SECOND
                        # linetime == utc_second
                        whole.write(",".join(parts[1:]))
                        whole.write("\n")
                    linetime += DELTA_SECOND
                    utc_second += DELTA_SECOND
            while current_day.month == utc_second.month:
                if utc_second.date() < last_day:
                    whole.write("NA,NA\n")
                utc_second += DELTA_SECOND
            # utc_second has first second of next month
            current_day = utc_second.date()
            print('OK')


if __name__ == '__main__':
    # download_station('UCCS')
    # download_station('MEMS')
    # download_station('OSTB')
    compress_station('UCCS')
    compress_station('MEMS')
    compress_station('OSTB')
