from django.db import connection
from collections import namedtuple
import datetime
import json
import time

#def gen_calendar(service_code = "WOPO", start = datetime.datetime.now(), finish = datetime.datetime.today() + datetime.timedelta(days = 7)):
def gen_calendar(service_code, start, finish):

    cursor = connection.cursor()

    # TODO do usuniecia, tylko na czas testow
    #start = '2017-12-01T00:00:00'
    #finish = '2017-12-02T00:00:00'
    #start = time.mktime(time.strptime('01/12/2017', "%d/%m/%Y"))
    #finish = time.mktime(time.strptime('3/12/2011', "%d/%m/%Y"))
#     finish_ts = datetime.datetime.fromtimestamp(finish).strftime('%Y-%m-%d %H:%M:%S')

    def namedtuplefetchall(cursor):
        desc = cursor.description
        nt_result = namedtuple('Result', [col[0] for col in desc])
        return [nt_result(*row) for row in cursor.fetchall()]

    cursor.execute("SELECT * FROM get_calendar (%s, %s, %s)", [service_code, start, finish]);

    results = namedtuplefetchall(cursor)
    # results = json.dumps(resultsTuple)
    return results
