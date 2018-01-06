from django.http import Http404
from django.shortcuts import render, get_object_or_404
from django.http import HttpResponseRedirect, Http404, HttpResponse
from django.core.urlresolvers import reverse
from services.models import SeDict
import datetime
from company.sqlSelect import *
import json
from .models import *
from services.models import SeDict, SeGroupDict
from django.views.generic.list import ListView
from utilities.models import WorkdayCalendarParams
import datetime
import time

def index(request):
    """Strona główna dla aplikacji ussr."""
    return render(request, 'company/index.html')

# def calGenListView(ListView):


def reservation(request):

    services_groups = SeGroupDict.objects.all()
    services = SeDict.objects.all()


    context = {'services_groups' : services_groups,
                'services' : services}

    return render(request, 'company/calendar_test.html', context)

def calculate_date_to_display():

    workday_calendar_params = WorkdayCalendarParams.objects.get( id_workday_calendar_params = 1 )
    days_to_display = workday_calendar_params.days_to_display

    start = datetime.datetime.now()
    finish = datetime.date.today() + datetime.timedelta(days = days_to_display)

    return start, finish

def generate_calendar(request):

    if request.method == 'POST' and request.is_ajax():

        service = SeDict.objects.get(id_se_dict = request.POST.get('service_code'))

        workday_calendar = WorkdayCalendarParams.objects.get(id_workday_calendar_params = 1)

        week_workday_start = workday_calendar.default_workday_start_time
        week_workday_end = workday_calendar.default_workday_end_time

        saturday_workday_start = workday_calendar.default_saturday_start_time
        saturday_workday_end = workday_calendar.default_saturday_end_time

        if ( saturday_workday_start != None and saturday_workday_end != None):
            if (week_workday_start < saturday_workday_start):
                workday_start = week_workday_start
            else:
                workday_start = saturday_workday_start

            if (week_workday_end > saturday_workday_end):
                workday_end = week_workday_end
            else:
                workday_end = saturday_workday_end
        else:
            workday_start = week_workday_start
            workday_end = week_workday_end

        start, finish = calculate_date_to_display()

        result = gen_calendar(service.id_se_dict, start, finish)

        context = {'result' : result,
                    'service' : service,
                    'workday_start' : workday_start,
                    'workday_end' : workday_end,
                    'display_start' : start,
                    'display_end' : finish
                    }
        # listResult = gen_calendar()
        # result = json.dumps(listResult)
        # result = '/n'.join(record in listResults)
        return render(request, 'company/calendar.html', context)
#    return HttpResonse(result, content_type = "application/json")

# def add(request):
#     sqlResult = sqlSelect()
#     return render(request, 'company/sql.html', {'queryResult': sqlResult})
