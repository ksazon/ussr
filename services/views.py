from django.shortcuts import render, get_object_or_404
from django.http import HttpResponseRedirect, Http404, HttpResponse
from django.core.urlresolvers import reverse
from services.models import SeDict
import time
import datetime
from company.sqlSelect import *
import json
from .models import *
from clients.models import Client
from services.models import SeDict, SeGroupDict
from machines.models import Machine
from workers.models import Worker
from company.models import Location
from django.views.generic.list import ListView
from utilities.models import WorkdayCalendarParams, ResourcesUsage, WorkdayCalendar
from django.core.urlresolvers import reverse
from django.contrib.auth.decorators import login_required
from .forms import ReservationForm, ReservationFormForClient, ReportFormatForm
from .tables import AllServicesTable, ClientServicesTable, ClientFinishedServicesTable, WorkerServicesTable
from django_tables2 import RequestConfig
from .filters import ResourcesUsageFilter
from django_tables2.export.export import TableExport

def service(request, segroupdict_id):
    service = SeDict.objects.filter(se_group=segroupdict_id)
    context = {'services': service}
    return render(request, 'services/services.html', context)


def services(request):
    services = SeDict.objects.all().order_by('se_group')
    segrupes = SeDict.objects.all().filter(se_group="ZWIERZ")
    segrupees = SeDict.objects.all().filter(se_group="SAMOCHOD")
    segrupeees = SeDict.objects.all().filter(se_group=None)
    segroupdicts = SeGroupDict.objects.all()
    #ses = SeDict.objects.all()
    context = {'services': services, 'segrupes':segrupes, 'segrupees':segrupees, 'segrupeees': segrupeees, 'segroupdicts':segroupdicts}
    return render(request, 'services/services.html', context)

# Displaying Services

def client_services(request):
    request_client = Client.objects.get(client_user_login = request.user.id)
    client_services = Service.objects.filter(client = request_client)
    client_resource_usage = ResourcesUsage.objects.filter(service__in = client_services, start_timestamp__gte = datetime.datetime.now())
    client_resource_usage_finished = ResourcesUsage.objects.filter(service__in = client_services, start_timestamp__lt = datetime.datetime.now())

    context = {}

    if client_resource_usage.exists():
        client_services_table = ClientServicesTable(client_resource_usage, order_by = 'service_detetime_start')
        RequestConfig(request, paginate={"per_page": 10, "page": 1}).configure(client_services_table)
        context.update({'client_services_table' : client_services_table})

    if client_resource_usage_finished.exists():
        client_finished_services_table = ClientFinishedServicesTable(client_resource_usage_finished, order_by = '-service_detetime_start', prefix="2-")
        RequestConfig(request, paginate={"per_page": 10, "page": 1}).configure(client_finished_services_table)
        context.update({'client_finished_services_table' : client_finished_services_table})

    return render(request, 'services/client_services.html', context)

def client_service_resignation(request):
    if request.method == 'POST' and request.is_ajax():
        service_resources_usage = get_object_or_404(ResourcesUsage, id_resources_usage = request.POST.get("id_resources_usage"))
        if (service_resources_usage != None):
            service = service_resources_usage.service
            service.delete()
            service_resources_usage.delete()
            html = '<p>Rezygnacja powiodla się</p>'
            return HttpResponse(html)
        else:
            html = '<p>Rezygnacja nie powiodla się. Spróbuj później lub skontaktuj się z nami</p>'
            return HttpResponse(html)
    else:
        html = '<p>Rezygnacja nie powiodla się. Spróbuj później lub skontaktuj się z nami</p>'
        return HttpResponse(html)

def worker_services_table(request):
    queryset = ResourcesUsage.objects.select_related().all()
    f = ResourcesUsageFilter(request.GET, queryset=queryset)
    table = AllServicesTable(f.qs)
    context = {}
    user_worker = Worker.objects.get(user_login = request.user.id)

    format_form = ReportFormatForm()

    RequestConfig(request, paginate={"per_page": 20, "page": 1}).configure(table)

    context.update({'filter' : f, 'table' : table, 'worker' : user_worker, 'format_form' : format_form})

    return render(request, 'services/worker_services.html', context)

# Generate Service reports

def generate_service_report(request):

    if request.method == 'GET':

        queryset = ResourcesUsage.objects.select_related().all()
        f = ResourcesUsageFilter(request.GET, queryset=queryset)
        table = AllServicesTable(f.qs)

        export_format = request.GET.get('_export', None)
        if TableExport.is_valid_format(export_format):
            exporter = TableExport(export_format, table)
            return exporter.response('report.{}'.format(export_format))

def generate_weakly_worker_services_report(request):

    if request.method == 'GET':

        request_worker = Worker.objects.get(user_login = request.user.id)
        resources_usage = ResourcesUsage.objects.filter(worker = request_worker, start_timestamp__gte = datetime.date.today(), finish_timestamp__lte = datetime.date.today() + datetime.timedelta(days = 7))

        table = AllServicesTable(resources_usage)

        export_format = request.GET.get('_export', None)
        if TableExport.is_valid_format(export_format):
            exporter = TableExport(export_format, table)
            return exporter.response('weekly_report.{}'.format(export_format))

# Client Service reservation

def reservation(request):

    services_groups = SeGroupDict.objects.all()
    services = SeDict.objects.all()


    context = {'services_groups' : services_groups,
                'services' : services}

    return render(request, 'services/calendar_for_reservation.html', context)

def generate_summary(request):
    if request.method == 'POST':
        reservation_form = ReservationFormForClient(request.POST)
        if reservation_form.is_valid():
            service = SeDict.objects.get(id_se_dict = reservation_form.cleaned_data['service'])
            datetime = reservation_form.cleaned_data['date']

            context = {'service' : service,
                       'datetime' : datetime}

            return render(request, 'services/reservation_summary.html', context)

def get_resources_to_reservation(result):
    free_worker = result.free_workers[0]
    free_machine = result.free_machines[0]
    free_location = result.free_locations[0]
    resources = {'free_worker' : free_worker,
                'free_machine' : free_machine,
                'free_location' : free_location }

    return resources

def save_reservation(request):
    if request.method == 'POST':
        reservation_form = ReservationFormForClient(request.POST)
        if reservation_form.is_valid():
            service_id = SeDict.objects.get(id_se_dict = reservation_form.cleaned_data['service'])
            date_time = reservation_form.cleaned_data['date']
            facture = reservation_form.cleaned_data['facture']

            service_date = date_time.isoformat(sep=' ')

            result = gen_calendar(service_id.id_se_dict, service_date, service_date)

            if result is not None:
                result = result[0]
                client = Client.objects.get(client_user_login = request.user.id)

                new_service = Service()
                new_service.service_code = service_id
                new_service.client = client
                new_service.create_invoice = facture
                new_service.planned_start = date_time
                if service_id.avg_time is not None:
                    new_service.planned_end = date_time + datetime.timedelta(minutes = service_id.avg_time)
                new_service.save()

                resources_to_reservation = get_resources_to_reservation(result)
                new_resources_usage = ResourcesUsage()
                new_resources_usage.service = new_service
                new_resources_usage.machine = Machine.objects.get(id_machine = resources_to_reservation['free_machine'])
                new_resources_usage.worker = Worker.objects.get(id_worker = resources_to_reservation['free_worker'])
                new_resources_usage.location = Location.objects.get(id_location = resources_to_reservation['free_location'])
                new_resources_usage.start_timestamp = date_time
                if service_id.avg_time is not None:
                    new_resources_usage.finish_timestamp = date_time + datetime.timedelta(minutes = service_id.avg_time)
                new_resources_usage.calendar_date = WorkdayCalendar(id_workday_calendar = date_time)
                new_resources_usage.save()


                context = {'date' : service_date,
                            'result' : result,
                            'facture' : new_resources_usage.machine}
                return HttpResponse('success')

# Worker Service reservation

def worker_reservation(request, id_client):
    services_groups = SeGroupDict.objects.all()
    services = SeDict.objects.all()


    context = { 'id_client' : id_client,
                'services_groups' : services_groups,
                'services' : services}

    return render(request, 'services/calendar_for_worker_reservation.html', context)


def generate_worker_reservation_summary(request):
    if request.method == 'POST':
        reservation_form = ReservationForm(request.POST)
        if reservation_form.is_valid():
            service = SeDict.objects.get(id_se_dict = reservation_form.cleaned_data['service'])
            datetime = reservation_form.cleaned_data['date']
            client = Client.objects.get(id_client = reservation_form.cleaned_data['id_client'])

        context = { 'client' : client,
                    'service' : service,
                    'datetime' : datetime}

        return render(request, 'services/reservation_worker_summary.html', context)

def save_worker_reservation(request):
    if request.method == 'POST':
        reservation_form = ReservationForm(request.POST)
        if reservation_form.is_valid():
            client = Client.objects.get(id_client = reservation_form.cleaned_data['id_client'])
            service_id = SeDict.objects.get(id_se_dict = reservation_form.cleaned_data['service'])
            date_time = reservation_form.cleaned_data['date']
            facture = reservation_form.cleaned_data['facture']

            service_date = date_time.isoformat(sep=' ')

            result = gen_calendar(service_id.id_se_dict, service_date, service_date)

            if result is not None:
                result = result[0]

                new_service = Service()
                new_service.service_code = service_id
                new_service.client = client
                new_service.create_invoice = facture
                new_service.planned_start = date_time
                if service_id.avg_time is not None:
                    new_service.planned_end = date_time + datetime.timedelta(minutes = service_id.avg_time)
                new_service.save()

                resources_to_reservation = get_resources_to_reservation(result)
                new_resources_usage = ResourcesUsage()
                new_resources_usage.service = new_service
                new_resources_usage.machine = Machine.objects.get(id_machine = resources_to_reservation['free_machine'])
                new_resources_usage.worker = Worker.objects.get(id_worker = resources_to_reservation['free_worker'])
                new_resources_usage.location = Location.objects.get(id_location = resources_to_reservation['free_location'])
                new_resources_usage.start_timestamp = date_time
                if service_id.avg_time is not None:
                    new_resources_usage.finish_timestamp = date_time + datetime.timedelta(minutes = service_id.avg_time)
                new_resources_usage.calendar_date = WorkdayCalendar(id_workday_calendar = date_time)
                new_resources_usage.save()


                context = {'date' : service_date,
                            'result' : result,
                            'facture' : new_resources_usage.machine}
                return HttpResponse('success')

# Calendar generators

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
        return render(request, 'services/calendar.html', context)
#    return HttpResonse(result, content_type = "application/json")

# def add(request):
#     sqlResult = sqlSelect()
#     return render(request, 'company/sql.html', {'queryResult': sqlResult})


def generate_worker_calendar(request):

    if request.method == 'GET' and request.is_ajax():

        request_worker = Worker.objects.get(user_login = request.user.id)

        start, finish = calculate_date_to_display()

        worker_resource_usage = ResourcesUsage.objects.filter(worker = request_worker, start_timestamp__gt = start, finish_timestamp__lt = finish)

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

        context = {'worker_resource_usage' : worker_resource_usage,
                    'workday_start' : workday_start,
                    'workday_end' : workday_end,
                    'display_start' : start,
                    'display_end' : finish
                    }
        # listResult = gen_calendar()
        # result = json.dumps(listResult)
        # result = '/n'.join(record in listResults)
        return render(request, 'workers/worker_calendar.html', context)
