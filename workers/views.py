from django.shortcuts import render, get_object_or_404
from django.contrib.auth.forms import AuthenticationForm
from django.contrib.auth import authenticate, login
from django.http import HttpResponseRedirect
from django.core.urlresolvers import reverse
from django.contrib.auth.models import User, Group
from django.contrib.auth.decorators import user_passes_test
from .models import Worker
from utilities.models import Address, ResourcesUsage
from .forms import WorkerAddressForm, WorkerPersonalInformationsForm, WoNotificationForm
from django.core.exceptions import ObjectDoesNotExist
from .models import WoNotification
from machines.models import Machine, MachineType
import datetime
from services.forms import ReportFormatForm

def is_logged_employee(user):
    if user is not None:
        return user.groups.filter(name='workers').exists()

def is_logged_and_in_worker_table(user):
    if user is not None:
        return Worker.objects.filter(user_login = user.id).exists()

def is_not_in_worker_table(request):
    return render(request, 'workers/is_not_in_worker_table.html')

@user_passes_test(is_logged_employee, login_url = '/users/login/', redirect_field_name = None)
@user_passes_test(is_logged_and_in_worker_table, login_url = '/employee/is_not_in_worker_table/', redirect_field_name = None)
def change_personal_informations(request):

    try:
        worker = Worker.objects.get(user_login = request.user.id)
    except ObjectDoesNotExist:
        return render(request, 'workers/personal_informations.html', {'not_registered_worker_in_worker_tab':True})

    if (worker.address is not None):
        address = Address.objects.get(id_address = worker.address.id_address)
    else:
        address = Address()

    name_data = {'first_name':worker.first_name,
                  'last_name':worker.last_name,}

    address_data = {'email':address.email,
                'phone':address.phone,
                'street':address.street,
                'house_no':address.house_no,
                'apartment_no':address.apartment_no,
                'city':address.city,
                'zip':address.zip}


    if request.method == 'POST':
        name_form = WorkerPersonalInformationsForm(request.POST, instance=worker)
        address_form = WorkerAddressForm(request.POST, instance = address)

        if address_form.is_valid():
            address = address_form.save()

        if name_form.is_valid():
            worker.address = address
            worker = name_form.save()
            worker.save()

        success = True

    else:
        success = False
        name_form = WorkerPersonalInformationsForm(name_data, instance=worker)
        address_form = WorkerAddressForm(address_data, instance = address)

    context = {'success' : success,
                'name_form' : name_form,
                'address_form': address_form}

    return render(request, 'workers/personal_informations.html', context)



def logout_view(request):
    logout(request)
    return HttpResponseRedirect(reverse('MainPage:index'))

@user_passes_test(is_logged_employee, login_url = '/users/login/', redirect_field_name = None)
@user_passes_test(is_logged_and_in_worker_table, login_url = '/employee/is_not_in_worker_table/', redirect_field_name = None)
def employee_main(request):
    date = datetime.date.today().strftime('%m/%d/%y')
    report_form = ReportFormatForm()
    return  render(request, 'workers/index.html', {'date' : date, 'report_form' : report_form})


@user_passes_test(is_logged_employee, login_url = '/users/login/', redirect_field_name = None)
def woNotifications(request):
    woNotifications = WoNotification.objects.order_by('-id_wo_notification')
    context = {'woNotifications': woNotifications}
    return render(request, 'workers/woNotifications.html', context)

@user_passes_test(is_logged_employee, login_url = 'users/login/', redirect_field_name = None)
def woNotification(request, woNotification_id):
    woNotification = WoNotification.objects.get(id_wo_notification=woNotification_id)
    context = {'woNotification': woNotification}
    return render(request, 'workers/woNotification.html', context)

@user_passes_test(is_logged_employee, login_url = '/users/login/', redirect_field_name = None)
def new_woNotification(request):
    if request.method != 'POST':
        form = WoNotificationForm()
    else:
        form = WoNotificationForm(request.POST)
        if form.is_valid():
            #new_topic = form.save(commit=False)
            #new_topic.owner = request.user
            #new_topic.save()
            form.save()
            return HttpResponseRedirect(reverse('workers:woNotifications'))

    context = {'form': form}
    return render(request, 'workers/new_woNotification.html', context)


@user_passes_test(is_logged_employee, login_url = '/users/login/', redirect_field_name = None)
def edit_woNotification(request, woNotification_id):
    woNotification = WoNotification.objects.get(id_wo_notification=woNotification_id)
    #if topic.owner != request.user:
        #raise Http404
    if request.method != 'POST':
        form = WoNotificationForm(instance=woNotification)
    else:
        form = WoNotificationForm(instance=woNotification, data=request.POST)
        if form.is_valid():
            form.save()
            return HttpResponseRedirect(reverse('workers:woNotification',
                                        args=[woNotification.id_wo_notification]))

    context = {'woNotification': woNotification, 'form': form}
    return render(request, 'workers/edit_woNotification.html', context)

@user_passes_test(is_logged_employee, login_url = '/users/login/', redirect_field_name = None)
def wRaports(request):
    #woNotifications = WoNotification.objects.order_by('-id_wo_notification')
    return render(request, 'workers/wRaports.html')

@user_passes_test(is_logged_employee, login_url = '/users/login/', redirect_field_name = None)
def wRaport1(request):
    machines = Machine.objects.all().order_by('machine_type')
    context = {'machines': machines}
    return render(request, 'workers/wRaport1.html', context)

@user_passes_test(is_logged_employee, login_url = '/users/login/', redirect_field_name = None)
def wRaport2(request):
    machines = Machine.objects.filter(is_operational=True).order_by('machine_type')
    context = {'machines': machines}
    return render(request, 'workers/wRaport2.html', context)

@user_passes_test(is_logged_employee, login_url = '/users/login/', redirect_field_name = None)
def wRaport3(request):
    workers = Worker.objects.all().filter(active=True).order_by('last_name')
    context = {'workers': workers}
    return render(request, 'workers/wRaport3.html', context)

@user_passes_test(is_logged_employee, login_url = 'users/login/', redirect_field_name = None)
def wRaport4(request):
    resources = ResourcesUsage.objects.all().order_by('-finish_timestamp')
    context = {'resources': resources}
    return render(request, 'workers/wRaport4.html', context)
