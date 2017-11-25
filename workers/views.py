from django.shortcuts import render, get_object_or_404
from django.contrib.auth.forms import AuthenticationForm
from django.contrib.auth import authenticate, login
from django.http import HttpResponseRedirect
from django.core.urlresolvers import reverse
from django.contrib.auth.models import User, Group
from django.contrib.auth.decorators import user_passes_test
from .models import Worker
from utilities.models import Address
from .forms import WorkerAddressForm, WorkerPersonalInformationsForm
from django.core.exceptions import ObjectDoesNotExist



def is_logged_employee(user):
    if user is not None:
        return user.groups.filter(name='workers').exists()


@user_passes_test(is_logged_employee, login_url = '/employee/login', redirect_field_name = None)
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
    else:
        name_form = WorkerPersonalInformationsForm(name_data, instance=worker)
        address_form = WorkerAddressForm(address_data, instance = address)

    context = {'name_form' : name_form,
                'address_form': address_form}

    return render(request, 'workers/personal_informations.html', context)



def logout_view(request):
    logout(request)
    return HttpResponseRedirect(reverse('MainPage:index'))


@user_passes_test(is_logged_employee, login_url = '/employee/login', redirect_field_name = None)
def employee_main(request):
    return  render(request, 'workers/index.html')


def employee_login(request):
    form = AuthenticationForm(data = request.POST)
    if form.is_valid():
        username = form.data['username']
        password = form.data['password']
        user = authenticate(request, username=username, password=password)
        if user is not None:
            if user.groups.filter(name='workers').exists():
                login(request, user)
                return HttpResponseRedirect(reverse('workers:employee_main'))
            else:
                return render(request, 'workers/login.html', {'form': form,
                                                            'invalid': True})

    return render(request, 'workers/login.html', {'form': form})
