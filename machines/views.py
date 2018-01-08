from django.http import Http404
from django.shortcuts import render, get_object_or_404
from django.http import HttpResponseRedirect, Http404
from django.core.urlresolvers import reverse
from django.contrib.auth.decorators import login_required
from django.contrib.auth.models import User, Group
from django.contrib.auth.decorators import user_passes_test
from django.contrib.auth import authenticate, login
from utilities.models import Address, ResourcesUsage
from services.models import SeDict, SeRequirement
#from .tables import AllSerWorTable

from .models import *
from .forms import MachineTypeForm, MachineForm

def is_logged_employee(user):
    if user is not None:
        return user.groups.filter(name='workers').exists()

@user_passes_test(is_logged_employee, login_url = 'users/login/', redirect_field_name = None)
def index(request):
    return render(request, 'machines/index.html')

@user_passes_test(is_logged_employee, login_url = 'users/login/', redirect_field_name = None)
def machinetypes(request):
    machinetypes = MachineType.objects.all().order_by('machine_type_name')
    context = {'machinetypes': machinetypes}
    return render(request, 'machines/machinetypes.html', context)

@user_passes_test(is_logged_employee, login_url = 'users/login/', redirect_field_name = None)
def machinetype(request, machinetype_id):
    machinetype = MachineType.objects.get(id_machine_type=machinetype_id)
    machines = machinetype.machine_set.all()
    context = {'machinetype': machinetype, 'machines': machines}
    return render(request, 'machines/machinetype.html', context)

@user_passes_test(is_logged_employee, login_url = 'users/login/', redirect_field_name = None)
def new_machinetype(request):
    if request.method != 'POST':
        form = MachineTypeForm()
    else:
        form = MachineTypeForm(request.POST)
        if form.is_valid():
            #new_topic = form.save(commit=False)
            #new_topic.owner = request.user
            #new_topic.save()
            form.save()
            return HttpResponseRedirect(reverse('machines:machinetypes'))

    context = {'form': form}
    return render(request, 'machines/new_machinetype.html', context)

@user_passes_test(is_logged_employee, login_url = 'users/login/', redirect_field_name = None)
def new_machine(request, machinetype_id):
    machinetype = MachineType.objects.get(id_machine_type=machinetype_id)

    if request.method != 'POST':
        form = MachineForm()
    else:
        form = MachineForm(data=request.POST)
        if form.is_valid():
            new_machine = form.save(commit=False)
            new_machine.machinetype = machinetype
            new_machine.save()
            return HttpResponseRedirect(reverse('machines:machinetype',
                                        args=[machinetype_id]))

    context = {'machinetype': machinetype, 'form': form}
    return render(request, 'machines/new_machine.html', context)

@user_passes_test(is_logged_employee, login_url = 'users/login/', redirect_field_name = None)
def edit_machine(request, machine_id):
    machine = Machine.objects.get(id_machine=machine_id)
    machinetype = machine.machine_type

    if request.method != 'POST':
        form = MachineForm(instance=machine)
    else:
        form = MachineForm(instance=machine, data=request.POST)
        if form.is_valid():
            form.save()
            return HttpResponseRedirect(reverse('machines:machinetype',
                                        args=[machinetype.id_machine_type]))

    context = {'machine': machine, 'machinetype': machinetype, 'form': form}
    return render(request, 'machines/edit_machine.html', context)

@user_passes_test(is_logged_employee, login_url = 'users/login/', redirect_field_name = None)
def delete_machine(request, machine_id):
    machine = Machine.objects.get(id_machine=machine_id)
    machinetype = machine.machine_type

    if request.method == 'POST':
        machine.delete()
        return HttpResponseRedirect(reverse('machines:machinetype',
                                    args=[machinetype.id_machine_type]))

    context = {'machine': machine, 'machinetype': machinetype}
    return render(request, 'machines/delete_machine.html', context)
