from django.http import Http404
from django.shortcuts import render, get_object_or_404
from django.http import HttpResponseRedirect, Http404
from django.core.urlresolvers import reverse
from django.contrib.auth.decorators import login_required

from .models import *
from .forms import MachineTypeForm, MachineForm

def index(request):
    """Strona główna dla aplikacji ussr."""
    return render(request, 'machines/index.html')

#@login_required
def machinetypes(request):
    machinetypes = MachineType.objects.all()
    context = {'machinetypes': machinetypes}
    return render(request, 'machines/machinetypes.html', context)

#@login_required
def machinetype(request, machinetype_id):
    machinetype = MachineType.objects.get(id_machine_type=machinetype_id)
    machines = machinetype.machine_set.all()
    context = {'machinetype': machinetype, 'machines': machines}
    return render(request, 'machines/machinetype.html', context)

#@login_required
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

#@login_required
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

#@login_required
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
