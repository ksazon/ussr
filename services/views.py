from django.http import Http404
from django.shortcuts import render, redirect, get_object_or_404
from django.http import HttpResponseRedirect, Http404
from django.core.urlresolvers import reverse
from django.contrib.auth.decorators import login_required
from .models import *
# from .views import *
from .forms import ServiceForm
from . import forms
# Create your views here.

def services(request):
    services = SeDict.objects.all()
    context = {'services': services}
    return render(request, 'services/services.html', context)

def returnServices(request):
    services = SeDict.objects.all()
    context = {'services': services}
    return render(request, context)



def index(request):
    return render(request, 'services/index.html')



# dodawanie usługi elo
@login_required
def addService(request):
    """Dodanie nowego wpisu dla określonego tematu."""
    # machinetype = MachineType.objects.get(id=machinetype_id)
    # machinetype = MachineType.objects.get()

    if request.method != 'POST':
        form = ServiceForm()
    else:
        form = ServiceForm(request.POST)
        if form.is_valid():
            service = form.save(commit=False)
            # new_machine.machinetype = machinetype
            service.save()
            # return HttpResponseRedirect(reverse('services'))
            return redirect('services')#, pk=service.id_se_dict)
    context = {'form': form}
    # return render(request, 'services/services.html', context)
    return render(request, 'services/add_service.html', context)

# kuniec dodawenia



# @login_required
# def addService(request):
#     services = SeDict.objects.all()
#     context = {'services': services}
#     return render(request, 'services/services.html', context)

# def showServiceSlots(request):
#     slo
