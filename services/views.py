from django.http import Http404
from django.shortcuts import render, redirect, get_object_or_404
from django.http import HttpResponseRedirect, Http404
from django.core.urlresolvers import reverse
from django.contrib.auth.decorators import login_required
from .models import *

def service(request, segroupdict_id):
    service = SeDict.objects.filter(se_group=segroupdict_id)
    context = {'services': service}
    return render(request, 'services/services.html', context)


def services(request):
    services = SeGroupDict.objects.all()
    context = {'services': services}
    return render(request, 'services/categories.html', context)
