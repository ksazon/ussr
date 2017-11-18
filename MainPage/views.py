from django.shortcuts import render, get_object_or_404
from django.http import HttpResponseRedirect, Http404
from django.core.urlresolvers import reverse


def index(request):
    """Strona główna dla aplikacji ussr."""
    return render(request, 'MainPage/index.html')
