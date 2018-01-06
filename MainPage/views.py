from django.shortcuts import render, get_object_or_404, get_list_or_404
from django.http import HttpResponseRedirect, Http404
from django.core.urlresolvers import reverse
from company.models import CompanyBranch, CompanyDescription
from django.core.exceptions import MultipleObjectsReturned
from .forms import ContactForm, LookupForm
from django.core.mail import send_mail
from django.views.generic.edit import FormView
#from django.contrib.gis.geos import Point
#from django.contrib.gis.db.models.functions import Distance


def index(request):
    """Strona główna dla aplikacji ussr."""
    maincompany = get_object_or_404(CompanyBranch, is_main=1)
    shortdescription = get_object_or_404(CompanyDescription, id_company_description = 'Witam')
    context = {'maincompany': maincompany,
                'shortdescription': shortdescription}

    return render(request, 'MainPage/index.html', context)

def contact_form(request):

    success = False

    if request.method != 'POST':
        contact_form = ContactForm()
    else:
        contact_form = ContactForm(request.POST)
        if contact_form.is_valid():
            send_mail(
            'Nowa wiadomośc z systemu USSR',
            'Otrzymales nowa wiadomosc z systemu ussr\n'+
            'Temat: ' + contact_form.cleaned_data['subject'] +
            '\nNadawca: ' + contact_form.cleaned_data['sender'] +
            '\nTreśc: ' + contact_form.cleaned_data['message'],
            'System USSR',
            ['USSRUG@gmail.com'],
            fail_silently=False)

            success = True

    contact_form = ContactForm()

    context = {'contact_form' : contact_form,
                'success' : success}

    return render(request, 'MainPage/contact_form.html', context)

def branches(request):

    branches_list = get_list_or_404(CompanyBranch)
    branch_id_list = CompanyBranch.objects.values_list('id_company_branch', flat=True)
    context = {'branches_list': branches_list,
                'branch_id_list': branch_id_list}

    return render(request, 'MainPage/branches.html', context)


def about(request):

    description = get_object_or_404(CompanyDescription, id_company_description = 'Witam')
    context = {'description': description}

    return render(request, 'MainPage/about.html', context)

def directions(request):
    return render(request, 'MainPage/gmaps.html')

"""
class LookupView(FormView):
    form_class = LookupForm

    def get(self, request):
        return render_to_response('MainPage/location_test.html', RequestContext(request))

    def form_valid(self, form):
        user_latitude = form.cleaned_data['latitude']
        user_longitude = form.cleaned_date['longitude']

        closest_branch = CompanyBranch.objects.annotate(distance= \
                CalcDistance(user_latitude \
                , user_longitude \
                , 'latitude' \
                , 'longitude')) \
                .order_by(distance)[0:2]

        return render_to_response('MainPage/location_result.html',
                {'closest_branch': closest_branch})

    def CalcDistance(u_lat, u_lon, b_lat, b_lon):
        u_p = Point(u_lat, u_lon, srid=4326)
        b_p = Point(b_lat, b_lon, srid=4326)
        return Distance(u_p, b_p)
"""
