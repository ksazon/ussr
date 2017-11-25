from django.shortcuts import render, get_object_or_404, get_list_or_404
from django.http import HttpResponseRedirect, Http404
from django.core.urlresolvers import reverse
from company.models import CompanyBranch, CompanyDescription
from django.core.exceptions import MultipleObjectsReturned
from .forms import ContactForm
from django.core.mail import send_mail




def index(request):
    """Strona główna dla aplikacji ussr."""
    maincompany = get_object_or_404(CompanyBranch, is_main=1)
    shortdescription = get_object_or_404(CompanyDescription, id_company_description = 'Witam')
    context = {'maincompany': maincompany,
                'shortdescription': shortdescription}

    return render(request, 'MainPage/index.html', context)


def contact(request):
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

    contact_form = ContactForm()
    branches_list = get_list_or_404(CompanyBranch)
    context = {'branches_list': branches_list,
                'contact_form': contact_form}



    return render(request, 'MainPage/contact.html', context)

def about(request):

    description = get_object_or_404(CompanyDescription, id_company_description = 'Witam')
    context = {'description': description}

    return render(request, 'MainPage/about.html', context)
