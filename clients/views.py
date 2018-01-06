from django.shortcuts import render
from django.contrib.auth.models import User, Group
from utilities.models import Address, SexDict
from django.core.exceptions import ObjectDoesNotExist
from django.contrib.auth.decorators import login_required
from .models import Client
from .forms import ClientAddressForm, ClientPersonalInformationsForm, EmailForm
from django.http import JsonResponse
from django.core import serializers
from .tables import ClientTable
from django_tables2 import RequestConfig
from django.core.mail import send_mail
from .filters import ClientTableFilter
from services.forms import ReportFormatForm
from django_tables2.export.export import TableExport



def is_logged_client(user):
    if user is not None:
        return Client.objects.get(client_user_login = user.id).exists()

@login_required
def change_personal_informations(request):

    try:
        client = Client.objects.get(client_user_login = request.user.id)
    except ObjectDoesNotExist:
        return render(request, 'clients/personal_informations.html', {'not_registered_client_in_client_tab':True})

    if (client.address is not None):
        address = Address.objects.get(id_address = client.address.id_address)
    else:
        address = Address()

    name_data = {'first_name':client.first_name,
                  'last_name':client.last_name,
                  'sex':client.sex}

    address_data = {'email':address.email,
                'phone':address.phone,
                'street':address.street,
                'house_no':address.house_no,
                'apartment_no':address.apartment_no,
                'city':address.city,
                'zip':address.zip}

    success = False

    if request.method == 'POST':
        name_form = ClientPersonalInformationsForm(request.POST, instance=client)
        address_form = ClientAddressForm(request.POST, instance = address)

        if address_form.is_valid():
            address = address_form.save()

        if name_form.is_valid():
            client.address = address
            client = name_form.save()
            client.save()

        success = True

    else:
        name_form = ClientPersonalInformationsForm(name_data, instance=client)
        address_form = ClientAddressForm(address_data, instance = address)

    context = {'success' : success,
                'name_form' : name_form,
                'address_form': address_form}

    return render(request, 'clients/personal_informations.html', context)

def Client_table(request):
    if request.method == 'GET':
        clients = Client.objects.all()
        f = ClientTableFilter(request.GET, queryset=clients)
        client_table = ClientTable(f.qs, order_by="last_name")
        RequestConfig(request, paginate={"per_page": 20, "page": 1}).configure(client_table)


        context = { 'client_table' : client_table,}

        return render(request, 'clients/client_table.html', context)

@login_required
def clientCRUDlist(request):

    clients = Client.objects.all()
    f = ClientTableFilter(request.GET, queryset=clients)
    client_table = ClientTable(f.qs, order_by="last_name")
    RequestConfig(request, paginate={"per_page": 20, "page": 1}).configure(client_table)
    name_form = ClientPersonalInformationsForm()
    address_form = ClientAddressForm()
    email_form = EmailForm()
    format_form = ReportFormatForm()


    context = { 'filter' : f,
                'client_table' : client_table,
                'name_form' : name_form,
                'address_form': address_form,
                'email_form' : email_form,
                'format_form' : format_form,}

    return render(request, 'clients/clientCRUDlist.html', context)

def CreateClient(request):
    if request.method == 'POST' and request.is_ajax():
        client = Client()
        address = Address()

        if request.method == 'POST':
            name_form = ClientPersonalInformationsForm(request.POST, instance=client)
            address_form = ClientAddressForm(request.POST, instance = address)

            if address_form.is_valid():
                address = address_form.save()
            else:
                context = {'name_form' : name_form,
                            'address_form' : address_form}
                return render(request, 'clients/new_client_form.html', context)

            if name_form.is_valid():
                client.address = address
                client = name_form.save()
                client.save()

        context = {'name_form' : name_form,
                    'address_form' : address_form}

        return render(request, 'clients/new_client_form.html', context)

def GenerateTable(request):
    clients = Client.objects.all()
    client_table = ClientTable(clients).order_by = 'last_name'
    name_form = ClientPersonalInformationsForm()
    address_form = ClientAddressForm()
    context = {'client_table' : client_table,
                'name_form' : name_form,
                'address_form': address_form}

    return render(request, 'clients/clientCRUDlist.html', context)


def DeleteClient(request):
    if request.method == 'POST' and request.is_ajax():

        id_clients = request.POST.getlist("selection")

        selected_clients = Client.objects.filter(id_client__in = id_clients)
        selected_clients.delete()

        clients = Client.objects.all()
        client_table = ClientTable(clients)
        name_form = ClientPersonalInformationsForm()
        address_form = ClientAddressForm()

        context = {'client_table' : client_table,
                    'name_form' : name_form,
                    'address_form': address_form}

        return JsonResponse({'success' : 'success'})

def EditClientFormFill(request):
    if request.method == 'POST' and request.is_ajax():
        post_id_client = request.POST.get("id")
        client = Client.objects.get(id_client = post_id_client)


        if (client.address is not None):
            address = Address.objects.get(id_address = client.address.id_address)
        else:
            address = Address()

        name_data = {'first_name':client.first_name,
                      'last_name':client.last_name,
                      'sex':client.sex,
                      'id_client' : client.id_client }

        address_data = {'email':address.email,
                    'phone':address.phone,
                    'street':address.street,
                    'house_no':address.house_no,
                    'apartment_no':address.apartment_no,
                    'city':address.city,
                    'zip':address.zip}


        name_form = ClientPersonalInformationsForm(name_data, instance=client)
        address_form = ClientAddressForm(address_data, instance = address)

        context = {'name_form' : name_form,
                    'address_form': address_form,
                    'client' : client}

        return render(request, 'clients/editform.html', context)

def EditClient(request):

    if request.method == 'POST':
        client = Client.objects.get(id_client = request.POST.get("id"))
        if (client.address is not None):
            address = Address.objects.get(id_address = client.address.id_address)
        else:
            address = Address()

        name_form = ClientPersonalInformationsForm(request.POST, instance=client)
        address_form = ClientAddressForm(request.POST, instance = address)

        if address_form.is_valid():
            address = address_form.save()

        if name_form.is_valid():
            client.address = address
            client = name_form.save()
            client.save()

        context = {'name_form' : name_form,
                    'address_form' : address_form,}

        return render(request, 'clients/editform.html', context)

def EmailCheck(request):

    if request.method == 'POST':
        client = Client.objects.get(id_client = request.POST.get("id"))
        if (client.address is None):
            return JsonResponse({'email' : False})
        elif (client.address.email is None):
            return JsonResponse({'email' : False})
        else:
            return JsonResponse({'email' : True})

def SendEmail(request):

    if request.method == 'POST':
        client = Client.objects.get(id_client = request.POST.get("id"))
        email_form = EmailForm(request.POST)
        if email_form.is_valid():
            send_mail(
            email_form.cleaned_data['subject'],
            'Otrzymałeś nową wiadomość z systemu ussr\n'+
            'Temat: ' + email_form.cleaned_data['subject'] +
            '\nTreść: ' + email_form.cleaned_data['message'],
            'System USSR',
            [client.address.email],
            fail_silently=False)
            return JsonResponse({'success' : "success"})


def generate_clients_report(request):

    if request.method == 'GET':

        queryset = Client.objects.select_related().all()
        f =  ClientTableFilter(request.GET, queryset=queryset)
        table = ClientTable(f.qs, order_by="last_name")

        export_format = request.GET.get('_export', None)
        if TableExport.is_valid_format(export_format):
            exporter = TableExport(export_format, table)
            return exporter.response('report.{}'.format(export_format))
