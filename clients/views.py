from django.shortcuts import render
from django.contrib.auth.models import User, Group
from utilities.models import Address
from django.core.exceptions import ObjectDoesNotExist
from django.contrib.auth.decorators import login_required
from .models import Client
from .forms import ClientAddressForm, ClientPersonalInformationsForm

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

    if request.method == 'POST':
        name_form = ClientPersonalInformationsForm(request.POST, instance=client)
        address_form = ClientAddressForm(request.POST, instance = address)

        if address_form.is_valid():
            address = address_form.save()

        if name_form.is_valid():
            client.address = address
            client = name_form.save()
            client.save()
    else:
        name_form = ClientPersonalInformationsForm(name_data, instance=client)
        address_form = ClientAddressForm(address_data, instance = address)

    context = {'name_form' : name_form,
                'address_form': address_form}

    return render(request, 'clients/personal_informations.html', context)
