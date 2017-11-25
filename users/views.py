from django.shortcuts import render
from django.http import HttpResponseRedirect
from django.core.urlresolvers import reverse
from django.contrib.auth import login, logout, authenticate
from django.contrib.auth.forms import UserCreationForm
from clients.forms import ClientAddressForm, ClientPersonalInformationsForm
from utilities.models import Address
from clients.models import Client
from django.db import transaction


def logout_view(request):
    logout(request)
    return HttpResponseRedirect(reverse('MainPage:index'))

@transaction.atomic()
def register(request):
    if request.method != 'POST':
        user_form = UserCreationForm()
        name_form = ClientPersonalInformationsForm()
        address_form = ClientAddressForm()
    else:
        address = Address()
        client = Client()

        user_form = UserCreationForm(data=request.POST)
        name_form = ClientPersonalInformationsForm(request.POST, instance = client)
        address_form = ClientAddressForm(request.POST, instance = address)

        if user_form.is_valid():
            new_user = user_form.save()
        else:
            context = {'user_form' : user_form,
                        'name_form' : name_form,
                        'address_form' : address_form}

            return render(request, 'users/register.html', context)
        
        if address_form.is_valid():
            address.save()

        if name_form.is_valid():
            client.client_user_login = new_user
            client.address = address
            client.save()

        authenticated_user = authenticate(username=new_user.username,
                                password=request.POST['password1'])
        login(request, authenticated_user)
        return HttpResponseRedirect(reverse('MainPage:index'))

    context = {'user_form' : user_form,
                'name_form' : name_form,
                'address_form' : address_form}

    return render(request, 'users/register.html', context)
