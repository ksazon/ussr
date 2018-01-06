from django.shortcuts import render
from django.http import HttpResponseRedirect
from django.core.urlresolvers import reverse
from django.contrib.auth import login, logout, authenticate
from .forms import ClientAddressForm, ClientPersonalInformationsForm
from utilities.models import Address
from clients.models import Client
from django.db import transaction
from django.contrib.auth.forms import AuthenticationForm
from .forms import UserCreateForm


def logout_view(request):
    logout(request)
    return HttpResponseRedirect(reverse('MainPage:index'))

@transaction.atomic()
def register(request):
    if request.method != 'POST':
        user_form = UserCreateForm()
        name_form = ClientPersonalInformationsForm()
        address_form = ClientAddressForm()
    else:
        address = Address()
        client = Client()

        user_form = UserCreateForm(data=request.POST)
        name_form = ClientPersonalInformationsForm(request.POST, instance = client)
        address_form = ClientAddressForm(request.POST, instance = address)

        if user_form.is_valid():
            if address_form.is_valid():
                if name_form.is_valid():
                    new_user = user_form.save()
                    address.save()
                    client.client_user_login = new_user
                    client.address = address
                    client.save()

                    user_form = UserCreateForm()
                    name_form = ClientPersonalInformationsForm()
                    address_form = ClientAddressForm()
                    success = True

                    context = {'success':success,
                                'user_form' : user_form,
                                'name_form' : name_form,
                                'address_form' : address_form}

                else:
                    context = {'user_form' : user_form,
                                'name_form' : name_form,
                                'address_form' : address_form}
                    return render(request, 'users/register.html', context)


            else:
                context = {'user_form' : user_form,
                            'name_form' : name_form,
                            'address_form' : address_form}
                return render(request, 'users/register.html', context)

        else:
            context = {'user_form' : user_form,
                        'name_form' : name_form,
                        'address_form' : address_form}

            return render(request, 'users/register.html', context)

        return render(request, 'users/register.html', context)

    context = {'user_form' : user_form,
                'name_form' : name_form,
                'address_form' : address_form}

    return render(request, 'users/register.html', context)

def user_login(request):
    if request.method == 'POST':
        form = AuthenticationForm(data = request.POST)
        if form.is_valid():
            username = form.data['username']
            password = form.data['password']
            user = authenticate(request, username=username, password=password)
            if user is not None:
                if user.groups.filter(name='workers').exists():
                    login(request, user)
                    return HttpResponseRedirect(reverse('workers:employee_index'))
                else:
                    login(request, user)
                    return HttpResponseRedirect(reverse('MainPage:index'))
            else:
                return render(request, 'users/login.html', {'form': form})
        else:
            return render(request, 'users/login.html', {'form': form})
    else:
        form = AuthenticationForm()
        return render(request, 'users/login.html', {'form': form})
