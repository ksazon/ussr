from django import forms
from django.contrib.auth.models import User
from django.contrib.auth.forms import UserCreationForm
from django.forms import ModelForm
from utilities.models import Address
from clients.models import Client

class UserCreateForm(UserCreationForm):
    username = forms.CharField(label=("Nazwa użytkownika"))
    password1 = forms.CharField(label=("Hasło"), widget=forms.PasswordInput, help_text=("Hasło musi zawierać co najmniej 8 liter"))
    password2 = forms.CharField(label=("Powtórz hasło"), help_text=("Powtórz powyższe hasło"), widget=forms.PasswordInput)


class ClientAddressForm(ModelForm):
    email = forms.EmailField()
    street = forms.CharField(required = True, label = 'Ulica')
    house_no = forms.CharField(required = True, label = 'Numer domu')
    city = forms.CharField(required = True, label = 'Miasto')
    zip = forms.CharField(required = True, label = 'Kod pocztowy')
    class Meta:
        model = Address
        fields = [ 'email', 'phone', 'street', 'house_no', 'apartment_no', 'city', 'zip' ]


class ClientPersonalInformationsForm(ModelForm):
    first_name = forms.CharField(required = True, label = 'Imię')
    last_name = forms.CharField(required = True, label = 'Nazwisko')
    class Meta:
        model = Client
        fields = [ 'first_name', 'last_name', 'sex' ]
