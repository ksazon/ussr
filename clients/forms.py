from django import forms
from django.forms import ModelForm
from utilities.models import Address
from .models import Client


class ClientAddressForm(ModelForm):
    class Meta:
        model = Address
        fields = ['email', 'phone', 'street', 'house_no', 'apartment_no', 'city', 'zip' ]


class ClientPersonalInformationsForm(ModelForm):
    class Meta:
        model = Client
        fields = ['first_name', 'last_name', 'sex', ]
