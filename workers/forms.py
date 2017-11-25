from django import forms
from django.forms import ModelForm
from .models import Worker
from utilities.models import Address



class WorkerAddressForm(ModelForm):
    class Meta:
        model = Address
        fields = ['email', 'phone', 'street', 'house_no', 'apartment_no', 'city', 'zip' ]


class WorkerPersonalInformationsForm(ModelForm):
    class Meta:
        model = Worker
        fields = ['first_name', 'last_name']
