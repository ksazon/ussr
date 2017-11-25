from django import forms

from .models import SeDict

# class MachineTypeForm(forms.ModelForm):
#     class Meta:
#         model = MachineType
#         fields = ['machine_type_name']

        #labels = {'text': ''}

class ServiceForm(forms.ModelForm):
    class Meta:
        model = SeDict
        fields = ['id_se_dict', 'se_dict_name', 'base_price', 'location_type']
        #labels = {'text': ''}
