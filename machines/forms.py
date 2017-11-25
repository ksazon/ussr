from django import forms

from .models import MachineType, Machine

class MachineTypeForm(forms.ModelForm):
    class Meta:
        model = MachineType
        fields = ['id_machine_type', 'machine_type_name']
        #labels = {'text': ''}

class MachineForm(forms.ModelForm):
    class Meta:
        model = Machine
        fields = ['machine_name', 'machine_type', 'service_interval', 'last_service', 'is_operational', 'notes']
        #labels = {'text': ''}
