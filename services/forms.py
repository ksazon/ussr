from django import forms
import datetime

class ReservationForm(forms.Form):
    id_client = forms.IntegerField()
    service = forms.CharField()
    date = forms.DateTimeField(input_formats=["%Y-%m-%d %H:%M"])
    facture = forms.BooleanField(required=False)

class ReservationFormForClient(forms.Form):
    service = forms.CharField()
    date = forms.DateTimeField(input_formats=["%Y-%m-%d %H:%M"])
    facture = forms.BooleanField(required=False)

REPORT_CHOICES = (
                ('xls', 'xls'),
                ('csv', 'csv'),
                ('json', 'json'),
                ('latex', 'latex'),
                )

class ReportFormatForm(forms.Form):
    _export = forms.ChoiceField(choices = REPORT_CHOICES, label = "Pobierz swój raport na 7 dni") 
