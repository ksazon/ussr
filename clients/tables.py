import django_tables2 as tables
from clients.models import Client

TEMPLATE = """
<button class = "edit-button" value="{{record.id_client}}" data-toggle="modal"  href="#editmodal">Edytuj</button>
"""
EMAIL = """{% load staticfiles %}
<button class = "email-button" value="{{record.id_client}}"><img class = "email-icon" src = "{% static "clients/images/mail.png" %}"></button>
"""
RESERVATION = """
<button class = "client-reservation-button" value="{{record.id_client}}">Zarezerwuj</button>
"""

class CheckBoxColumnWithName(tables.CheckBoxColumn):
    @property
    def header(self):
        return ' '


class ClientTable(tables.Table):
    selection = CheckBoxColumnWithName(accessor='id_client', exclude_from_export=True, orderable=False)
    edit = tables.TemplateColumn(TEMPLATE, exclude_from_export=True, orderable=False)
    napisz = tables.TemplateColumn(EMAIL, exclude_from_export=True, orderable=False)
    zarezerwuj = tables.TemplateColumn(RESERVATION, exclude_from_export=True, orderable=False)
    class Meta:
        model = Client
        fields = ( 'first_name', 'last_name', 'sex', 'address.street', 'address.house_no', 'address.apartment_no', 'address.city', 'address.zip', 'address.phone', 'address.email')
        sequence = ('selection', '...')
        template = 'django_tables2/bootstrap.html'
