from .tables import ClientTable
from .models import Client

import django_filters

class ClientTableFilter(django_filters.FilterSet):
    address__street = django_filters.CharFilter(label = "Ulica:")
    address__house_no = django_filters.CharFilter(label = "Numer domu:")
    address__apartment_no = django_filters.CharFilter(label = "Numer mieszkania:")
    address__city = django_filters.CharFilter(label = "Miasto:")
    address__zip = django_filters.CharFilter(label = "Kod pocztowy:")
    address__phone = django_filters.CharFilter(lookup_expr='contains', label = 'Telefon:')
    address__email = django_filters.CharFilter(lookup_expr='contains', label = 'E-mail:')

    class Meta:
        model = Client
        fields = [ 'first_name','last_name', 'address__street', 'address__house_no' , 'address__apartment_no' ,
                     'address__city', 'address__zip', 'address__phone', 'address__email' ]
