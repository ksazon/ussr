from .tables import AllServicesTable
from utilities.models import ResourcesUsage

import django_filters

class ResourcesUsageFilter(django_filters.FilterSet):
    service__service_code__se_dict_name =  django_filters.CharFilter(label = "Nazwa usługi:")
    start_timestamp = django_filters.DateFilter(lookup_expr='gte', label = 'Data od:')
    finish_timestamp = django_filters.DateFilter(lookup_expr='lte', label = 'Data do:')
    service__client__first_name = django_filters.CharFilter(label = "Imię klienta:")
    service__client__last_name = django_filters.CharFilter(label = "Nazwisko klienta:")
    worker__first_name = django_filters.CharFilter(label = "Imię pracownika:")
    worker__last_name = django_filters.CharFilter(label = "Nazwisko pracownika:")

    class Meta:
        model = ResourcesUsage
        fields = [ 'service__service_code__se_dict_name','start_timestamp', 'finish_timestamp', 'service__client__first_name' , 'service__client__last_name' ,
                     'worker__first_name', 'worker__last_name']
