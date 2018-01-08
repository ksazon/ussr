import django_tables2 as tables
from utilities.models import ResourcesUsage
TEMPLATE = """
<button class = "resignation-button" value="{{record.id_resources_usage}}" data-toggle="modal" href="#reservation-resignation-box">Zrezygnuj</button>
"""


class ClientServicesTable(tables.Table):

    service_name = tables.Column(accessor = 'service.service_code.se_dict_name')
    service_detetime_start = tables.DateTimeColumn(accessor = 'service.planned_start', format="d-m-Y H:i")
    service_detetime_end = tables.DateTimeColumn(accessor = 'service.planned_end', format="d-m-Y H:i")
    service_price = tables.Column(accessor = 'service.service_code.base_price')
    employee_name = tables.Column(accessor = 'worker.get_employee_name', orderable = False)
    resignation_button = tables.TemplateColumn(TEMPLATE, verbose_name = '')

    class Meta:
        model = ResourcesUsage
        fields = {}
        template = 'django_tables2/bootstrap.html'


class ClientFinishedServicesTable(tables.Table):

    service_name = tables.Column(accessor = 'service.service_code.se_dict_name')
    service_detetime_start = tables.DateTimeColumn(accessor = 'service.planned_start', format="d-m-Y H:i")
    service_detetime_end = tables.DateTimeColumn(accessor = 'service.planned_end', format="d-m-Y H:i")
    service_price = tables.Column(accessor = 'service.service_code.base_price')
    employee_name = tables.Column(accessor = 'worker.get_employee_name', orderable = False)

    class Meta:
        model = ResourcesUsage
        fields = {}
        template = 'django_tables2/bootstrap.html'

class WorkerServicesTable(tables.Table):

    service_name = tables.Column(accessor = 'service.service_code.se_dict_name')
    service_detetime_start = tables.DateTimeColumn(accessor = 'service.planned_start', format="d-m-Y H:i")
    service_detetime_end = tables.DateTimeColumn(accessor = 'service.planned_end', format="d-m-Y H:i")
    service_price = tables.Column(accessor = 'service.service_code.base_price')
    client_first_name = tables.Column(accessor = 'service.client.first_name')
    client_last_name = tables.Column(accessor = 'service.client.last_name')


    class Meta:
        model = ResourcesUsage
        fields = {}
        template = 'django_tables2/bootstrap.html'

class AllServicesTable(tables.Table):

    service_name = tables.Column(verbose_name = 'Nazwa usługi', accessor = 'service.service_code.se_dict_name')
    service_detetime_start = tables.DateTimeColumn(accessor = 'service.planned_start', format="d-m-Y H:i")
    service_detetime_end = tables.DateTimeColumn(accessor = 'service.planned_end', format="d-m-Y H:i")
    service_price = tables.Column(verbose_name = 'Bazowa cena [zł]', accessor = 'service.service_code.base_price')
    client_first_name = tables.Column(accessor = 'service.client.first_name', visible = False)
    client_last_name = tables.Column(accessor = 'service.client.last_name', visible = False)
    client_name = tables.Column(accessor = 'service.client.get_client_full_name', orderable = False)
    worker_first_name = tables.Column(accessor = 'worker.first_name', visible = False)
    worker_last_name = tables.Column(accessor = 'worker.last_name', visible = False)
    employee_name = tables.Column(accessor = 'worker.get_employee_name', orderable = False)
    location_name = tables.Column(verbose_name = 'Nazwa lokalizacji', accessor = 'location.location_name')
    location_type = tables.Column(verbose_name = 'Typ lokalizacji', accessor = 'location.location_type')
    machine_name = tables.Column(verbose_name = 'Nazwa maszyny', accessor = 'machine.machine_name')
    machine_type = tables.Column(verbose_name = 'Typ maszyny', accessor = 'machine.machine_type')

    class Meta:
        model = ResourcesUsage
        fields = {}
        template = 'django_tables2/bootstrap.html'
