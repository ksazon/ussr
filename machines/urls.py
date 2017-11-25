from django.conf.urls import url
from . import views

app_name = 'machines'
urlpatterns = [
    url(r'^machinetypes/$', views.machinetypes, name='machinetypes'),
    url(r'^machinetypes/(?P<machinetype_id>\w+)/$', views.machinetype, name='machinetype'),
    url(r'^new_machinetype/$', views.new_machinetype, name='new_machinetype'),
    url(r'^new_machine/(?P<machinetype_id>\w+)/$', views.new_machine, name='new_machine'),
    url(r'^edit_machine/(?P<machine_id>\d+)/$', views.edit_machine, name='edit_machine'),
    url(r'^$', views.index, name='machines_index'),
]
