from django.conf.urls import url
from . import views

app_name = 'services'
urlpatterns = [
    url(r'^$', views.services, name='services'),
    # url(r'^machinetypes/(?P<machinetype_id>\d+)/$', views.machinetype, name='machinetype'),
    # # url(r'^machinetypes/(?P<machine_type>\d+)/$', views.machinetype, name='machinetype'),
    # url(r'^new_machinetype/$', views.new_machinetype, name='new_machinetype'),
    url(r'^add/$', views.addService, name='addService'),
    # url(r'^edit_machine/(?P<machine_id>\d+)/$', views.edit_machine, name='edit_machine'),
    # url(r'^$', views.index, name='index'),
]
