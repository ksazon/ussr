from django.conf.urls import url
from . import views

app_name = 'machines'
urlpatterns = [
    url(r'^machinetypes/', views.machinetypes, name='machinetypes'),
    url(r'^machinetype/(?P<machinetype_id>\w+)/', views.machinetype, name='machinetype'),
    url(r'^new_machinetype/$', views.new_machinetype, name='new_machinetype'),
    url(r'^new_machine/(?P<machinetype_id>\w+)/$', views.new_machine, name='new_machine'),
    url(r'^edit_machine/(?P<machine_id>\w+)/$', views.edit_machine, name='edit_machine'),
    url(r'^delete_machine/(?P<machine_id>\w+)/$', views.delete_machine, name='delete_machine'),
    url(r'^delete_machinetype/(?P<machinetype_id>\w+)/$', views.delete_machinetype, name='delete_machinetype'),
    url(r'^$', views.index, name='machines_index'),
]
