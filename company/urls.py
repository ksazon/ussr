from django.conf.urls import url
from . import views

app_name = 'company'
urlpatterns = [
    url(r'^$', views.index, name='index'),
    url(r'^reservation/', views.reservation, name='reservation'),
    url(r'^get_calendar/', views.generate_calendar, name='get_calendar'),
#
#     url(r'^clients_all/$', views.clients_all, name='clients_all'),
#     url(r'^client/(?P<client_id>\d+)/$', views.client, name='client'),
#     url(r'^new_client/$', views.new_client, name='new_client'),
#     url(r'^edit_client/(?P<client_id>\d+)/$', views.edit_client, name='edit_client'),
#
#     url(r'^workers/$', views.workers, name='workers'),
#     url(r'^workers/(?P<worker_id>\d+)/$', views.worker, name='worker'),
#     url(r'^new_worker/$', views.new_worker, name='new_worker'),
#     url(r'^edit_worker/(?P<worker_id>\d+)/$', views.edit_worker, name='edit_worker'),
#
#     #url(r'^services/$', views.services, name='services'),
#     #url(r'^services/(?P<tservive_id>\d+)/$', views.service, name='service'),
#     url(r'^machinetypes/(?P<machinetype_id>\d+)/$', views.machinetype, name='machinetype'),
#     url(r'^machinetypes/$', views.machinetypes, name='machinetypes'),
#
#     #url(r'^machinetypes/(?P<machine_type>\d+)/$', views.machinetype, name='machinetype'),
#     url(r'^new_machinetype/$', views.new_machinetype, name='new_machinetype'),
#
#     url(r'^new_machine/(?P<machinetype_id>\d+)/$', views.new_machine, name='new_machine'),
#
#     url(r'^edit_machine/(?P<machine_id>\d+)/$', views.edit_machine, name='edit_machine'),


    #url(r'^clients/$', views.clients, name='clients'),
    #url(r'^add/$', views.add, name = 'add'),
    #url(r'^(?P<blockedReason>[a-zA-Z]+)/$', views.detail, name='blockedReason'),

]
