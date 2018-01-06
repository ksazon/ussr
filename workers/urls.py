from django.conf.urls import url
from . import views
from clients.views import clientCRUDlist

app_name = 'workers'
urlpatterns = [
    url(r'^$', views.employee_main, name='employee_index'),
    url(r'^woNotifications/', views.woNotifications, name='woNotifications'),
    url(r'^wRaports/', views.wRaports, name='wRaports'),
    url(r'^wRaport1/', views.wRaport1, name='wRaport1'),
    url(r'^wRaport2/', views.wRaport2, name='wRaport2'),
    url(r'^wRaport3/', views.wRaport3, name='wRaport3'),
    url(r'^woNotification/(?P<woNotification_id>\w+)/', views.woNotification, name='woNotification'),
    url(r'^edit_woNotification/(?P<woNotification_id>\w+)/$', views.edit_woNotification, name='edit_woNotification'),
    url(r'^new_woNotification/', views.new_woNotification, name='new_woNotification'),
    url(r'^personal/', views.change_personal_informations, name='personal_informations'),
    url(r'^clients/', clientCRUDlist, name='client_list')

]
