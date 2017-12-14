from django.conf.urls import url
from . import views

app_name = 'services'
urlpatterns = [
    url(r'^$', views.services, name='services'),
    url(r'^(?P<segroupdict_id>\w+)?/?$', views.service, name='service'),
]
