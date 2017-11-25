from django.conf.urls import url
from . import views

app_name = 'clients'
urlpatterns = [
    url(r'^personal/', views.change_personal_informations, name='personal_informations')
]
