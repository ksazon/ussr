from django.conf.urls import url
from . import views

app_name = 'MainPage'
urlpatterns = [
    url(r'^$', views.index, name='index'),
    url(r'^branches/', views.branches, name='branches'),
    url(r'^about/', views.about, name='about'),
    url(r'^directions/', views.directions, name='directions'),
    url(r'^contact_form/', views.contact_form, name='contact_form'),
#    url(r'^test/', views.LookupView, name='lookup'),
    ]
