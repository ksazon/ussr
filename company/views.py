from django.http import Http404
from django.shortcuts import render, get_object_or_404
from django.http import HttpResponseRedirect, Http404
from django.core.urlresolvers import reverse

from .models import *
# from .forms import MachineTypeForm, MachineForm, WorkerForm, ClientForm
#
def index(request):
    """Strona główna dla aplikacji ussr."""
    return render(request, 'company/index.html')
#
# def machinetypes(request):
#     machinetypes = MachineType.objects.all()
#     context = {'machinetypes': machinetypes}
#     return render(request, 'company/machinetypes.html', context)
#
# def machinetype(request, machinetype_id):
#     machinetype = MachineType.objects.get(id=machinetype_id)
#     machines = machinetype.machine_set.all()
#     context = {'machinetype': machinetype, 'machines': machines}
#     return render(request, 'company/machinetype.html', context)
#
# def new_machinetype(request):
#     if request.method != 'POST':
#         # Nie przekazano żadnych danych, należy utworzyć pusty formularz.
#         form = MachineTypeForm()
#     else:
#         # Przekazano dane za pomocą żądania POST, należy je przetworzyć.
#         form = MachineTypeForm(request.POST)
#         if form.is_valid():
#             #new_topic = form.save(commit=False)
#             #new_topic.owner = request.user
#             #new_topic.save()
#             form.save()
#             return HttpResponseRedirect(reverse('company:machinetypes'))
#
#     context = {'form': form}
#     return render(request, 'company/new_machinetype.html', context)
#
# def new_machine(request, machinetype_id):
#     """Dodanie nowego wpisu dla określonego tematu."""
#     machinetype = MachineType.objects.get(id=machinetype_id)
#
#     if request.method != 'POST':
#         # Nie przekazano żadnych danych, należy utworzyć pusty formularz.
#         form = MachineForm()
#     else:
#         # Przekazano dane za pomocą żądania POST, należy je przetworzyć.
#         form = MachineForm(data=request.POST)
#         if form.is_valid():
#             new_machine = form.save(commit=False)
#             new_machine.machinetype = machinetype
#             new_machine.save()
#             return HttpResponseRedirect(reverse('company:machinetype',
#                                         args=[machinetype_id]))
#
#     context = {'machinetype': machinetype, 'form': form}
#     return render(request, 'company/new_machine.html', context)
#
# def edit_machine(request, machine_id):
#     """Edycja istniejącego wpisu."""
#     machine = Machine.objects.get(id=machine_id)
#     machinetype = machine.machinetype
#
#     if request.method != 'POST':
#         # Żądanie początkowe, wypełnienie formularza aktualną treścią wpisu.
#         form = MachineForm(instance=machine)
#     else:
#         # Przekazano dane za pomocą żądania POST, należy je przetworzyć.
#         form = MachineForm(instance=machine, data=request.POST)
#         if form.is_valid():
#             form.save()
#             return HttpResponseRedirect(reverse('company:machinetype',
#                                         args=[machinetype.id]))
#
#     context = {'machine': machine, 'machinetype': machinetype, 'form': form}
#     return render(request, 'company/edit_machine.html', context)
#
# def workers(request):
#     workers = Worker.objects.all()
#     context = {'workers': workers}
#     return render(request, 'company/workers.html', context)
#
# def worker(request, worker_id):
#     worker = Worker.objects.get(id=worker_id)
#     context = {'worker': worker}
#     return render(request, 'company/worker.html', context)
#
# def new_worker(request):
#     if request.method != 'POST':
#         # Nie przekazano żadnych danych, należy utworzyć pusty formularz.
#         form = WorkerForm()
#     else:
#         # Przekazano dane za pomocą żądania POST, należy je przetworzyć.
#         form = WorkerForm(request.POST)
#         if form.is_valid():
#             #new_topic = form.save(commit=False)
#             #new_topic.owner = request.user
#             #new_topic.save()
#             form.save()
#             return HttpResponseRedirect(reverse('company:workers'))
#
#     context = {'form': form}
#     return render(request, 'company/new_worker.html', context)
#
# def edit_worker(request, worker_id):
#     """Edycja istniejącego wpisu."""
#     worker = Worker.objects.get(id=worker_id)
#
#     if request.method != 'POST':
#         # Żądanie początkowe, wypełnienie formularza aktualną treścią wpisu.
#         form = WorkerForm(instance=worker)
#     else:
#         # Przekazano dane za pomocą żądania POST, należy je przetworzyć.
#         form = WorkerForm(instance=worker, data=request.POST)
#         if form.is_valid():
#             form.save()
#             return HttpResponseRedirect(reverse('company:worker',
#                                         args=[worker.id]))
#
#     context = {'worker': worker, 'form': form}
#     return render(request, 'company/edit_worker.html', context)
#
# def clients_all(request):
#     clients_all = Client.objects.all()
#     context = {'clients_all': clients_all}
#     return render(request, 'company/clients.html', context)
#
# def client(request, client_id):
#     client = Client.objects.get(id_client=client_id)
#     context = {'client': client}
#     return render(request, 'company/client.html', context)
#
# def new_client(request):
#     if request.method != 'POST':
#         # Nie przekazano żadnych danych, należy utworzyć pusty formularz.
#         form = ClientForm()
#     else:
#         # Przekazano dane za pomocą żądania POST, należy je przetworzyć.
#         form = ClientForm(request.POST)
#         if form.is_valid():
#             #new_topic = form.save(commit=False)
#             #new_topic.owner = request.user
#             #new_topic.save()
#             form.save()
#             return HttpResponseRedirect(reverse('company:clients_all'))
#
#     context = {'form': form}
#     return render(request, 'company/new_client.html', context)
#
# def edit_client(request, client_id):
#     """Edycja istniejącego wpisu."""
#     client = Client.objects.get(id_client=client_id)
#
#     if request.method != 'POST':
#         # Żądanie początkowe, wypełnienie formularza aktualną treścią wpisu.
#         form = ClientForm(instance=client)
#     else:
#         # Przekazano dane za pomocą żądania POST, należy je przetworzyć.
#         form = ClientForm(instance=client, data=request.POST)
#         if form.is_valid():
#             form.save()
#             return HttpResponseRedirect(reverse('company:client',
#                                         args=[client.id_client]))
#
#     context = {'client': client, 'form': form}
#     return render(request, 'company/edit_client.html', context)
#
# # # from django.shortcuts import render
# #
# # # Create your views here.
# #
# # from django.http import HttpResponse
# # # from django.template import loader
# # from django.shortcuts import render, get_object_or_404
# # from .models import ClBlockedReasonDict
# # from .sqlSelect import sqlSelect
# #
# # def index(request):
# #     plannedServicesList = ClBlockedReasonDict.objects.order_by('id_cl_blocked_reason_dict')[:5]
# #     # template = loader.get_template('company/index.html')
# #     context = {
# #         'plannedServicesList': plannedServicesList,
# #     }
# #     # output = ', '.join([s.blocked_reason_name for s in testList])
# #     # return HttpResponse(template.render(context, request))
# #     return render(request, 'company/index.html', context)
# #
# # def detail(request, blockedReason):
# #     # response = "dupa %s"
# #     # return HttpResponse(response % sth)
# #     blockedReasonX = get_object_or_404(ClBlockedReasonDict, pk=blockedReason)
# #     return render(request, 'company/detail.html', {'blockedReason': blockedReasonX})
# #
# # def add(request):
# #     sqlResult = sqlSelect()
# #     return render(request, 'company/sql.html', {'queryResult': sqlResult})
