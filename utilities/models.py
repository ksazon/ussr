from __future__ import unicode_literals

from django.db import models

#from company.models import CompanyBranch
#from services.models import Services
#from workers.models import Worker
# Create your models here.

class Address(models.Model):
    id_address = models.AutoField(primary_key=True, verbose_name='Id')
    # TODO do ukrycia w adminie
    prefered_contact_type = models.ForeignKey('ContactType', models.DO_NOTHING, db_column='prefered_contact_type', blank=True, null=True, verbose_name='Preferowany sposób kontaktu')
    # TODO do ukrycia w adminie
    email = models.CharField(unique=True, max_length=50, blank=True, null=True, verbose_name='E-mail')
    # TODO do ukrycia w adminie
    phone = models.CharField(max_length=20, blank=True, null=True, verbose_name='Telefon')
    street = models.CharField(max_length=100, blank=True, null=True, verbose_name='Ulica')
    house_no = models.CharField(max_length=5, blank=True, null=True, verbose_name='Numer domu')
    apartment_no = models.CharField(max_length=5, blank=True, null=True, verbose_name='Numer mieszkania')
    city = models.CharField(max_length=50, blank=True, null=True, verbose_name='Miejscowość')
    zip = models.CharField(max_length=10, blank=True, null=True, verbose_name='Kod pocztowy')
    country = models.ForeignKey('CountryDict', models.DO_NOTHING, db_column='country', blank=True, null=True, default='Polska', verbose_name='Państwo')
    notes = models.CharField(max_length=400, blank=True, null=True, verbose_name='Uwagi')

    class Meta:
        managed = False
        db_table = 'address'
        verbose_name = 'Adres'
        verbose_name_plural = 'Lista adresów'

    def __str__(self):
        def AddressString():
            ret = str(self.id_address)
            if (self.street): ret += ', ' + str(self.street)
            if (self.city): ret += ', ' + str(self.city)
            if (self.house_no): ret += ', ' + str(self.house_no)
            if (self.apartment_no): ret += '/' + str(self.apartment_no)
            return ret

        return AddressString()


class ContactType(models.Model):
    id_contact_type = models.CharField(primary_key=True, max_length=10, verbose_name='Id')
    contact_type_name = models.CharField(unique=True, max_length=100, blank=True, null=True, verbose_name='Nazwa')

    class Meta:
        managed = False
        db_table = 'contact_type'
        verbose_name = 'Typ kontaktu'
        verbose_name_plural = 'Słownik typów kontaktu'

    def __str__(self):
        return str(self.id_contact_type)

class CountryDict(models.Model):
    country = models.CharField(primary_key=True, max_length=100, verbose_name='Państwo')

    class Meta:
        managed = False
        db_table = 'country_dict'
        verbose_name = 'Państwo'
        verbose_name_plural = 'Słownik państw'

    def __str__(self):
        return self.country


#nie korzystamy
class Currrency(models.Model):
    id_currency = models.CharField(primary_key=True, max_length=10, verbose_name='Id')
    currency_name = models.CharField(unique=True, max_length=100, blank=True, null=True, verbose_name='Nazwa')
    # TODO można pokombinować po skończeniu podstawy - api ściągające aktualny kurs wymiarny z NBP,
    # nie takie trudne jak się wydaje
    ratio_to_main_currency = models.FloatField(blank=True, null=True, verbose_name='Kurs wymiany względem waluty głównej')

    class Meta:
        managed = False
        db_table = 'currrency'
        verbose_name = 'Waluta'
        verbose_name_plural = 'Słownik walut'

    def __str__(self):
        return self.id_currency

class DjangoMigrations(models.Model):
    app = models.CharField(max_length=255)
    name = models.CharField(max_length=255)
    applied = models.DateTimeField()

    class Meta:
        managed = False
        db_table = 'django_migrations'

class ResourcesUsage(models.Model):
    id_resources_usage = models.AutoField(primary_key=True, verbose_name='Id')
    service = models.ForeignKey('services.Service', models.DO_NOTHING, db_column='service', blank=True, null=True, related_name = '+', verbose_name='Serwis')
    machine = models.ForeignKey('machines.Machine', models.DO_NOTHING, db_column='machine', blank=True, null=True, verbose_name='Maszna')
    worker = models.ForeignKey('workers.Worker', models.DO_NOTHING, db_column='worker', blank=True, null=True, related_name = '+', verbose_name='Pracownik')
    # TODO do ukrycia
    #time_slot = models.ForeignKey('TimeSlotList', models.DO_NOTHING, db_column='time_slot', blank=True, null=True)
    start_timestamp = models.DateTimeField(db_column='start_timestamp', verbose_name='TS początku')
    # TODO dodać sprawdznie finish - start > 0 i, że data start = data koniec
    finish_timestamp = models.DateTimeField(db_column='finish_timestamp', verbose_name='TS końca')
    # TODO do ukrycia
    calendar_date = models.ForeignKey('WorkdayCalendar', models.DO_NOTHING, db_column='calendar_date', blank=True, null=True)
    company_branch = models.ForeignKey('company.CompanyBranch', models.DO_NOTHING, db_column='company_branch', default='main', related_name = '+', verbose_name='Oddział')


    class Meta:
        managed = False
        db_table = 'resources_usage'
        #unique_together = (('worker', 'time_slot', 'calendar_date'), ('machine', 'time_slot', 'calendar_date'),)
        verbose_name = 'Użycie zasobu'
        verbose_name_plural = 'Wykorzystanie zasobów'

    def __str__(self):
        return str(self.id_resources_usage)


class ResourcesUsageParams(models.Model):
    id_resources_usage_params = models.AutoField(db_column='resources_usage_params_id', primary_key=True, verbose_name='Id')
    # TODO do ukrycia
    allow_using_machine_without_service_days_before_date = models.IntegerField(default=100, verbose_name='Pozwalaj na planowanie z użyciem maszyny na okres dni przed serwisem')

    class Meta:
        managed = False
        db_table = 'resources_usage_params'
        verbose_name = 'Parametry modułu wykorzystania zasobów'
        verbose_name_plural = 'Parametry modułu wykorzystania zasobów'

    def __str__(self):
        return 'Parametry modułu wykorzystania zasobów'


class SexDict(models.Model):
    id_sex_dict = models.CharField(primary_key=True, max_length=1, verbose_name='Id')
    sex = models.CharField(max_length=10, blank=True, null=True, verbose_name='Nazwa')

    class Meta:
        managed = False
        db_table = 'sex_dict'
        verbose_name = 'Płeć'
        verbose_name_plural = 'Słownik płci'

    def __str__(self):
        return str(self.id_sex_dict)


# #nie korzystamy
# class TimeSlotList(models.Model):
#     id_time_slot = models.DateTimeField(primary_key=True)
#
#     class Meta:
#         managed = False
#         db_table = 'time_slot_list'
#         verbose_name = 'Sloty czasowe'
#         verbose_name_plural = 'Sloty czasowe'
#
#     def __str__(self):
#         return str(self.id_time_slot)

# # nie korzystamy
# class TimeSlotParams(models.Model):
#     id_time_slot_params = models.AutoField(db_column='time_slot_params_id', primary_key=True)
#     time_slot_minutes = models.IntegerField()
#
#     class Meta:
#         managed = False
#         db_table = 'time_slot_params'
#         verbose_name = 'Parametry slotow czasowych'
#         verbose_name_plural = 'Parametry slotow czasowych'
#
#     def __str__(self):
#         return 'Parametry slotow czasowych'


class WorkdayCalendar(models.Model):
    id_workday_calendar = models.DateField(primary_key=True, verbose_name='Data')
    is_workday = models.BooleanField(default=True, verbose_name='Dzień roboczy')
    work_start = models.TimeField(blank=True, null=True, verbose_name='Początek pracy')
    work_end = models.TimeField(blank=True, null=True, verbose_name='Koniec pracy')
    company_branch = models.ForeignKey('company.CompanyBranch', models.DO_NOTHING, db_column='company_branch', default='main', related_name = '+', verbose_name='Oddział')

    class Meta:
        managed = False
        db_table = 'workday_calendar'
        verbose_name = 'Kaledarz roboczy'
        verbose_name_plural = 'Kalendarze robocze'

    def __str__(self):
        return str(self.id_workday_calendar) + ', ' + str(self.company_branch)

# TODO dodać automatyczne tworzenie kalendarza na podstawie:
class WorkdayCalendarParams(models.Model):
    id_workday_calendar_params = models.AutoField(db_column='workday_calendar_params_id', primary_key=True, verbose_name='Id')
    default_workday_start_time = models.TimeField(blank=True, null=True, verbose_name='Domyślny początek czasu pracy')
    default_workday_end_time = models.TimeField(blank=True, null=True, verbose_name='Domyślny koniec czasu pracy')
    default_saturday_start_time = models.TimeField(blank=True, null=True, verbose_name='Domyślny początek czasu pracy w sobotę')
    default_saturday_end_time = models.TimeField(blank=True, null=True, verbose_name='Domyślny koniec czasu pracy w sobotę')
    # TODO do ukrycia
    # default_is_saturday_workday = models.BooleanField()

    class Meta:
        managed = False
        db_table = 'workday_calendar_params'
        verbose_name = 'Paramatry kalendarza'
        verbose_name_plural = 'Parametry kalendarza'

    def __str__(self):
        return str(self.id_workday_calendar_params)

#nie korzystamy
class ZipCodesDict(models.Model):
    id_zip_codes_dict = models.AutoField(primary_key=True, verbose_name='Id')
    country = models.ForeignKey(CountryDict, models.DO_NOTHING, db_column='country', verbose_name='Państwo')
    zip = models.CharField(max_length=10, blank=True, null=True, verbose_name='Kod pocztowy')
    city = models.CharField(max_length=100, blank=True, null=True, verbose_name='Miejscowość')

    class Meta:
        managed = False
        db_table = 'zip_codes_dict'
        verbose_name = 'Kod pocztowy'
        verbose_name_plural = 'Słownik kodów pocztowych'

    def __str__(self):
        return self.zip
