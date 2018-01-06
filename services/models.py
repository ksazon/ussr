from __future__ import unicode_literals

from django.db import models

#from company.models import CompanyBranch
#from workers.models import WoAbility

class SeDict(models.Model):
    id_se_dict = models.CharField(primary_key=True, max_length=10, verbose_name='Id')
    se_dict_name = models.CharField(unique=True, max_length=200, verbose_name='Nazwa')
    se_group = models.ForeignKey('SeGroupDict', blank=True, null=True, db_column='se_group', verbose_name='Grupa serwisów', related_name = 'services_in_group')
    base_price = models.DecimalField(blank=True, null=True, decimal_places=2, max_digits=8, verbose_name='Cena bazowa')
    location_type = models.ForeignKey('company.LocationType', models.DO_NOTHING, db_column='location_type', blank=True, null=True, verbose_name='Wymagany typ lokacji')
    # TODO do ukrycia
    avg_time = models.IntegerField(blank=True, null=True, verbose_name='Przeciętny czas wykonania')
    continous = models.BooleanField(default=True, verbose_name='Czy musi być wykonywane bez przerw')
    notes = models.CharField(max_length=400, blank=True, null=True, verbose_name='Uwagi')

    class Meta:
        managed = False
        db_table = 'se_dict'
        verbose_name = 'Serwis'
        verbose_name_plural = 'Słownik serwisów'

    def __str__(self):
        return str(self.id_se_dict)

class SeGroupDict(models.Model):
    id_se_group_dict = models.CharField(primary_key=True, max_length=10, verbose_name='Id')
    se_group_dict_name = models.CharField(unique=True, db_column='se_group_dict_name', max_length=200, verbose_name='Nazwa')

    class Meta:
        managed = False
        db_table = 'se_group_dict'
        verbose_name = 'Grupa serwisów'
        verbose_name_plural = 'Słownik grup serwisów'

    def __str__(self):
        return str(self.id_se_group_dict)

#nie korzystamy
class SeDiscount(models.Model):
    id_se_discount = models.AutoField(primary_key=True, verbose_name='Id')
    discount = models.ForeignKey('clients.DiscountDict', models.DO_NOTHING, db_column='discount', verbose_name='Zniżka')
    service = models.ForeignKey('Service', models.DO_NOTHING, db_column='service', verbose_name='Serwis')
    company_branch = models.ForeignKey('company.CompanyBranch', models.DO_NOTHING, db_column='company_branch', default='main', related_name = '+', verbose_name='Oddział')

    class Meta:
        managed = False
        db_table = 'se_discount'
        verbose_name = 'Zniżka na serwis'
        verbose_name_plural = 'Zniżki na serwis'

    def __str__(self):
        return str(self.id_se_discount)


class SeRequirement(models.Model):
    id_se_requirement = models.AutoField(primary_key=True, verbose_name='Id')
    service_code = models.ForeignKey(SeDict, models.DO_NOTHING, db_column='service_code', verbose_name='Serwis')
    machine_type = models.ForeignKey('machines.MachineType', models.DO_NOTHING, db_column='machine_type', blank=True, null=True, verbose_name='Typ maszyny')
    worker_ability = models.ForeignKey('workers.WoAbilityDict', models.DO_NOTHING, db_column='worker_ability', blank=True, null=True, related_name = '+', verbose_name='Umiejętność pracownika')
    location_type = models.ForeignKey('company.LocationType', models.DO_NOTHING, db_column='location_type', blank=True, null=True, verbose_name='Lokacja')
    # TODO do ukrycia
    qty = models.FloatField(blank=True, null=True, default=1, verbose_name='Ilość')
    # TODO do ukrycia
    activity_sort_order = models.IntegerField(blank=True, null=True, default=1, verbose_name='Numer porządkowy czynności')
    activity_name = models.CharField(max_length=200, blank=True, null=True, verbose_name='Nazwa czynności')
    time_minutes = models.IntegerField(blank=True, null=True, verbose_name='Czas trwania')

    class Meta:
        managed = False
        db_table = 'se_requirement'
        verbose_name = 'Wymaganie serwisu'
        verbose_name_plural = 'Wymagania serwisów'

    def __str__(self):
        return str(self.id_se_requirement)


class Service(models.Model):
    id_service = models.AutoField(primary_key=True, editable=False, verbose_name='Id')
    is_confirmed = models.NullBooleanField(default=False, verbose_name='Powierdzony')
    service_code = models.ForeignKey(SeDict, models.DO_NOTHING, db_column='service_code', verbose_name='Typ usługi')
    client = models.ForeignKey('clients.Client', models.DO_NOTHING, db_column='client', verbose_name='Klient')
    # TODO do ukrycia
    location = models.ForeignKey('company.Location', models.DO_NOTHING, db_column='location', blank=True, null=True, verbose_name='Lokacja')
    create_invoice = models.NullBooleanField(default=False, verbose_name='Utworzyć fakturę VAT')
    # TODO do ukrycia
    service_discount_amount = models.DecimalField(blank=True, null=True, decimal_places=2, max_digits=10, verbose_name='Kwota zniżki')
    # TODO do ukrycia
    service_discount_percent = models.FloatField(blank=True, null=True, verbose_name='Procent zniżki')
    min_start_datetime = models.DateTimeField(blank=True, null=True, verbose_name='Szukanie czasu od')
    planned_start = models.DateTimeField(blank=True, null=True, verbose_name='Planowany początek')
    planned_end = models.DateTimeField(blank=True, null=True, verbose_name='Planowane zakończenie')
    # TODO do ukrycia
    real_start = models.DateTimeField(blank=True, null=True, verbose_name='Rzeczywisty początek')
    # TODO do ukrycia
    real_end = models.DateTimeField(blank=True, null=True, verbose_name='Rzeczywiste zakończenie')
    # TODO do ukrycia
    reminder_sms_minutes = models.IntegerField(blank=True, null=True, default=-1, verbose_name='Ilość minut między SMS a rozpoczęciem')
    # TODO do ukrycia
    reminder_email_minutes = models.IntegerField(blank=True, null=True, default=-1, verbose_name='Ilość minut między e-mail a rozpoczęciem')
    # TODO do ukrycia
    finished_info_sms = models.NullBooleanField(default=True, verbose_name='SMS o ukończeniu')
    # TODO do ukrycia
    # test wykomnetowania pola
    finished_info_email = models.NullBooleanField(default=True, verbose_name='E-mail o ukończeniu')
    notes = models.CharField(max_length=400, blank=True, null=True, verbose_name='Uwagi')
    # TODO do ukrycia ??
    created_datetime = models.DateTimeField(blank=True, null=True, verbose_name='Data utworzenia')
    # TODO do ukrycia
    created_by = models.CharField(max_length=10, blank=True, null=True, verbose_name='Utworzone przez')
    # TODO do ukrycia
    confirmed_datetime = models.DateTimeField(blank=True, null=True, verbose_name='Potwierdzony początek usługi')
    # TODO do ukrycia
    confirmed_by = models.CharField(max_length=10, blank=True, null=True, verbose_name='Potwierdone przez')
    # TODO do ukrycia
    company_branch = models.ForeignKey('company.CompanyBranch', models.DO_NOTHING, db_column='company_branch', default='main', verbose_name='Oddział', related_name = '+')

    class Meta:
        managed = False
        db_table = 'service'
        verbose_name = 'Serwis'
        verbose_name_plural = 'Lista serwisów'

    # TODO poniżej test czy pobiera się czasem z unicode
    def __str__(self):
        return 'z __str__: ' + str(self.id_service)

    def __unicode__(self):
        return 'z __unicode__: ' + str(self.id_service)


#nie korzystamy
# TODO to w sumie jedna z pierwszych rzeczy do implementacji po zrobieniu podstaw
# nie jest trudne a pokazuje pewną automtyzację czynnyości, korzystanie z triggerów
# na sqlu - dodatkow punkty powinny za to pójść
# tu wchodzą zarówno usunięte umyślnie przez pracowników jak te sprzed jakiegoś okresu
class ServiceArchived(models.Model):
    id_service_archived = models.AutoField(primary_key=True)
    id_service = models.IntegerField()
    service_code = models.CharField(max_length=10)
    service_name = models.CharField(max_length=200, blank=True, null=True)
    service_discount_amount = models.FloatField(blank=True, null=True)
    service_discount_percent = models.FloatField(blank=True, null=True)
    client = models.IntegerField()
    client_first_name = models.CharField(max_length=20, blank=True, null=True)
    client_last_name = models.CharField(max_length=20, blank=True, null=True)
    client_name = models.CharField(max_length=200, blank=True, null=True)
    client_discount = models.FloatField(blank=True, null=True)
    location = models.IntegerField(blank=True, null=True)
    location_name = models.CharField(max_length=200, blank=True, null=True)
    planned_start = models.DateField(blank=True, null=True)
    planned_end = models.DateField(blank=True, null=True)
    real_start = models.DateField(blank=True, null=True)
    real_end = models.DateField(blank=True, null=True)
    reminder_sms_minutes = models.IntegerField(blank=True, null=True)
    reminder_email_minutes = models.IntegerField(blank=True, null=True)
    finished_info_sms = models.NullBooleanField()
    finished_info_email = models.NullBooleanField()
    notes = models.CharField(max_length=400, blank=True, null=True)
    created_datetime = models.DateTimeField(blank=True, null=True)
    created_by = models.CharField(max_length=10, blank=True, null=True)
    is_confirmed = models.NullBooleanField()
    confirmed_datetime = models.DateTimeField(blank=True, null=True)
    confirmed_by = models.CharField(max_length=10, blank=True, null=True)
    is_deleted = models.NullBooleanField()
    deleted_timestamp = models.DateTimeField(blank=True, null=True)
    deleted_by = models.CharField(max_length=10, blank=True, null=True)
    deleted_reason = models.CharField(max_length=400, blank=True, null=True)
    company_branch = models.ForeignKey('company.CompanyBranch', models.DO_NOTHING, db_column='company_branch', default='main', related_name = '+')
    min_start_datetime = models.DateTimeField(blank=True, null=True)

    class Meta:
        managed = False
        db_table = 'service_archived'
        verbose_name = 'Serwis archiwalny'
        verbose_name_plural = 'Archiwum serwisów'

    def __str__(self):
        return str(self.id_service_archived)
