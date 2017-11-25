from django.db import models
from django.contrib.auth.models import User


#from services.models import Service
#from company.models import CompanyBranch
#from utilities.models import Currency, Address, SexDict

# Create your models here.

#nie korzystamy
class ClBlockedReasonDict(models.Model):
    id_cl_blocked_reason_dict = models.CharField(primary_key=True, max_length=10, verbose_name='Id')
    blocked_reason_name = models.CharField(unique=True, max_length=200, blank=True, null=True, verbose_name='Nazwa')

    class Meta:
        managed = False
        db_table = 'cl_blocked_reason_dict'
        verbose_name = "Powód blokady"
        verbose_name_plural = "Powody blokady"

    def __str__(self):
        return "[" + self.id_cl_blocked_reason_dict + "]" + " " + self.blocked_reason_name


#nie korzystamy
class ClCommunicationLog(models.Model):
    id_cl_communication_log = models.AutoField(primary_key=True, verbose_name='Id')
    client = models.ForeignKey('Client', models.DO_NOTHING, db_column='client', blank=True, null=True, verbose_name='Klient')
    communication_reason = models.ForeignKey('ClCommunicationReason', models.DO_NOTHING, db_column='communication_reason', blank=True, null=True, verbose_name='Powód')
    service = models.ForeignKey('services.Service', models.DO_NOTHING, db_column='service', blank=True, null=True, related_name = '+', verbose_name='Serwis')
    contact_type = models.ForeignKey('utilities.ContactType', models.DO_NOTHING, db_column='contact_type', verbose_name='Medium')
    contact_address = models.CharField(max_length=100, blank=True, null=True, verbose_name='Docelowy numer/mail')
    message_body = models.TextField(blank=True, null=True, verbose_name='Treść')
    minutes_before_action = models.IntegerField(blank=True, null=True, verbose_name='Minut przed serwisem')
    notes = models.IntegerField(blank=True, null=True, verbose_name='Notatki')
    created_datetime = models.DateTimeField(blank=True, null=True, verbose_name='TS utworzenia')
    created_by = models.CharField(max_length=10, blank=True, null=True, verbose_name='Utworzone przez')
    company_branch = models.ForeignKey('company.CompanyBranch', models.DO_NOTHING, db_column='company_branch', default='main', related_name = '+', verbose_name='Oddział')

    class Meta:
        managed = False
        db_table = 'cl_communication_log'
        verbose_name = 'Komunikacja z klientem'
        verbose_name_plural  = 'Log komunikacji z klientami'

    def __str__(self):
        return str(self.id_cl_communication_log)


#nie korzystamy
class ClCommunicationReason(models.Model):
    id_client_communication_reason = models.CharField(primary_key=True, max_length=10, verbose_name='Id')
    reason_name = models.CharField(unique=True, max_length=100, blank=True, null=True, verbose_name='Nazwa')

    class Meta:
        managed = False
        db_table = 'cl_communication_reason'
        verbose_name = "Powód komunikacji"
        verbose_name_plural = "Słownik powodow komunikacji"

    def __str__(self):
        return str(self.id_client_communication_reason)


#nie korzystamy
class ClDiscount(models.Model):
    id_cl_discount = models.AutoField(primary_key=True, verbose_name='Id')
    client = models.ForeignKey('Client', models.DO_NOTHING, db_column='client', verbose_name='Klient')
    discount = models.ForeignKey('DiscountDict', models.DO_NOTHING, db_column='discount', verbose_name='Typ zniżki')
    company_branch = models.ForeignKey('company.CompanyBranch', models.DO_NOTHING, db_column='company_branch', default='main', related_name = '+', verbose_name='Oddział')

    class Meta:
        managed = False
        db_table = 'cl_discount'
        unique_together = (('client', 'discount'),)
        verbose_name = "Zniżka dla klienta"
        verbose_name_plural = "Zniżki dla klientów"

    def __str__(self):
        return str(self.id_cl_discount)

# TODO nie korzystamy
class ClParams(models.Model):
    # w rzeczywistości powinien być tylko 1 na oddział aktywny, czyli odział mógłby być primary key
    # ale dobra - warto chociaż ten usunąć z widoku
    id_cl_params = models.AutoField(db_column='cl_params_id', primary_key=True, verbose_name='Id')
    max_debt = models.FloatField(verbose_name='Zadłużenie powyżej którego nie będą realizowane zadłużenia, 0 - brak')
    allow_new_no_contact = models.BooleanField(verbose_name='Pozwalaj na dodawanie nowego klienta bez kontaktu')
    default_reminder_sms_minutes = models.IntegerField(verbose_name='Domyślna ilość minut między początkiem wykonania usługi a SMS-em informującym')
    default_reminder_email_minutes = models.IntegerField(verbose_name='Domyślna ilość minut między początkiem wykonania usługi a email-em informującym')
    default_finished_info_sms = models.BooleanField(verbose_name='Domyślnie wysyłaj SMS informujący o zakończeniu usługi')
    default_finished_info_email = models.BooleanField(verbose_name='Domyślnie wysyłaj e-mail informujący o zakończeniu usługi')
    max_worktime_wo_conf_minutes = models.IntegerField(verbose_name='Maksymalny czas pracy bez potwierdzenia')
    default_currency = models.ForeignKey('utilities.Currrency', models.DO_NOTHING, db_column='default_currency', related_name = '+', verbose_name='Domyślna waluta')
    company_branch = models.ForeignKey('company.CompanyBranch', models.DO_NOTHING, db_column='company_branch', default='main', related_name = '+', verbose_name='Oddział')

    class Meta:
        managed = False
        db_table = 'cl_params'
        verbose_name = "Parametry modułu klienta"
        verbose_name_plural = "Parametry modułu klienta"

    def __str__(self):
        return str(self.company_branch)


class ClPayment(models.Model):
    id_cl_payment = models.AutoField(primary_key=True, verbose_name='Id')
    is_closed = models.NullBooleanField(verbose_name='Zamknięta')
    payment_name = models.CharField(max_length=200, blank=True, null=True, verbose_name='Tytuł płatności')
    client = models.ForeignKey('Client', models.DO_NOTHING, db_column='client', verbose_name='Klient')
    is_invoice = models.NullBooleanField(verbose_name='Jest fakturą')
    # address = models.ForeignKey('utilities.Address', models.DO_NOTHING, db_column='address', related_name = '+', verbose_name='Adres')
    # TODO pokombinować czy nie powinno być autofield
    invoice_voucher = models.CharField(unique=True, max_length=15, blank=True, null=True, verbose_name='Numer faktury')
    payment_sum = models.FloatField(blank=True, null=True, verbose_name='Kwota brutto do zapłaty')
    # TODO do ukrycia
    paid_amount = models.FloatField(blank=True, null=True, verbose_name='Suma dotychczasowych wpłat')
    # TODO do ukrycia
    currency = models.ForeignKey('utilities.Currrency', models.DO_NOTHING, db_column='currency', related_name = '+', verbose_name='Waluta')
    # TODO przy kolejnych trzech polach pokombinować nad "django signals"
    paid_datetime = models.DateTimeField(blank=True, null=True, verbose_name='Data opłacenia')
    posted_datetime = models.DateTimeField(blank=True, null=True, verbose_name='Data wystawienia')
    due_date = models.DateField(blank=True, null=True, verbose_name='Należy opłacić do')
    notes = models.CharField(max_length=400, blank=True, null=True, verbose_name='Notatki')
    created_datetime = models.DateTimeField(blank=True, null=True, verbose_name='TS utworzenia')
    created_by = models.CharField(max_length=10, blank=True, null=True, verbose_name='Utworzone przez')
    company_branch = models.ForeignKey('company.CompanyBranch', models.DO_NOTHING, db_column='company_branch', default='main', related_name = '+', verbose_name='Oddział')

    class Meta:
        managed = False
        db_table = 'cl_payment'
        verbose_name = 'Płatność klienta'
        verbose_name_plural = 'Platności klientów'

    def __str__(self):
        return str(self.id_cl_payment)


class ClPaymentLine(models.Model):
    id_cl_payment_line = models.AutoField(primary_key=True, verbose_name='Id')
    payment = models.ForeignKey(ClPayment, models.DO_NOTHING, db_column='payment', verbose_name='Do płatności')
    service = models.ForeignKey('services.Service', models.DO_NOTHING, db_column='service', related_name = '+', verbose_name='Serwis')
    # TODO można ukryć i tytuł linii ciągnąć z nazwy serwisu
    text_on_invoice = models.CharField(max_length=200, verbose_name='Tytuł linii')
    qty = models.FloatField(blank=True, null=True, verbose_name='Ilość')
    final_price = models.FloatField(blank=True, null=True, verbose_name='Cena po zniżkach')
    # TODO do ukrycia
    currency = models.ForeignKey('utilities.Currrency', models.DO_NOTHING, db_column='currency', related_name = '+', verbose_name='Waluta')
    company_branch = models.ForeignKey('company.CompanyBranch', models.DO_NOTHING, db_column='company_branch', default='main', related_name = '+', verbose_name='Oddział')

    class Meta:
        managed = False
        db_table = 'cl_payment_line'
        verbose_name = 'Linia płatności klienta'
        verbose_name_plural = 'Linie płatności klientów'

    def __str__(self):
        return str(self.id_cl_payment_line)


# TODO nie korzystamy, usunąć z DB
# class ClUnconfirmed(models.Model):
#     cl_unconfirmed = models.AutoField(primary_key=True)
#     first_name = models.CharField(max_length=20, blank=True, null=True)
#     last_name = models.CharField(max_length=20, blank=True, null=True)
#     client_name = models.CharField(max_length=200, blank=True, null=True)
#     created_datetime = models.DateTimeField(blank=True, null=True)
#     ip_address = models.CharField(max_length=20, blank=True, null=True)
#     email = models.CharField(max_length=50, blank=True, null=True)
#     phone = models.CharField(max_length=20, blank=True, null=True)
#     default_company_branch = models.ForeignKey('company.CompanyBranch', models.DO_NOTHING, db_column='default_company_branch', related_name = '+')
#
#     class Meta:
#         managed = False
#         db_table = 'cl_unconfirmed'
#         verbose_name = 'Niepotwierdzony klient'
#         verbose_name_plural = 'Niepotwierdzeni klienci'
#
#     def __str__(self):
#         return str(self.cl_unconfirmed)


class Client(models.Model):
    id_client = models.AutoField(primary_key=True, verbose_name='Id')
    sex = models.ForeignKey('utilities.SexDict', models.DO_NOTHING, db_column='sex', blank=True, null=True, related_name = '+', verbose_name='Płeć')
    first_name = models.CharField(max_length=20, blank=True, null=True, verbose_name='Imię')
    last_name = models.CharField(max_length=20, blank=True, null=True, verbose_name='Nazwisko')
    is_company = models.NullBooleanField(default=False, verbose_name='Jest firmą')
    client_name = models.CharField(max_length=200, blank=True, null=True, verbose_name='Nazwa firmy')
    nip = models.CharField(max_length=11, blank=True, null=True, verbose_name='NIP')
    address = models.ForeignKey('utilities.Address', models.DO_NOTHING, db_column='address', blank=True, null=True, related_name = '+', verbose_name='Adres')
    is_blocked = models.NullBooleanField(default=False, verbose_name='Zablokowany')
    # TODO schowac w adminie
    blocked_reason = models.ForeignKey(ClBlockedReasonDict, models.DO_NOTHING, blank=True, null=True, verbose_name='Powód blokady')
    # TODO schowac w adminie
    blocked_notes = models.CharField(max_length=400, blank=True, null=True, verbose_name='Notatki do blokady')
    # TODO schowac w adminie ???
    default_invoice = models.NullBooleanField(default=True, verbose_name='Domyślnie wystawiaj fakturę')
    # TODO schowac w adminie
    default_reminder_sms_minutes = models.IntegerField(blank=True, null=True, verbose_name='Domyślna różnica czasu przypomnienia SMS')
    is_confirmed = models.NullBooleanField(default=True, verbose_name='Zatwierdzony')
    # TODO schowac w adminie
    is_rejected = models.NullBooleanField(default=False, verbose_name='Odrzucony')
    # TODO schowac w adminie
    ip_address = models.CharField(max_length=20, blank=True, null=True, verbose_name='Adres IP')
    # TODO schowac w adminie
    default_reminder_email_minutes = models.IntegerField(blank=True, null=True, verbose_name='Domyślna różnica czasu przypomnienia e-mail')
    # TODO schowac w adminie
    default_finished_info_sms = models.IntegerField(blank=True, null=True, verbose_name='Domyślnie wysyłaj SMS z powiadomnieniem o zakończeniu')
    # TODO schowac w adminie
    default_finished_info_email = models.IntegerField(blank=True, null=True, verbose_name='Domyślnie wysyłaj e-mail z powiadomnieniem o zakończeniu')
    # TODO schowac w adminie, to w sumie bardziej info wynikające z sumy zniżek, więc nie powinno być do edycji
    client_discount_percent_sum = models.FloatField(blank=True, null=True, verbose_name='Sumaryczna procentowa zaniżka klienta')
    notes = models.CharField(max_length=400, blank=True, null=True, verbose_name='Uwagi')
    default_company_branch = models.ForeignKey('company.CompanyBranch', models.DO_NOTHING, db_column='default_company_branch', default='main', related_name = '+', verbose_name='Oddział')
    client_user_login = models.OneToOneField('auth.User', models.DO_NOTHING, db_column = 'client_user_login', blank=True, null=True, verbose_name='Login')


    class Meta:
        managed = False
        db_table = 'client'
        verbose_name = 'Klinet'
        verbose_name_plural = 'Lista klientów'

    def __str__(self):
        return str("{0} {1} {2}".format(self.first_name, self.last_name, self.client_name))
        ##return 'abc'


#nie korzystamy
class DiscountDict(models.Model):
    id_discount_dict = models.CharField(primary_key=True, max_length=10, verbose_name='Id')
    discount_name = models.CharField(max_length=100, blank=True, null=True, verbose_name='Nazwa')
    scope = models.ForeignKey('DiscountScope', models.DO_NOTHING, db_column='scope', verbose_name='Zakres')
    # poniższe dwa pola powinny się wzajemnie wykluczać
    discount_amount = models.FloatField(blank=True, null=True, verbose_name='Kwota zniżki')
    discount_percent = models.FloatField(blank=True, null=True, verbose_name='Procent zniżki')
    valid_from = models.DateField(blank=True, null=True, verbose_name='Aktywne od')
    valid_to = models.DateField(blank=True, null=True, verbose_name='Aktywne do')

    class Meta:
        managed = False
        db_table = 'discount_dict'
        verbose_name = 'Zniżka'
        verbose_name_plural = 'Słownik zniżek'

    def __str__(self):
        return self.discount_name


#nie korzystamy
class DiscountScope(models.Model):
    id_discount_scope = models.CharField(primary_key=True, max_length=10, verbose_name='Id')

    class Meta:
        managed = False
        db_table = 'discount_scope'
        verbose_name = 'Zakres zniżki'
        verbose_name_plural = 'Słownik zakresów zniżek'

    def __str__(self):
        return self.id_discount_scope
