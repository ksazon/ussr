from django.db import models
from django.contrib.auth.models import User


# Create your models here.

class WoAbility(models.Model):
    id_wo_ability = models.AutoField(primary_key=True, verbose_name='Id')
    worker = models.ForeignKey('Worker', models.DO_NOTHING, db_column='worker', verbose_name='Pracownik')
    worker_ability = models.ForeignKey('WoAbilityDict', models.DO_NOTHING, db_column='worker_ability', verbose_name='Umiejętność')
    notes = models.CharField(max_length=400, blank=True, null=True, verbose_name='Uwagi')
    company_branch = models.ForeignKey('company.CompanyBranch', models.DO_NOTHING, db_column='company_branch', default='main', verbose_name='Oddział')

    class Meta:
        managed = False
        db_table = 'wo_ability'
        verbose_name = 'Umiejetność pracownika'
        verbose_name_plural = 'Umiejetności pracowników'

    def __str__(self):
        return str(self.worker) + ' - ' + str(self.worker_ability)


class WoAbilityDict(models.Model):
    id_wo_ability_dict = models.CharField(primary_key=True, max_length=10, verbose_name='Id')
    ability_name = models.CharField(max_length=200, verbose_name='Nazwa')
    # TODO do ukrycia
    ability_group = models.ForeignKey('WoAbilityGroupDict', models.DO_NOTHING, db_column='ability_group', blank=True, null=True, verbose_name='Grupa umiejętności')

    class Meta:
        managed = False
        db_table = 'wo_ability_dict'
        verbose_name = 'Umiejętność'
        verbose_name_plural = 'Słownik umiejętności'

    def __str__(self):
        return str(self.id_wo_ability_dict)


# TODO nie korzystamy
class WoAbilityGroupDict(models.Model):
    id_wo_ablility_group_dict = models.CharField(primary_key=True, max_length=10, verbose_name='Id')
    ability_group_name = models.CharField(unique=True, max_length=200, verbose_name='Name')

    class Meta:
        managed = False
        db_table = 'wo_ability_group_dict'
        verbose_name = 'Grupa umiejętności'
        verbose_name_plural = 'Słownik grup umiejętności'

    def __str__(self):
        return str(self.id_wo_ablility_group_dict)


# TODO nie korzystamy
class WoAbsence(models.Model):
    id_wo_absence = models.IntegerField(primary_key=True, verbose_name='Id')
    worker = models.ForeignKey('Worker', models.DO_NOTHING, db_column='worker', verbose_name='Pracownik')
    absence_type = models.ForeignKey('WoAbsenceType', models.DO_NOTHING, db_column='absence_type', verbose_name='Typ nieobecności')
    #TODO pomyśleć czy dwa następne pola nie lepiej w date
    start_datetime = models.DateTimeField(blank=True, null=True, verbose_name='Początek absencji')
    end_datetime = models.DateTimeField(blank=True, null=True, verbose_name='Koniec absencji')
    workdays = models.IntegerField(blank=True, null=True, verbose_name='Ilość dni roboczych')
    hours = models.FloatField(blank=True, null=True, verbose_name='Ilość godzin')
    notes = models.CharField(max_length=400, blank=True, null=True, verbose_name='Uwagi')
    company_branch = models.ForeignKey('company.CompanyBranch', models.DO_NOTHING, db_column='company_branch', default='main', verbose_name='Oddział')

    class Meta:
        managed = False
        db_table = 'wo_absence'
        verbose_name = 'Absencja pracownika'
        verbose_name_plural = 'Absencje pracowników'

    def __str__(self):
        return str(id_wo_absence)


# TODO nie korzystamy
class WoAbsenceType(models.Model):
    id_wo_absence_type = models.CharField(primary_key=True, max_length=10, verbose_name='Id')
    absence_name = models.CharField(unique=True, max_length=200, blank=True, null=True, verbose_name='Nazwa')

    class Meta:
        managed = False
        db_table = 'wo_absence_type'
        verbose_name = 'Absencja'
        verbose_name_plural = 'Słownik absencji'

    def __str__(self):
        return str(self.id_wo_absence_type)


# TODO nie korzystamy
class WoGroup(models.Model):
    id_wo_group = models.AutoField(primary_key=True, verbose_name='Id')
    worker = models.ForeignKey('Worker', models.DO_NOTHING, db_column='worker', verbose_name='Pracownik')
    worker_group = models.ForeignKey('WoGroupDict', models.DO_NOTHING, db_column='worker_group', verbose_name='Grupa')
    company_branch = models.ForeignKey('company.CompanyBranch', models.DO_NOTHING, db_column='company_branch', default='main', verbose_name='Oddział')

    class Meta:
        managed = False
        db_table = 'wo_group'
        verbose_name = 'Grupa - pracownik'
        verbose_name_plural = 'Grupy pracowników'

    def __str__(self):
        return str(self.worker_group) + " " + str(self.worker)


# TODO nie korzystamy
class WoGroupDict(models.Model):
    id_wo_group_dict = models.CharField(primary_key=True, max_length=10, verbose_name='Id')
    worker_group_name = models.CharField(max_length=200, blank=True, null=True, verbose_name='Nazwa')

    class Meta:
        managed = False
        db_table = 'wo_group_dict'
        verbose_name = 'Grupa (nazwa słownikowa)'
        verbose_name_plural = 'Słownik grupy pracowników'

    def __str__(self):
        return str(self.id_wo_group_dict)


# TODO nie korzystamy
class WoGroupPrivilege(models.Model):
    id_wo_group_privilege = models.AutoField(primary_key=True, verbose_name='Id')
    worker_group = models.ForeignKey(WoGroupDict, models.DO_NOTHING, db_column='worker_group', verbose_name='Grupa pracowników')
    privilege = models.ForeignKey('WoPrivilegeDict', models.DO_NOTHING, verbose_name='Uprawnienia')
    # a to nawet nie pamiętam  po co pisałem
    view_id = models.CharField(max_length=100, blank=True, null=True)
    privilege_level = models.ForeignKey('WoPrivilegeLevelDict', models.DO_NOTHING, db_column='privilege_level', verbose_name='Poziom uprawnienia')

    class Meta:
        managed = False
        db_table = 'wo_group_privilege'
        unique_together = (('worker_group', 'privilege'),)
        verbose_name = 'Uprawnienie grupy'
        verbose_name_plural = 'Uprawnienia grupy'

    def __str__(self):
        return str(self.privilage) + " " + str(self.worker_group)


# TODO nie korzystamy, a mogłoby nabić parę punkótw - auto wysyłające się informacje do pracownika
# po jakiś eventach
class WoNotification(models.Model):
    id_wo_notification = models.AutoField(primary_key=True, verbose_name='Id')
    worker = models.ForeignKey('Worker', models.DO_NOTHING, db_column='worker', verbose_name='Pracownik')
    worker_group = models.ForeignKey(WoGroupDict, models.DO_NOTHING, db_column='worker_group', verbose_name='Grupa pracowników')
    notification_subject = models.CharField(max_length=200, blank=True, null=True, verbose_name='Temat przypomnienia')
    notification_text = models.CharField(max_length=400, blank=True, null=True, verbose_name='Tekst przypomnienia')
    severity = models.IntegerField(blank=True, null=True, verbose_name='Ważność')
    marked_as_read = models.NullBooleanField(verbose_name='Oznaczone jako przeczytane')
    company_branch = models.ForeignKey('company.CompanyBranch', models.DO_NOTHING, db_column='company_branch', default='main', verbose_name='Oddział')

    class Meta:
        managed = False
        db_table = 'wo_notification'
        verbose_name = 'Alert pracownika'
        verbose_name_plural = 'Alerty pracownika'

    def __str__(self):
        return str(self.id_wo_notification)


# TODO nie korzystamy
class WoPrivilegeDict(models.Model):
    id_wo_privilige_dict = models.IntegerField(primary_key=True, verbose_name='Id')
    privilege_name = models.CharField(unique=True, max_length=200, blank=True, null=True, verbose_name='Nazwa')

    class Meta:
        managed = False
        db_table = 'wo_privilege_dict'
        verbose_name = 'Uprawnienie pracownika'
        verbose_name_plural = 'Słownik uprawnień pracowników'

    def __str__(self):
        return str(self.id_wo_privilige_dict)


# TODO nie korzystamy
class WoPrivilegeLevelDict(models.Model):
    id_wo_privilege_level_dict = models.CharField(primary_key=True, max_length=10, verbose_name='Id')

    class Meta:
        managed = False
        db_table = 'wo_privilege_level_dict'
        verbose_name = 'Poziom uprawnienia pracownika'
        verbose_name_plural = 'Słownik poziomów uprawnień pracownika'

    def __str__(self):
        return str(self.id_sex_dict)


# TODO nie korzystamy
class WoUser(models.Model):
    app_user = models.CharField(primary_key=True, max_length=10, verbose_name='Użytkownik')
    worker = models.ForeignKey('Worker', models.DO_NOTHING, db_column='worker', verbose_name='Pracownik')

    class Meta:
        managed = False
        db_table = 'wo_user'
        verbose_name = 'Pracownik - user'
        verbose_name_plural = 'Relacje pracownicy - userzy'

    def __str__(self):
        return self.app_user + ' ' + self.worker


class Worker(models.Model):
    id_worker = models.AutoField(primary_key=True, verbose_name='Id')
    first_name = models.CharField(max_length=20, blank=True, null=True, verbose_name='Imię')
    last_name = models.CharField(max_length=20, blank=True, null=True, verbose_name='Nazwisko')
    worker_title = models.CharField(max_length=200, blank=True, null=True, verbose_name='Stanowisko')
    active = models.NullBooleanField(default=True, verbose_name='Aktywny')
    address = models.ForeignKey('utilities.Address', models.DO_NOTHING, db_column='address', blank=True, null=True, verbose_name='Adres')
    notes = models.CharField(max_length=400, blank=True, null=True, verbose_name='Uwagi')
    company_branch = models.ForeignKey('company.CompanyBranch', models.DO_NOTHING, db_column='company_branch', default='main', verbose_name='Oddział')
    user_login = models.OneToOneField('auth.User', models.DO_NOTHING, db_column = 'worker_user_login', blank=True, null=True, verbose_name='Login')

    class Meta:
        managed = False
        db_table = 'worker'
        verbose_name = 'Pracownik'
        verbose_name_plural = 'Lista pracowników'

    def __str__(self):
        return self.first_name + ' ' + self.last_name
