# This is an auto-generated Django model module.
# You'll have to do the following manually to clean this up:
#   * Rearrange models' order
#   * Make sure each model has one field with primary_key=True
#   * Make sure each ForeignKey has `on_delete` set to the desired behavior.
#   * Remove `managed = False` lines if you wish to allow Django to create, modify, and delete the table
# Feel free to rename the models, but don't rename db_table values or field names.
from __future__ import unicode_literals

from django.db import models
#from company.models import CompanyBranch


class Machine(models.Model):
    id_machine = models.AutoField(primary_key=True, verbose_name='Id')
    machine_name = models.CharField(max_length=200, verbose_name='Name')
    machine_type = models.ForeignKey('MachineType', models.DO_NOTHING, db_column='machine_type', verbose_name='Typ')
    service_interval = models.IntegerField(blank=True, null=True, verbose_name='Ilość dni między wymaganą kontrolą')
    last_service = models.DateField(blank=True, null=True, verbose_name='Data ostaniej kontroli')
    is_operational = models.NullBooleanField(verbose_name='Sprawne')
    notes = models.CharField(max_length=400, blank=True, null=True, verbose_name='Uwagi')
    company_branch = models.ForeignKey('company.CompanyBranch',  models.DO_NOTHING, db_column='company_branch', default='main', related_name = '+', verbose_name='Oddział')

    class Meta:
        managed = False
        db_table = 'machine'
        verbose_name = 'Maszyna'
        verbose_name_plural = 'Maszyny'

    def __str__(self):
        if self.machine_name:
            return self.machine_name
        else:
            return str(self.machine_type) + str(self.id_machine)


class MachineType(models.Model):
    id_machine_type = models.CharField(primary_key=True, max_length=10, verbose_name='Id')
    machine_type_name = models.CharField(max_length=200, verbose_name='Nazwa')

    class Meta:
        managed = False
        db_table = 'machine_type'
        verbose_name = 'Typ maszyny'
        verbose_name_plural = 'Typy maszyn'

    def __str__(self):
        return str(self.id_machine_type)
