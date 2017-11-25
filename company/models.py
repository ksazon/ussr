# This is an auto-generated Django model module.
# You'll have to do the following manually to clean this up:
#   * Rearrange models' order
#   * Make sure each model has one field with primary_key=True
#   * Make sure each ForeignKey has `on_delete` set to the desired behavior.
#   * Remove `managed = False` lines if you wish to allow Django to create, modify, and delete the table
# Feel free to rename the models, but don't rename db_table values or field names.
from __future__ import unicode_literals

from django.db import models
#from utilities.models import Address

# TODO do ukrycia
class CompanyBranch(models.Model):
    id_company_branch = models.CharField(primary_key=True, max_length=10, verbose_name='Id')
    is_main = models.NullBooleanField(verbose_name='Glowny`')
    company_name = models.CharField(max_length=200, blank=True, null=True, verbose_name='Pełna nazwa')
    nip = models.CharField(max_length=11, blank=True, null=True, verbose_name='NIP')
    address = models.ForeignKey('utilities.Address', models.DO_NOTHING, db_column='address', related_name = '+', verbose_name='Adres')

    class Meta:
        managed = False
        db_table = 'company_branch'
        verbose_name = 'Oddział'
        verbose_name_plural = 'Lista oddziałów'

    def __str__(self):
        return self.id_company_branch


class CompanyDescription(models.Model):
    id_company_description = models.CharField(primary_key=True, max_length=10, verbose_name='Id')
    description_long = models.CharField(max_length=10000, blank=True, null=True, verbose_name='Opis długi')
    description_short = models.CharField(max_length=5000, blank=True, null=True, verbose_name='Opis krótki')

    class Meta:
        managed = False
        db_table = 'company_description'
        verbose_name = 'Opis przedsiębiorstwa'
        verbose_name_plural = 'Opisy przedsiębiorstwa'

    def __str__(self):
        return self.id_company_description


class Location(models.Model):
    id_location = models.AutoField(primary_key=True, verbose_name='Id')
    location_name = models.CharField(unique=True, max_length=200, blank=True, null=True, verbose_name='Nazwa')
    location_type = models.ForeignKey('LocationType', models.DO_NOTHING, db_column='location_type', verbose_name='Typ')
    is_operational = models.NullBooleanField(default=True, verbose_name='Sprawna')
    notes = models.CharField(max_length=400, blank=True, null=True, verbose_name='Notatki')
    company_branch = models.ForeignKey('CompanyBranch', models.DO_NOTHING, db_column='company_branch', default='main', verbose_name='Oddział')

    class Meta:
        managed = False
        db_table = 'location'
        verbose_name = 'Lokalizacja'
        verbose_name_plural = 'Lokalizacje'

    def __str__(self):
        return self.location_name


class LocationType(models.Model):
    id_location_type = models.CharField(primary_key=True, max_length=10, verbose_name='Id')
    location_type_name = models.CharField(unique=True, max_length=200, blank=True, null=True, verbose_name='Nazwa')
    location_capacity = models.IntegerField(blank=True, null=True, verbose_name='Pojemność')

    class Meta:
        managed = False
        db_table = 'location_type'
        verbose_name = 'Typ lokacji'
        verbose_name_plural = 'Lista typów lokacji'

    def __str__(self):
        return self.location_type_name
