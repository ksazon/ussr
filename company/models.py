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


class CompanyBranch(models.Model):
    id_company_branch = models.CharField(primary_key=True, max_length=10)
    is_main = models.NullBooleanField()
    company_name = models.CharField(max_length=200, blank=True, null=True)
    nip = models.CharField(max_length=11, blank=True, null=True)
    address = models.ForeignKey('utilities.Address', models.DO_NOTHING, db_column='address', related_name = '+')

    class Meta:
        managed = False
        db_table = 'company_branch'
        verbose_name = 'Oddzial'
        verbose_name_plural = 'Lista oddzialow'

    def __str__(self):
        return self.id_company_branch


class Location(models.Model):
    id_location = models.AutoField(primary_key=True)
    location_name = models.CharField(unique=True, max_length=200, blank=True, null=True)
    location_type = models.ForeignKey('LocationType', models.DO_NOTHING, db_column='location_type')
    is_operational = models.NullBooleanField()
    notes = models.CharField(max_length=400, blank=True, null=True)
    company_branch = models.ForeignKey('CompanyBranch', models.DO_NOTHING, db_column='company_branch', default='main')

    class Meta:
        managed = False
        db_table = 'location'
        verbose_name = 'Lokalizacja'
        verbose_name_plural = 'Lokalizacje'

    def __str__(self):
        return self.location_name


class LocationType(models.Model):
    id_location_type = models.CharField(primary_key=True, max_length=10)
    location_type_name = models.CharField(unique=True, max_length=200, blank=True, null=True)
    location_capacity = models.IntegerField(blank=True, null=True)

    class Meta:
        managed = False
        db_table = 'location_type'
        verbose_name = 'Typ lokacji'
        verbose_name_plural = 'Lista typow lokacji'

    def __str__(self):
        return self.location_type_name
