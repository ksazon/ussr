from django.contrib import admin

from .models import *

class ClBlockedReasonDictAdmin(admin.ModelAdmin):
    fields = ['id_cl_blocked_reason_dict', 'blocked_reason_name']
admin.site.register(ClBlockedReasonDict, ClBlockedReasonDictAdmin)


# admin.site.register(ClBlockedReasonDict)
admin.site.register(ClCommunicationLog)
admin.site.register(ClCommunicationReason)
admin.site.register(ClDiscount)

admin.site.register(ClParams)
admin.site.register(ClPayment)
admin.site.register(ClPaymentLine)
# admin.site.register(ClUnconfirmed)
# admin.site.register(Client)
class ClientAdmin(admin.ModelAdmin):
    list_display = ('first_name', 'last_name')
admin.site.register(Client, ClientAdmin)
admin.site.register(DiscountDict)
admin.site.register(DiscountScope)
#admin.site.register(Contact)

# class ContactInline(admin.TabularInline):
#     model = Contact
#     extra = 2
# class ClientAdmin(admin.ModelAdmin):
#     inlines = (ContactInline, )
# # admin.site.unregister(Client)
# admin.site.register(Client, ClientAdmin)
