from django import forms

class ContactForm(forms.Form):
    subject = forms.CharField(max_length=100, label='Temat')
    message = forms.CharField(widget=forms.Textarea, label='Wiadomośc')
    sender = forms.EmailField(label='Twój adres E-mail')
