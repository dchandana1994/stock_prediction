
#print filenames

# !/usr/bin/env python
# encoding: utf-8
"""
python_3_email_with_attachment.py
Created by Robert Dempsey on 12/6/14.
Copyright (c) 2014 Robert Dempsey. Use at your own peril.
This script works with Python 3.x
NOTE: replace values in ALL CAPS with your own values
"""

import os,fnmatch
import smtplib
from email import encoders
from email.mime.base import MIMEBase
from email.mime.multipart import MIMEMultipart
from Companies import ApiDetails,stock_time_zone,constituent_data,fetch_data
from email.mime.text import MIMEText
import sys

#djfjdk
COMMASPACE = ', '
#Set up users for email

def main(attachments,message_text):
    sender = 'durgaharish1993@gmail.com'
    gmail_password = 'npfybmwpzmskhptg'
    recipients = ['durgaharish1992@gmail.com', 'wvktaraknath@gmail.com']

    # Create the enclosing (outer) message
    outer = MIMEMultipart()
    outer['Subject'] = 'Stock Predicition From Anicca Data'
    outer['To'] = COMMASPACE.join(recipients)
    outer['From'] = sender
    outer.preamble = 'You will not see this in a MIME-aware mail reader.\n'
    text = "Dear Investor, \n\n This mail contains forecast for next 6 days for following companies, \n\n"
    text =  text + message_text
    text =  text + '\n\n\n\n Regards,\n Team Anicca \n\n\n\n\n'

    outer.attach(
        MIMEText(text, 'plain'))
    # List of attachments

    # Add the attachments to the message
    for file in attachments:
        try:
            with open(file, 'rb') as fp:
                msg = MIMEBase('application', "octet-stream")
                msg.set_payload(fp.read())
            encoders.encode_base64(msg)
            msg.add_header('Content-Disposition', 'attachment', filename=os.path.basename(file))
            outer.attach(msg)

        except:
            print("Unable to open one of the attachments. Error: ", sys.exc_info()[0])
            raise

    composed = outer.as_string()

    # Send the email
    try:
        with smtplib.SMTP('smtp.gmail.com', 587) as s:
            s.ehlo()
            s.starttls()
            s.ehlo()
            s.login(sender, gmail_password)
            s.sendmail(sender, recipients, composed)
            s.close()
        print("Email sent!")
    except:
        print("Unable to send the email. Error: ", sys.exc_info()[0])
        raise


if __name__ == '__main__':
    # Set up crap for the attachments

    a = constituent_data()
    company_details = a.get_topK_dict(k=10)
    keys = company_details['Name'].keys()
    output_csv_folder = '/Users/dimbul/Desktop/temp_desktop/private/stock_Prediction/output_data/csv_10092017'
    file_list = []
    count =1
    message_text = ''
    for key in keys:
        company_name = company_details['Name'][key].replace(" ", "_")
        company_symbol = company_details['Symbol'][key]
        file_name = fnmatch.filter(os.listdir(output_csv_folder),company_name + '_Forecast_*' + '.csv')[0]
        file_list+=[output_csv_folder+'/'+file_name]
        message_text += str(count) +'.       '+ company_details['Name'][key] + '      -     ' + company_symbol
        message_text+='\n'
        count +=1

    print('dkfjd')


    main(attachments=file_list, message_text=message_text)


