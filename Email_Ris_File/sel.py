#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Feb  17 19:15:46 2022

@author: antonio
"""
import sys
import base64
import imaplib
import json
import smtplib
import urllib.parse
import urllib.request
import lxml.html
import threading
import schedule
import time
import os
from datetime import datetime
from datetime import timedelta
from selenium import webdriver
from selenium.webdriver.common.by import By
from selenium.webdriver.firefox.firefox_profile import FirefoxProfile
from email.mime.multipart import MIMEMultipart
from email.mime.text import MIMEText
from email.mime.base import MIMEBase
from pathlib import Path
from email import encoders

from sel_functions import ALL_FILES
"""
https://blog.macuyiko.com/post/2016/how-to-send-html-mails-with-oauth2-and-gmail-in-python.html
"""

"""
Adapted from:
https://github.com/google/gmail-oauth2-tools/blob/master/python/oauth2.py
https://developers.google.com/identity/protocols/OAuth2

1. Generate and authorize an OAuth2 (generate_oauth2_token)
2. Generate a new access tokens using a refresh token(refresh_token)
3. Generate an OAuth2 string to use for login (access_token)
"""

##############################################################################
######################Email sendage portion, from the blog ###################
##############################################################################

GOOGLE_ACCOUNTS_BASE_URL = 'https://accounts.google.com'
REDIRECT_URI = 'urn:ietf:wg:oauth:2.0:oob'

GOOGLE_CLIENT_ID = ""
GOOGLE_CLIENT_SECRET = ""
GOOGLE_REFRESH_TOKEN = None

def command_to_url(command):
    return '%s/%s' % (GOOGLE_ACCOUNTS_BASE_URL, command)


def url_escape(text):
    return urllib.parse.quote(text, safe='~-._')


def url_unescape(text):
    return urllib.parse.unquote(text)


def url_format_params(params):
    param_fragments = []
    for param in sorted(params.items(), key=lambda x: x[0]):
        param_fragments.append('%s=%s' % (param[0], url_escape(param[1])))
    return '&'.join(param_fragments)


def generate_permission_url(client_id, scope='https://mail.google.com/'):
    params = {}
    params['client_id'] = client_id
    params['redirect_uri'] = REDIRECT_URI
    params['scope'] = scope
    params['response_type'] = 'code'
    return '%s?%s' % (command_to_url('o/oauth2/auth'), \
                      url_format_params(params))


def call_authorize_tokens(client_id, client_secret, authorization_code):
    params = {}
    params['client_id'] = client_id
    params['client_secret'] = client_secret
    params['code'] = authorization_code
    params['redirect_uri'] = REDIRECT_URI
    params['grant_type'] = 'authorization_code'
    request_url = command_to_url('o/oauth2/token')
    response = urllib.request.urlopen(request_url, urllib.parse.urlencode(params).encode('UTF-8')).read().decode('UTF-8')
    return json.loads(response)


def call_refresh_token(client_id, client_secret, refresh_token):
    params = {}
    params['client_id'] = client_id
    params['client_secret'] = client_secret
    params['refresh_token'] = refresh_token
    params['grant_type'] = 'refresh_token'
    request_url = command_to_url('o/oauth2/token')
    response = urllib.request.urlopen(request_url, urllib.parse.urlencode(params).encode('UTF-8')).read().decode('UTF-8')
    return json.loads(response)


def generate_oauth2_string(username, access_token, as_base64=False):
    auth_string = 'user=%s\1auth=Bearer %s\1\1' % (username, access_token)
    if as_base64:
        auth_string = base64.b64encode(auth_string.encode('ascii')).decode('ascii')
    return auth_string


def test_imap(user, auth_string):
    imap_conn = imaplib.IMAP4_SSL('imap.gmail.com')
    imap_conn.debug = 4
    imap_conn.authenticate('XOAUTH2', lambda x: auth_string)
    imap_conn.select('INBOX')


def test_smpt(user, base64_auth_string):
    smtp_conn = smtplib.SMTP('smtp.gmail.com', 587)
    smtp_conn.set_debuglevel(True)
    smtp_conn.ehlo('test')
    smtp_conn.starttls()
    smtp_conn.docmd('AUTH', 'XOAUTH2 ' + base64_auth_string)


def get_authorization(google_client_id, google_client_secret):
    scope = "https://mail.google.com/"
    print('Navigate to the following URL to auth:', generate_permission_url(google_client_id, scope))
    authorization_code = input('Enter verification code: ')
    response = call_authorize_tokens(google_client_id, google_client_secret, authorization_code)
    return response['refresh_token'], response['access_token'], response['expires_in']


def refresh_authorization(google_client_id, google_client_secret, refresh_token):
    response = call_refresh_token(google_client_id, google_client_secret, refresh_token)
    return response['access_token'], response['expires_in']


def send_mail(fromaddr, toaddr, subject, message, files=[]):
    access_token, expires_in = refresh_authorization(GOOGLE_CLIENT_ID, GOOGLE_CLIENT_SECRET, GOOGLE_REFRESH_TOKEN)
    auth_string = generate_oauth2_string(fromaddr, access_token, as_base64=True)

    msg = MIMEMultipart('related')
    msg['Subject'] = subject
    msg['From'] = fromaddr
    msg['To'] = toaddr
    msg.preamble = 'This is a multi-part message in MIME format.'
    msg_alternative = MIMEMultipart('alternative')
    msg.attach(msg_alternative)
    part_text = MIMEText(lxml.html.fromstring(message).text_content().encode('utf-8'), 'plain', _charset='utf-8')
    part_html = MIMEText(message.encode('utf-8'), 'html', _charset='utf-8')
    msg_alternative.attach(part_text)
    msg_alternative.attach(part_html)
    
    for path in files:
        part = MIMEBase('application', "octet-stream")
        with open(path, 'rb') as file:
            part.set_payload(file.read())
        encoders.encode_base64(part)
        part.add_header('Content-Disposition',
                            'attachment; filename={}'.format(Path(path).name))
        msg.attach(part)
    
    server = smtplib.SMTP('smtp.gmail.com:587')
    server.ehlo(GOOGLE_CLIENT_ID)
    server.starttls()
    server.docmd('AUTH', 'XOAUTH2 ' + auth_string)
    server.sendmail(fromaddr, toaddr, msg.as_string())
    server.quit()
    
##############################################################################
######################### Written part by me Kuri-sama #######################
##############################################################################

def run_continuously(interval=1):
    """Continuously run, while executing pending jobs at each
    elapsed time interval.
    @return cease_continuous_run: threading. Event which can
    be set to cease continuous run. Please note that it is
    *intended behavior that run_continuously() does not run
    missed jobs*. For example, if you've registered a job that
    should run every minute and you set a continuous run
    interval of one hour then your job won't be run 60 times
    at each interval but only once.
    """
    cease_continuous_run = threading.Event()

    class ScheduleThread(threading.Thread):
        @classmethod
        def run(cls):
            while not cease_continuous_run.is_set():
                schedule.run_pending()
                time.sleep(interval)

    continuous_thread = ScheduleThread()
    continuous_thread.start()
    return cease_continuous_run

def get_search_date_range():
    
    weekday = datetime.today().strftime('%A')
    today = time.strftime("%Y%m%d")
    if weekday == "Monday":
        
        last_friday = datetime.today() - timedelta(days=3)
        last_friday = last_friday.strftime('%Y%m%d')
        
        search_dat = "Entry_date:([" + last_friday + " TO " + today + "])"
    else:
        
        search_dat = "Entry_date:(" + today +")"
        
    return search_dat

def wait_for_download_and_get_file_name(folder, timedel):
    DOWNLOAD_DIR = folder
    while True:
        # Get the name of the file with the latest creation time
        newest_file_name = max([os.path.join(DOWNLOAD_DIR, f) for f in os.listdir(DOWNLOAD_DIR)], key=os.path.getctime)
        # Get the creation time of the file
        # file_creation_time = datetime.fromtimestamp(os.path.getctime(newest_file_name))

        # five_seconds_ago = datetime.now() - timedelta(seconds=timedel)
        last_chars = newest_file_name[-4:]
        
        if last_chars != ".ris":
            print(f'.', end='')
            time.sleep(0.5)
        else:
            print(f'\nFinished downloading "{newest_file_name}"')
            break

    return newest_file_name

def look_up_ris_file(dates, folder):
    
    dl_path = folder

    profile = webdriver.FirefoxProfile()
    profile.set_preference("browser.helperApps.neverAsk.saveToDisk", "text/html,text/plain")
    profile.set_preference("browser.download.dir", dl_path)
    profile.set_preference("browser.download.folderList", 2);

    driver = webdriver.Firefox(firefox_profile=profile)

    driver.get('https://search.bvsalud.org/global-literature-on-novel-coronavirus-2019-ncov/')

    search_box = driver.find_element_by_id('q')
    search_box.send_keys(dates)

    right_side_buttons = driver.find_elements_by_class_name('btnTools')
    export_button = right_side_buttons[1]
    export_button.click()


    download = driver.find_element_by_xpath('/html/body/div[4]/div/div/div[2]/a[3]')
    download.click()
    time.sleep(5)
    ris_file = wait_for_download_and_get_file_name(folder, 10) #max([os.path.join(dl_path, f) for f in os.listdir(dl_path)], key=os.path.getctime)
    time.sleep(1)
    driver.close()
    
    return ris_file

def get_filtered_ris_file(folder, file_name):
    
    profile = webdriver.FirefoxProfile()
    profile.set_preference("browser.helperApps.neverAsk.saveToDisk", ALL_FILES)
    profile.set_preference('browser.download.manager.showWhenStarting', False) 
    profile.set_preference("browser.download.dir", path)
    profile.set_preference("browser.download.folderList", 2);

    driver = webdriver.Firefox(firefox_profile=profile)
    
    driver.get('https://robotsearch.vortext.systems/')

    chooseFile = driver.find_element_by_xpath('/html/body/div/div/div[1]/div[2]/form/input[1]')
    chooseFile.send_keys(file_name);

    upload = driver.find_element_by_xpath('/html/body/div/div/div[1]/div[2]/form/input[2]')
    upload.click()
    time.sleep(10)

    download = driver.find_element_by_xpath('/html/body/div/div/div[1]/div[2]/p[2]/a')
    download.click()
    time.sleep(5)
    ris_file =wait_for_download_and_get_file_name(folder, 10) # max([os.path.join(folder, f) for f in os.listdir(folder)], key=os.path.getctime)
    time.sleep(1)
    driver.close()
    return ris_file

def do_the_whole_shabang(path, sender, receiver):
    
    search_dat = get_search_date_range()

    file_name = look_up_ris_file(search_dat, path)
    
    if GOOGLE_REFRESH_TOKEN is None:
        print('No refresh token found, obtaining one')
        refresh_token, access_token, expires_in = get_authorization(GOOGLE_CLIENT_ID, GOOGLE_CLIENT_SECRET)
        print('Set the following as your GOOGLE_REFRESH_TOKEN:', refresh_token)
        sys.exit()
    
    if float(os.path.getsize(os.path.join(path, file_name))) > 0:
        print("email sent")
    
        final_file_name = get_filtered_ris_file(path, file_name)
        
        send_mail(sender, receiver,
                  '.ris files for the ' + search_dat + " date range",
                  '<b>the .ris file, processed and all</b><br><br>' +
                  'See you space cowboy...',
                  files=[final_file_name])
        
        
    else:
        
        print("The .ris file is empty, ending run without doing anything else")

        send_mail(sender, receiver,
              'No .ris files for the ' + search_dat + " date range",
              '<b>No .ris file processsed, the date range returned empty list</b><br><br>' +
              'Better luck next time')


#path where the file will be saved, suggested path

path = "/tmp/ris"

if not os.path.isdir(path):
    os.mkdir(path)
    
path = path + "/"

#The mail addresses, sender must be gmail

sender_address = "antonio.lunakuri@gmail.com"
receiver_adress = "juan.diaz.mart@gmail.com"

#### Section I: to check if it runs ok########################################

#do_the_whole_shabang(path, sender_address, receiver_adress)

##############################################################################


####### Section II: To run normally############################################

# schedule.every().monday.at("18:00").do(do_the_whole_shabang,path, sender_address, receiver_adress)
# schedule.every().tuesday.at("18:00").do(do_the_whole_shabang,path, sender_address, receiver_adress)
# schedule.every().wednesday.at("18:00").do(do_the_whole_shabang,path, sender_address, receiver_adress)
# schedule.every().thursday.at("18:00").do(do_the_whole_shabang,path, sender_address, receiver_adress)
# schedule.every().friday.at("18:00").do(do_the_whole_shabang,path, sender_address, receiver_adress)

# run_continuously = run_continuously(60)

#############################################################################

'''
Install firefox and geckodriver

To set up, follow the instrucctions in the blog post on the top, then run 
Section I, the "to check if it runs ok" segment, with the correct sender and 
receiver adresses

If everything is ok, recomment "to check if it runs ok" and de-comment Section
II:"To run normally part"

finally, in a terminal run "sudo crontab -e"
and add the following line to the document:
@reboot echo 'export PATH=$PATH:<where geckodriver is installed>' >> ~/.bash_profile; source ~/.bash_profile; cd <path to directory with this file> && nohup python3 sel.py
save and restart system
'''