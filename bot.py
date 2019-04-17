#!/usr/bin/env python

import tweepy
import time
import os

consumer_key =    '[your personal consumer key]'
consumer_secret = '[your personal secret consumer key]'
access_token =    '[your personal access token]'
access_secret =   '[your personal secret access token]'

auth = tweepy.OAuthHandler(consumer_key, consumer_secret)
auth.set_access_token(access_token, access_secret)
api = tweepy.API(auth)

bits_path = "./bits"

os.chdir(bits_path)
for abitofsaturn in os.listdir('.'):
    api.update_with_media(abitofsaturn, "A bit of Saturn")
    time.sleep(60 * 60 * 6)