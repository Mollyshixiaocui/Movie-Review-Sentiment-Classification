# -*- coding: utf-8 -*-
"""
Created on Mon Apr 04 17:10:40 2016

@author: csx
load movie data from rotten tomato API and save it into SQLite database.
"""
import requests
import simplejson
import sqlite3
import os

def search_movie(key, title):
    """
    return movie ID
    """
    link = "http://api.rottentomatoes.com/api/public/v1.0/movies.json"
    url = "%s?apikey=%s&q=%s&page_limit=1"
    url = url % (link, key, title)
    res = requests.get(url)
    js = simplejson.loads(res.content)
    
    for movie in js["movies"]:
        print "title: %s" % movie["title"]
        print "ID: %s" % movie["ID"]
        
def save_to_SQLite(key, ID):
    """
    save the movie information
    """
    link = "http://api.rottentomatoes.com/api/public/v1.0/movies/%s/reviews.json"
    link = link % ID
    url = "%s?apikey=%s&review_type=all&page_limit=200&page=1"
    url = url % (link, key)
    res = requests.get(url)
    js_review = simplejson.loads(res.content)
    
    link = "http://api.rottentomatoes.com/api/public/v1.0/movies/%s.json"
    link = link % ID
    url = "%s?apikey=%s&page_limit=1"
    url = url % (link, key)
    res = requests.get(url)
    js_movie = simplejson.loads(res.content)    
    
    if not os.path.exists("movies.db"):
        # create the database
        conn = sqlite3.connect("movies.db")
 
        cursor = conn.cursor()
 
        cursor.executescript("""
        CREATE TABLE movies(
        id INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT UNIQUE,
        title TEXT, 
        rated TEXT, 
        synopsis TEXT);
         
        CREATE TABLE reviews(
        author_id INTEGER, 
        movie_id INTEGER,
        content TEXT);
               
        CREATE TABLE author(
        id INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT UNIQUE,
        name TEXT UNIQUE)
        """)
    else:
        conn = sqlite3.connect("movies.db")
        cursor = conn.cursor()

    for movie in js_movie["movie"]:
        sql = "INSERT INTO movies(title, rated, synopsis) VALUES(?, ?, ?)"
        cursor.execute(sql, (movie["title"],
                             movie["mpaa_rating"],
                             movie["synopsis"]))
        movie_id = cursor.lastrowid
        
    for review in js_review["review"]:
        cursor.execute("INSERT OR IGNORE INTO author(name) VALUES(?)", (review["author"],))
        author_id = cursor.lastrowid
        cursor.execute("INSERT INTO reviews(author_id, movie_id, content) VALUE(?,?,?)",
                       (author_id, movie_id, review["content"]))
