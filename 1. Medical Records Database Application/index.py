import pandas as pd
import numpy as np
from flask import Flask, request, jsonify, Response, render_template
import json
import mysql.connector
from flask_sqlalchemy import SQLAlchemy
from flask_bootstrap import Bootstrap
from flask_wtf import FlaskForm
from joblib import dump, load
import re
from flask import g, session


# specify database configurations
config = {
    'host': 'localhost',
    'port': 3306,
    'user': 'root',
    'password': 'mypass',
    'database': 'hospital'
}

db_user = config.get('user')
db_pwd = config.get('password')


db_host = config.get('host')
db_port = config.get('port')
db_name = config.get('database')

# specify connection string
connection_str = f'mysql+pymysql://{db_user}:{db_pwd}@{db_host}:{db_port}/{db_name}'
print(connection_str)


app = Flask(__name__)
app.config['SQLALCHEMY_DATABASE_URI'] = connection_str
app.config["SECRET_KEY"] = "testsecretkey"
db = SQLAlchemy(app)


@app.route("/")
def home():
  session['key'] = 'value'

  return render_template("homepage.html")

@app.route("/homepage.html")
def home_banner():
  return render_template("homepage.html")


@app.route("/patient_page.html")
def patient_page():
  return render_template("patiencepage.html")

@app.route("/doctor_page.html")
def doctor_page():
  return render_template("doctorpage.html")


@app.route("/pin.html", methods = ['GET', 'POST'])
def pin():

  user_name = request.form['username']
  phone_number = request.form['phone_num']
  result = db.engine.execute("SELECT id, CONCAT(first_name, ' ', last_name) AS name, phone_number FROM patients WHERE CONCAT(first_name, ' ', last_name) = %s AND phone_number = %s", (user_name, phone_number))
  result = result.fetchall()


  #print(result)
  #print(user_name)
  #print(phone_number)
  # print(result[0][0])
  # print(result[0][0] == '')

  print(result)
  try:
    session['user_id'] = result[0][0]
    print(result[0][0])

    if result[0][0] == ' ':
      return render_template("patiencepage.html")
    else:
      print('hi')
      return render_template('pin0.html')

  except:
    #Login failed
    return render_template("patiencepage.html")

  '''
  print(result[0][0])
  print(result[0][0] == '')
  if result[0][0] == ' ':
    return render_template("patiencepage.html")
  else:
    return render_template("pin.html")
  '''


@app.route("/pin0.html")
def pin0():
  return render_template("pin0.html")

@app.route("/pprofile.html")
def pprofile():
  user_id = session.get('user_id')
  print(user_id)

  result = db.engine.execute("SELECT CONCAT(first_name, ' ', last_name) AS name, phone_number, CONCAT(street, ' ', city, ', ', state) AS address, zip \
    FROM patients WHERE id = %s", user_id)
  result = result.fetchall()

  print(result)
  name = result[0][0]
  phoneNum = result[0][1]
  address = result[0][2]
  zipcode = result[0][3]

  return render_template("pprofile.html", name = name, phoneNum = phoneNum, address = address, zipcode = zipcode)

@app.route("/precord.html")
def precord():
  user_id = session.get('user_id')
  result = db.engine.execute("SELECT DISTINCT v.date, \
         h.name AS hospital_name, \
         CONCAT(p2.first_name,' ', p2.last_name) AS doctor_name, \
         CONCAT(n.first_name,' ', n.last_name) AS nurse_name, \
         t.activity, d.name \
        FROM patients p \
          JOIN visits v on p.id = v.patient_id \
          JOIN hospitals h on v.hospital_id = h.id \
          JOIN physicians p2 on v.doctor_id = p2.id \
          JOIN nurses n on v.nurse_id = n.id \
          JOIN visit_treatment vt on v.id = vt.visit_id \
          JOIN treatment t on vt.treatment_id = t.id \
          JOIN diseases d on v.disease_id = d.id \
          JOIN drugs d2 on d2.id=t.drug_id \
          WHERE p.id=%s \
          ORDER BY 1 DESC", user_id);

  return render_template("precord.html", records = result)

@app.route("/pdoc.html")
def pdoc():

  user_id = session.get('user_id')
  result = db.engine.execute("SELECT DISTINCT CONCAT(p.first_name, ' ', p.last_name), h.name, \
                CONCAT(h.street, ' ', h.city, ', ', h.state, ' ', h.zip), specialty, rating FROM patients JOIN visits v on patients.id = v.patient_id \
                JOIN hospitals h on v.hospital_id = h.id \
               JOIN physicians p on v.doctor_id = p.id \
               WHERE patients.id = %s", user_id)

  return render_template("pdoc.html", records = result)

@app.route("/pinsur.html")
def pinsur():

  user_id = session.get('user_id')
  print(user_id)

  result = db.engine.execute("SELECT i.id AS insurance_ID, i.name \
                FROM patients p \
                LEFT JOIN insurance i ON p.insurance_id = i.id \
                WHERE p.id=%s", user_id)

  result = result.fetchall()
  print(result)


  return render_template("pinsur.html", records = result)

@app.route("/pmed.html")
def pmed():
  
  user_id = session.get('user_id')
  result = db.engine.execute("SELECT v.date, \
         h.name AS hospital_name, \
         CONCAT(p2.first_name, ' ', p2.last_name) AS doctor_name, \
         d2.name, \
         t.quantity, \
         t.drug_id \
  FROM patients p \
  JOIN visits v on p.id = v.patient_id \
  JOIN hospitals h on v.hospital_id = h.id \
  JOIN physicians p2 on v.doctor_id = p2.id \
  JOIN visit_treatment vt on v.id = vt.visit_id \
  JOIN treatment t on vt.treatment_id = t.id \
  JOIN diseases d on v.disease_id = d.id \
  JOIN drugs d2 on d2.id=t.drug_id \
  WHERE p.id=%s AND drug_id<>0 \
  ORDER BY 1 DESC", user_id)


  return render_template("pmed.html", records = result)

@app.route("/phos.html")
def phos():
  user_id = session.get('user_id')
  result = db.engine.execute("SELECT DISTINCT h.name, \
                CONCAT(h.street, ' ', h.city, ', ', h.state, ' ', h.zip), h.phone_number, h.website FROM patients JOIN visits v on patients.id = v.patient_id \
                   JOIN hospitals h on v.hospital_id = h.id \
                   JOIN physicians p on v.doctor_id = p.id \
                   WHERE patients.id = %s", user_id)
  return render_template("phos.html", records = result)

@app.route("/nearbyhos.html")
def nearbyhos():

  user_id = session.get('user_id')  

  result = db.engine.execute("SELECT \
                h.name AS hospital_name, CONCAT(h.street, ' ', h.city, ', ', h.state, ' ', h.zip) AS address, h.phone_number, h.website \
                FROM patients p \
                LEFT JOIN insurance i ON p.insurance_id = i.id \
                LEFT JOIN insurance_hospital ih ON i.id = ih.insurance_id \
                LEFT JOIN hospitals h ON ih.hospital_id=h.id \
                WHERE p.id=%s AND h.state = p.state", user_id)


  return render_template("nearbyhos.html", records = result)

@app.route("/docrecommend.html")
def docrecommend():

  user_id = session.get('user_id')  

  result = db.engine.execute("SELECT CONCAT(d.first_name, ' ', d.last_name), h.name, d.specialty, d.rating \
    FROM hospitals h JOIN physicians d ON h.id = d.hospital_id WHERE h.id IN ( \
               SELECT h.id \
                FROM patients p \
                LEFT JOIN insurance i ON p.insurance_id = i.id \
                LEFT JOIN insurance_hospital ih ON i.id = ih.insurance_id \
                LEFT JOIN hospitals h ON ih.hospital_id=h.id \
                WHERE p.id=%s AND h.state = p.state)", user_id)

  return render_template("docrecommend.html", records = result)

@app.route("/support.html")
def support():

  user_id = session.get('user_id')  

  result = db.engine.execute("SELECT CONCAT(sub.first_name,' ', sub.last_name) AS name, sub.phone_number, sub.email, sub.name AS disease_name, sub.zip \
    FROM ( \
      SELECT p.first_name, p.last_name,p.phone_number, p.email, p.zip,v.disease_id, d.name,SUBSTRING(zip,1,1) AS sub_zip \
      FROM patients p \
      JOIN visits v on p.id = v.patient_id \
      JOIN diseases d on v.disease_id = d.id \
      WHERE v.disease_id IN ( \
          SELECT disease_id \
          FROM patients \
          JOIN visits v2 on patients.id = v2.patient_id \
          WHERE patients.id=%s AND disease_id<>0) \
      AND p.id<>%s) AS sub \
    JOIN \
      (SELECT SUBSTRING(zip,1,1) AS sub_zip \
      FROM patients p2 \
      WHERE p2.id=%s ) sub1 \
      ON sub.sub_zip=sub1.sub_zip", (user_id, user_id, user_id))

  print(result)

  return render_template("support.html", records = result)

@app.route("/privacy.html")
def privacy():
  return render_template("privacy.html")

@app.route("/userguidelines.html")
def user():
  return render_template("userguidelines.html")




if __name__ == "__main__":
  app.run(host="0.0.0.0", debug=True)
