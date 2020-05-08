CREATE DATABASE IF NOT EXISTS hospital;

USE hospital;

DROP TABLE IF EXISTS insurance;
CREATE TABLE insurance (
    id int(11) NOT NULL,
    name varchar(255),
    CONSTRAINT insurance_pk PRIMARY KEY (id)
    );
LOAD DATA LOCAL INFILE 'C:/Users/lzeng/Documents/GitHub/hospital-app/Data/insurance_table.csv' INTO TABLE insurance FIELDS TERMINATED BY ',' IGNORE 1 LINES;

INSERT INTO insurance (id, name) VALUES (0, 'Uninsured');

DROP TABLE IF EXISTS patients;
CREATE TABLE patients (
    id int(11) NOT NULL AUTO_INCREMENT,
    first_name varchar(255),
    last_name varchar(255),
    email varchar(255),
    gender char(1),
    phone_number varchar(15),
    street varchar(255),
    city varchar(255),
    state char(2),
    zip varchar(9),
    insurance_id int(3) NOT NULL,
    CONSTRAINT patients_pk PRIMARY KEY (id),
    FOREIGN KEY (insurance_id) REFERENCES insurance(id)
                      ON UPDATE CASCADE
    );
LOAD DATA LOCAL INFILE 'C:/Users/lzeng/Documents/GitHub/hospital-app/Data/patients_table.csv' INTO TABLE patients FIELDS TERMINATED BY ',' IGNORE 1 LINES;


DROP TABLE IF EXISTS hospitals;
CREATE TABLE hospitals (
    id int(11) NOT NULL AUTO_INCREMENT,
    name varchar(255),
    type varchar(50),
    street varchar(255),
    city varchar(255),
    state char(2),
    zip varchar(9),
    phone_number varchar(15),
    website varchar(255),
    CONSTRAINT hospitals_pk PRIMARY KEY (id)
    );
LOAD DATA LOCAL INFILE 'C:/Users/lzeng/Documents/GitHub/hospital-app/Data/hospital_table.csv' INTO TABLE hospitals FIELDS TERMINATED BY ',' IGNORE 1 LINES;

DROP TABLE IF EXISTS physicians;
CREATE TABLE physicians (
    id int(11) NOT NULL AUTO_INCREMENT,
    hospital_id int(11) NOT NULL,
    last_name varchar(255),
    first_name varchar(255),
    email varchar(255),
    specialty varchar(255),
    rating float(11),
    CONSTRAINT physicians_pk PRIMARY KEY (id),
    FOREIGN KEY (hospital_id) REFERENCES hospitals(id)
                      ON UPDATE CASCADE
    );
LOAD DATA LOCAL INFILE 'C:/Users/lzeng/Documents/GitHub/hospital-app/Data/physicians_table.csv' INTO TABLE physicians FIELDS TERMINATED BY ',' IGNORE 1 LINES;

DROP TABLE IF EXISTS nurses;
CREATE TABLE nurses (
    id int(11) NOT NULL AUTO_INCREMENT,
    hospital_id int(11) NOT NULL,
    last_name varchar(255),
    first_name varchar(255),
    CONSTRAINT nurses_pk PRIMARY KEY (id),
    FOREIGN KEY (hospital_id) REFERENCES hospitals(id)
                      ON UPDATE CASCADE
    );
LOAD DATA LOCAL INFILE 'C:/Users/lzeng/Documents/GitHub/hospital-app/Data/nurse_table.csv' INTO TABLE nurses FIELDS TERMINATED BY ',' IGNORE 1 LINES;

DROP TABLE IF EXISTS drugs;
CREATE TABLE drugs (
    id int(11) NOT NULL,
    name varchar(255),
    CONSTRAINT drugs_pk PRIMARY KEY (id)
    );
LOAD DATA LOCAL INFILE 'C:/Users/lzeng/Documents/GitHub/hospital-app/Data/drugs_table.csv' INTO TABLE drugs FIELDS TERMINATED BY ',' IGNORE 1 LINES;

INSERT INTO drugs (id, name) VALUES (0, NULL);

DROP TABLE IF EXISTS diseases;
CREATE TABLE diseases (
    id int(11) NOT NULL,
    name varchar(255),
    symptoms varchar(255),
    CONSTRAINT disease_pk PRIMARY KEY (id)
    );
LOAD DATA LOCAL INFILE 'C:/Users/lzeng/Documents/GitHub/hospital-app/Data/disease_table.csv' INTO TABLE diseases FIELDS TERMINATED BY ',' IGNORE 1 LINES;

INSERT INTO diseases (id, name) VALUES (0, NULL);

DROP TABLE IF EXISTS disease_drug;
CREATE TABLE disease_drug (
    disease_id int(11) NOT NULL,
    drug_id int(11) NOT NULL ,
    CONSTRAINT
       FOREIGN KEY (disease_id) REFERENCES diseases(id),
       FOREIGN KEY (drug_id) REFERENCES  drugs(id)
                          ON UPDATE CASCADE

    );
LOAD DATA LOCAL INFILE 'C:/Users/lzeng/Documents/GitHub/hospital-app/Data/disease_drug_table.csv' INTO TABLE disease_drug FIELDS TERMINATED BY ',' IGNORE 1 LINES;



DROP TABLE IF EXISTS treatment;
CREATE TABLE treatment (
    id int(11) NOT NULL,
    activity varchar(255),
    drug_id int(11) ,
    quantity int(11),
    description varchar(255),
    CONSTRAINT treatment_pk PRIMARY KEY (id, drug_id),
        FOREIGN KEY (drug_id) REFERENCES drugs(id)
    );
LOAD DATA LOCAL INFILE 'C:/Users/lzeng/Documents/GitHub/hospital-app/Data/treatment_table.csv' INTO TABLE treatment FIELDS TERMINATED BY ',' IGNORE 1 LINES;

DROP TABLE IF EXISTS visits;
CREATE TABLE visits (
    id int(11) NOT NULL AUTO_INCREMENT,
    patient_id int(11) NOT NULL,
    date DATE,
    doctor_id int(11) NOT NULL,
    nurse_id int(11) NOT NULL,
    hospital_id int(11) NOT NULL,
    disease_id int(11) NOT NULL,
    CONSTRAINT visits_pk PRIMARY KEY (id),
    FOREIGN KEY (patient_id) REFERENCES patients(id)
                      ON UPDATE CASCADE,
    FOREIGN KEY (doctor_id) REFERENCES physicians(id)
                      ON UPDATE CASCADE,
    FOREIGN KEY (nurse_id) REFERENCES nurses(id)
                      ON UPDATE CASCADE,
    FOREIGN KEY (hospital_id) REFERENCES hospitals(id)
                      ON UPDATE CASCADE,
    FOREIGN KEY (disease_id) REFERENCES diseases(id)
                      ON UPDATE CASCADE
    );
LOAD DATA LOCAL INFILE 'C:/Users/lzeng/Documents/GitHub/hospital-app/Data/visit_table.csv' INTO TABLE visits FIELDS TERMINATED BY ',' IGNORE 1 LINES;

DROP TABLE IF EXISTS visit_treatment;
CREATE TABLE visit_treatment (
    visit_id int(11) NOT NULL,
    treatment_id int(11) NOT NULL,
    CONSTRAINT
       FOREIGN KEY (visit_id) REFERENCES visits(id),
       FOREIGN KEY (treatment_id) REFERENCES  treatment(id)
                          ON UPDATE CASCADE

    );
LOAD DATA LOCAL INFILE 'C:/Users/lzeng/Documents/GitHub/hospital-app/Data/visit_treatment_table.csv' INTO TABLE visit_treatment FIELDS TERMINATED BY ',' IGNORE 1 LINES;


DROP TABLE IF EXISTS insurance_hospital;
CREATE TABLE insurance_hospital (
    insurance_id int(11) NOT NULL,
    hospital_id int(11) NOT NULL,
    CONSTRAINT
       FOREIGN KEY (hospital_id) REFERENCES hospitals(id),
       FOREIGN KEY (insurance_id) REFERENCES  insurance(id)
                          ON UPDATE CASCADE

    );
LOAD DATA LOCAL INFILE 'C:/Users/lzeng/Documents/GitHub/hospital-app/Data/insurance_hospital_table.csv' INTO TABLE insurance_hospital FIELDS TERMINATED BY ',' IGNORE 1 LINES;


SELECT * FROM patients WHERE id = 501;


SELECT p.first_name, p.last_name, p2.last_name, p2.first_name,  h.name, date, treatment_id, t.activity, d.name, t.quantity FROM patients p JOIN visits v on p.id = v.patient_id
                                    JOIN hospitals h on v.hospital_id = h.id
                                    JOIN physicians p2 on v.doctor_id = p2.id
                                    JOIN visit_treatment vt on v.id = vt.visit_id JOIN treatment t on vt.treatment_id = t.id
                                    JOIN drugs d on t.drug_id = d.id
 WHERE p.id = 1

SELECT first_name, last_name, CONCAT(street, ' ', city, ', ', state), zip FROM patients






SELECT CONCAT(d.first_name, ' ', d.last_name), h.name, d.specialty, d.rating FROM hospitals h JOIN physicians d ON h.id = d.hospital_id WHERE h.id IN (
               SELECT h.id
                FROM patients p
                LEFT JOIN insurance i ON p.insurance_id = i.id
                LEFT JOIN insurance_hospital ih ON i.id = ih.insurance_id
                LEFT JOIN hospitals h ON ih.hospital_id=h.id
                WHERE p.id=1 AND h.state = p.state );






SELECT v.date,
         h.name AS hospital_name,
         CONCAT(p2.first_name,' ', p2.last_name) AS doctor_name,
         CONCAT(n.first_name,' ', n.last_name) AS nurse_name,
         t.activity, d.name AS diagnosis
        FROM patients p
          JOIN visits v on p.id = v.patient_id
          JOIN hospitals h on v.hospital_id = h.id
          JOIN physicians p2 on v.doctor_id = p2.id
          JOIN nurses n on v.nurse_id = n.id
          JOIN visit_treatment vt on v.id = vt.visit_id
          JOIN treatment t on vt.treatment_id = t.id
          JOIN diseases d on v.disease_id = d.id
          JOIN drugs d2 on d2.id=t.drug_id
          WHERE p.id=2
          ORDER BY 1 DESC;

SELECT h.name AS hospital_name, CONCAT(h.street, ' ', h.city, ', ', h.state, ' ', h.zip) AS address, h.phone_number, h.website
FROM patients p
LEFT JOIN insurance i ON p.insurance_id = i.id
LEFT JOIN insurance_hospital ih ON i.id = ih.insurance_id
LEFT JOIN hospitals h ON ih.hospital_id=h.id
WHERE p.id=2 AND h.state = p.state

SELECT CONCAT(sub.first_name,' ', sub.last_name) AS name, sub.phone_number, sub.email, sub.name AS disease_name, sub.zip
    FROM (
      SELECT p.first_name, p.last_name,p.phone_number, p.email, p.zip,v.disease_id, d.name,SUBSTRING(zip,1,1) AS sub_zip
      FROM patients p
      JOIN visits v on p.id = v.patient_id
      JOIN diseases d on v.disease_id = d.id
      WHERE v.disease_id IN (
          SELECT disease_id
          FROM patients
          JOIN visits v2 on patients.id = v2.patient_id
          WHERE patients.id=2 AND disease_id<>0)
      AND p.id<>2) AS sub
    JOIN
      (SELECT SUBSTRING(zip,1,1) AS sub_zip
      FROM patients p2
      WHERE p2.id=2 ) sub1
      ON sub.sub_zip=sub1.sub_zip
