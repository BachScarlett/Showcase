USE hospital;

#My Insurance
SELECT DISTINCT p.first_name,
                i.name AS insurance_name, i.id AS insurance_ID, #no insurance type in the table
                h.name AS hospital_name, h.street,h.city,h.state, h.zip, h.type,h.website, h.phone_number
FROM patients p
LEFT JOIN insurance i ON p.insurance_id = i.id
LEFT JOIN insurance_hospital ih ON i.id = ih.insurance_id
LEFT JOIN hospitals h ON ih.hospital_id=h.id
WHERE p.id=4
    AND SUBSTRING(p.zip,1,2)=SUBSTRING(h.zip,1,2);

SELECT i.id AS insurance_ID, i.name
                FROM patients p
                LEFT JOIN insurance i ON p.insurance_id = i.id
                WHERE p.id=1;



#Hospitals Near Me
SELECT i.name AS insurance_name,
                h.name AS hospital_name, h.street,h.city,h.state, h.zip, h.type,h.website, h.phone_number
FROM patients p
LEFT JOIN insurance i ON p.insurance_id = i.id
LEFT JOIN insurance_hospital ih ON i.id = ih.insurance_id
LEFT JOIN hospitals h ON ih.hospital_id=h.id
WHERE p.id=3 AND h.state = p.state;

SELECT * FROM insurance_hospital WHERE insurance_id = 22;

#Doctors Near Me
SELECT DISTINCT p.first_name, i.name AS insurance_name,p.zip,
                h.name AS hospital_name, h.street,h.city,h.state, h.zip, h.type,h.website, h.phone_number,
                p2.first_name AS doctor_firstname, p2.last_name AS doctor_lastname,p2.specialty, p2.email
FROM patients p
LEFT JOIN insurance i ON p.insurance_id = i.id
LEFT JOIN insurance_hospital ih ON i.id = ih.insurance_id
LEFT JOIN hospitals h ON ih.hospital_id=h.id
LEFT JOIN physicians p2 on h.id = p2.hospital_id
WHERE p.id=4
    AND SUBSTRING(p.zip,1,2)=SUBSTRING(h.zip,1,2);

#My Community
SELECT sub.name AS disease_name, sub.first_name,sub.last_name,sub.email,sub.zip
FROM (
    SELECT p.first_name, p.last_name,p.email, p.zip,v.disease_id, d.name,SUBSTRING(zip,1,1) AS sub_zip
    FROM patients p
    JOIN visits v on p.id = v.patient_id
    JOIN diseases d on v.disease_id = d.id
    WHERE v.disease_id IN (
        SELECT disease_id
        FROM patients
        JOIN visits v2 on patients.id = v2.patient_id
        WHERE patients.id=4 AND disease_id<>0
        )
    AND p.id<>4) AS sub
JOIN
    (SELECT SUBSTRING(zip,1,1) AS sub_zip
    FROM patients p2
    WHERE p2.id=4 ) sub1
    ON sub.sub_zip=sub1.sub_zip;


# Visiting Records

SELECT DISTINCT v.date,
       h.name AS hospital_name,
       p2.first_name AS doctor_firstname,
       p2.last_name AS doctor_lastname,
       n.first_name AS nurse_firstname,
       n.last_name AS nurse_lastname,
       t.activity, t.description,t.quantity,
       d.symptoms,
       d2.name AS drug_name
       #d2.condition (not exist)

FROM patients p
JOIN visits v on p.id = v.patient_id
JOIN hospitals h on v.hospital_id = h.id
JOIN physicians p2 on v.doctor_id = p2.id
JOIN nurses n on v.nurse_id = n.id
JOIN visit_treatment vt on v.id = vt.visit_id
JOIN treatment t on vt.treatment_id = t.id
JOIN diseases d on v.disease_id = d.id
JOIN drugs d2 on d2.id=t.drug_id
WHERE p.id=4
ORDER BY 1 DESC;



#  Medicine History

SELECT DISTINCT v.date,
       h.name AS hospital_name,
       p2.first_name AS doctor_firstname,
       p2.last_name AS doctor_lastname,
       t.quantity,
       t.drug_id, d2.name, d.name
FROM patients p
JOIN visits v on p.id = v.patient_id
JOIN hospitals h on v.hospital_id = h.id
JOIN physicians p2 on v.doctor_id = p2.id
JOIN visit_treatment vt on v.id = vt.visit_id
JOIN treatment t on vt.treatment_id = t.id
JOIN diseases d on v.disease_id = d.id
JOIN drugs d2 on d2.id=t.drug_id
WHERE p.id=1 AND drug_id<>0
ORDER BY 1 DESC;

SELECT v.date,
         h.name AS hospital_name,
         CONCAT(p2.first_name, ' ', p2.last_name) AS doctor_name,
         d2.name,
         t.quantity,
         t.drug_id
  FROM patients p
  JOIN visits v on p.id = v.patient_id
  JOIN hospitals h on v.hospital_id = h.id
  JOIN physicians p2 on v.doctor_id = p2.id
  JOIN visit_treatment vt on v.id = vt.visit_id
  JOIN treatment t on vt.treatment_id = t.id
  JOIN diseases d on v.disease_id = d.id
  JOIN drugs d2 on d2.id=t.drug_id
  WHERE p.id=1 AND drug_id<>0
  ORDER BY 1 DESC;

SELECT DISTINCT CONCAT(p.first_name, ' ', p.last_name), h.name,
                CONCAT(h.street, ' ', h.city, ', ', h.state, ' ', h.zip), specialty FROM patients JOIN visits v on patients.id = v.patient_id
 JOIN hospitals h on v.hospital_id = h.id
 JOIN physicians p on v.doctor_id = p.id
 WHERE patients.id = 1;


SELECT DISTINCT h.name,
                CONCAT(h.street, ' ', h.city, ', ', h.state, ' ', h.zip), h.phone_number, h.website FROM patients JOIN visits v on patients.id = v.patient_id
 JOIN hospitals h on v.hospital_id = h.id
 JOIN physicians p on v.doctor_id = p.id
 WHERE patients.id = 1;

SELECT * FROM patients LIMIT 2











