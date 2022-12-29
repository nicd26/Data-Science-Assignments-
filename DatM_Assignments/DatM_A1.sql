## Nicole Dunn NDU31 MBIS623 Assignment 1 Queries
 
  
#Query 1 - Date Completeness Measures
SELECT 	
		100.0 * SUM(CASE WHEN `Closed Date` IS NULL THEN 1 ELSE 0 END) / COUNT(*) AS closed_date_percentage,
		100.0 * SUM(CASE WHEN `Created Date` IS NULL THEN 1 ELSE 0 END) / COUNT(*) AS created_date_percentage,
		100.0 * SUM(CASE WHEN `Due Date` IS NULL THEN 1 ELSE 0 END) / COUNT(*) AS due_date_percentage
FROM
   nyc311.service_request_sample_10k;
   
#Query 2 - Date Accuracy Measures  
SELECT 	
		100.0 * SUM(CASE WHEN `Closed Date` < `Created Date` THEN 1 ELSE 0 END) / COUNT(*) AS error_percentage,
        100.0 * SUM(CASE WHEN `Closed Date` = '1900-01-01 00:00:00' THEN 1 ELSE 0 END) / SUM(CASE WHEN `Closed Date` < `Created Date` THEN 1 ELSE 0 END) AS default_percentage
FROM
   nyc311.service_request_sample_10k;
   
#Query 3 - Incident Zip Completeness Measures
SELECT
		100.0 * SUM(CASE WHEN `Incident Zip` = '' or `Incident Zip` IS NULL THEN 1 ELSE 0 END) / COUNT(*) AS zip_percentage
FROM
   nyc311.service_request_sample_10k;

#Query 4 - Accuracy of Service Request Zip Codes vs Zip Codes in NY
SELECT
		100.0 * SUM(CASE WHEN LEFT(`Incident Zip`, 5) IN (SELECT `Zip` FROM nyc311.zip_code_nyc_borough) THEN 1 ELSE 0 END) / COUNT(*) AS zip_comparison_match,
		100.0 * SUM(CASE WHEN LEFT(`Incident Zip`, 5) NOT IN (SELECT `Zip` FROM nyc311.zip_code_nyc_borough) THEN 1 ELSE 0 END) / COUNT(*) AS zip_comparison_differ
FROM 
   nyc311.service_request_sample_10k;

#Query 5 - Validity Check of the Incident Zip Summary table
SELECT
		100.0 * (SUM(Count) / (SELECT SUM(Count) FROM sr_incident_zip_summary)) AS incident_zip_validity
FROM sr_incident_zip_summary
WHERE `Incident Zip` NOT IN (SELECT `Zip` FROM nyc311.zip_code_nyc_borough);

#Query 6 - Validity Check of the Complaint Type Summary table
SELECT 
		100.0 * (SUM(Count) / (SELECT SUM(Count) FROM sr_complaint_type_summary)) AS complaint_type_validity
FROM sr_complaint_type_summary
WHERE LOWER(REGEXP_REPLACE(`Complaint Type`, '[^[:alnum:]]+', ''))
	IN (SELECT LOWER(REGEXP_REPLACE(`Type`, '[^[:alnum:]]+', ''))
		FROM nyc311.ref_sr_type_nyc311_open_data_26);

#Query 7 - Borough Completeness Measures
SELECT 	
		100.0 * SUM(CASE WHEN `Borough` IS NULL THEN 1 ELSE 0 END) / COUNT(*) AS borough_null_values
FROM
   nyc311.service_request_sample_10k;

#Query 8 - Borough Completeness Measures   
SELECT 	
		100.0 * SUM(CASE WHEN `Borough` = 'Unspecified' THEN 1 ELSE 0 END) / COUNT(*) AS borough_null_values
FROM
   nyc311.service_request_sample_10k;   
   
#Query 9 - Accuracy of the Boroughs vs Boroughs in NY
SELECT
		100.0 * SUM(CASE WHEN `Borough` IN (SELECT `Borough` FROM nyc311.zip_code_nyc_borough) THEN 1 ELSE 0 END) / COUNT(*) AS borough_comparison_match,
		100.0 * SUM(CASE WHEN `Borough` NOT IN (SELECT `Borough` FROM nyc311.zip_code_nyc_borough) THEN 1 ELSE 0 END) / COUNT(*) AS borough_comparison_differ
FROM 
   nyc311.service_request_sample_10k;

