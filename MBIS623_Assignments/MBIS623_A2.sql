use schema nyc311.ndu31_mbis623;

--TASK 1
alter session set week_start = 7;
insert into dim_yearweek
select distinct (yearofweek("Created_Date") * 100 + weekiso("Created_Date")) as yearweek
from nyc311.SERVICE_REQUEST_ALL
where "Created_Date" > date('2022-02-13')
order by yearweek;

-- 12 Rows inserted

--TASK 2
create or replace view sr_full_new as
select "Unique_Key" as unique_key,
    (yearofweek("Created_Date") * 100 + weekiso("Created_Date")) as yearweek,
    "Created_Date" as created_date, "Closed_Date" as closed_date, "Agency" as agency,
    "Agency_Name" as agency_name, "Complaint_Type",
    "Type_ID" as complaint_type_id, "Descriptor" as descriptor, "Location_Type" as location_type,
    "Zip" as incident_zip_id, "Incident_Address" as incident_address, "Street_Name" as street_name,
    "Cross_Street_1" as cross_street_1, "Cross_Street_2" as cross_street_2,
    "Intersection_Street_1" as intersection_street_1, "Intersection_Street_2" as intersection_street_2,
    "Address_Type" as address_type, "City" as city, "Landmark" as landmark,
    "Facility_Type" as facility_type, "Status" as status, "Due_Date" as due_date,
    "Resolution_Description" as resulution_description,
    "Resolution_Action_Updated_Date" as resolution_action_updated_date,
    "Community_Board" as community_board, "BBL" as bbl, "Borough" as borough,
    "X_Coordinate_(State Plane)" as x_coordinate_state_plane,
    "Y_Coordinate_(State Plane)" as y_coordinate_state_plane,
    "Open_Data_Channel_Type" as open_data_channel_type,
    "Park_Facility_Name" as park_facility_name, "Park_Borough" as park_borough, "Vehicle_Type" as vehicle_type,
    "Taxi_Company_Borough" as taxi_company_borough,
    "Taxi_Pick_Up_Location" as taxi_pick_up_location, "Bridge_Highway_Name" as bridge_highway_name,
    "Bridge_Highway_Direction" as bridge_highway_direction,
    "Road_Ramp" as road_ramp, "Bridge_Highway_Segment" as bridge_highway_segment,
    "Latitude" as latitude, "Longitude" as longitude, "Location" as location
from nyc311."SERVICE_REQUEST_ALL"
left join map_complaint_type_open_nyc311
    on map_complaint_type_open_nyc311.complaint_type = lower(nyc311."SERVICE_REQUEST_ALL"."Complaint_Type")
left join map_incident_zip_nyc_borough
    on map_incident_zip_nyc_borough."Zip" = nyc311."SERVICE_REQUEST_ALL"."Incident_Zip"
where "Created_Date" > date('2022-02-13');
select count(*) from sr_full_new;

insert into fact_service_quality (agency_id, location_zip, type_id, yearweek, count, total, avg, min, max)
select dim_agency.agency_id, dim_location.location_zip, dim_request_type.type_id,
    sr_full_new.yearweek,
    count(*),
    sum(timestampdiff(hour, created_date, closed_date)),
    avg(timestampdiff(hour, created_date, closed_date)),
    min(timestampdiff(hour, created_date, closed_date)),
    max(timestampdiff(hour, created_date, closed_date))
from sr_full_new
inner join dim_agency dim_agency on sr_full_new.Agency = dim_agency.agency_name
inner join dim_location dim_location on sr_full_new.incident_zip_id = dim_location.location_zip
inner join dim_request_type dim_request_type on sr_full_new.complaint_type_id = dim_request_type.type_id
inner join dim_yearweek dim_yearweek on sr_full_new.yearweek = dim_yearweek.yearweek
group by dim_agency.agency_id, dim_location.location_zip, dim_request_type.type_id, sr_full_new.yearweek;

-- 27950 rows inserted

--TASK 3A
select agency_name, round(sum(total)/sum(count), 2) Average
from fact_service_quality as f
join dim_agency using(agency_id)
group by agency_name
order by Average;

--TASK 3B
select "Borough" Borough, round(sum(total)/sum(count), 2) "Average Time"
from fact_service_quality as f
join nyc311.zip_code_nyc_borough as z
on f.location_zip = z."Zip"
group by Borough
order by "Average Time"


--TASK 3C
SELECT monthname(dateadd('week', substr(yearweek , 5, 2)::int - 1, substr(yearweek , 0, 4)||'-01-01')) AS month, sum(count) AS service_counts
from fact_service_quality
GROUP BY month
order by month(TO_DATE(month, 'MMMM'));

--TASK 4
select monthname("Created_Date") AS month, COUNT(*)
from "NYC311"."NYC311"."SERVICE_REQUEST_ALL"
GROUP BY month
order by month(TO_DATE(month, 'MMMM'));

