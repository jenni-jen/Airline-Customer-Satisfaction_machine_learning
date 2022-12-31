airline = read.csv('./Data/airline_processed.csv')
source('functions_CDA.R')

# ========== 2 x 2 Table ==========
# CI for odds ratio, difference in proportions, and relative risk
## gender & satisfaction
t_gender = table(airline$gender, airline$satisfaction)[, c(2,1)]
wald_CI_gender = Wald.ci(t_gender, 'c(1,1)')

## customer type & satisfaction
t_customer_type = table(airline$customer_type, airline$satisfaction)[c(2,1), c(2,1)]
wald_CI_customer_type = Wald.ci(t_customer_type, 'c(1,1)')

## travel type & satisfaction
t_travel_type = table(airline$travel_type, airline$satisfaction)[, c(2,1)]
wald_CI_travel_type = Wald.ci(t_travel_type, 'c(1,1)')

# Pearson’s chi-squared statistics
## gender & satisfaction
chisq.test(t_gender)

## customer type & satisfaction
chisq.test(t_customer_type)

## travel type & satisfaction
chisq.test(t_travel_type)

# ========== 2 x J Table ==========
# non-ordered Pearson’s chi-squared statistics
## class & satisfaction
t_class = table(airline$class, airline$satisfaction)[, c(2,1)]
chisq.test(t_class)

# monotone trend alternative to independence
## seat comfort & satisfaction
m_seat_comfort = as.matrix(table(airline$satisfaction, airline$seat_comfort))
Gamma.f(m_seat_comfort)

## departure/arrival time convenient & satisfaction
m_flight_time = as.matrix(table(airline$satisfaction, airline$flight_time_convenient))
Gamma.f(m_flight_time)

## food and drink & satisfaction
m_food_drink = as.matrix(table(airline$satisfaction, airline$food_drink))
Gamma.f(m_food_drink)

## gate location & satisfaction
m_gate_location = as.matrix(table(airline$satisfaction, airline$gate_location))
Gamma.f(m_gate_location)

## inflight wifi service & satisfaction
m_inflight_wifi = as.matrix(table(airline$satisfaction, airline$inflight_wifi))
Gamma.f(m_inflight_wifi)

## inflight entertainment & satisfaction
m_inflight_entertainment = as.matrix(table(airline$satisfaction, airline$inflight_entertainment))
Gamma.f(m_inflight_entertainment)

## online support & satisfaction
m_online_support = as.matrix(table(airline$satisfaction, airline$online_support))
Gamma.f(m_online_support)

## ease of online booking & satisfaction
m_booking_ease = as.matrix(table(airline$satisfaction, airline$online_booking_ease))
Gamma.f(m_booking_ease)

## on-board service & satisfaction
m_onboard_service = as.matrix(table(airline$satisfaction, airline$onboard_service))
Gamma.f(m_onboard_service)

## leg room service & satisfaction
m_legroom_service = as.matrix(table(airline$satisfaction, airline$leg_room_service))
Gamma.f(m_legroom_service)

## baggage handling & satisfaction
m_baggage_handling = as.matrix(table(airline$satisfaction, airline$baggage_handling))
Gamma.f(m_baggage_handling)

## checkin service & satisfaction
m_checkin_service = as.matrix(table(airline$satisfaction, airline$checkin_service))
Gamma.f(m_checkin_service)

## cleanliness & satisfaction
m_cleanliness = as.matrix(table(airline$satisfaction, airline$cleanliness))
Gamma.f(m_cleanliness)

## online boarding & satisfaction
m_online_boarding = as.matrix(table(airline$satisfaction, airline$online_boarding))
Gamma.f(m_online_boarding)


