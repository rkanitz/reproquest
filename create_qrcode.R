# QR Code for the app online URL:
library(qrcode)


# Define the URL
my_url_survey <- "https://s.surveyplanet.com/ef64jql9"
my_url_result <- "https://rkanitz.shinyapps.io/reproquest_results/"

# Generate the QR code
qr_out_survey <- qr_code(my_url_survey)
qr_out_result <- qr_code(my_url_result)
plot(qr_out_survey)
plot(qr_out_result)
plot(qr_out_survey)
