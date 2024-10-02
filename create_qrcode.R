# QR Code for the app online URL:
library(qrcode)


# Define the URL
my_url <- "https://rkanitz.shinyapps.io/reproquest/"

# Generate the QR code
qr_out <- qr_code(my_url)
plot(qr_out)
