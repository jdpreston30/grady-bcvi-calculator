library(qrcode)
qr <- qr_code("https://grady-bcvi-calc.shinyapps.io/calculator/", ecl = "H") # highest error correction
# Save as high-res PNG (600 DPI, 2000x2000 pixels)
png("BCVI_QR_ultra.png", width = 2000, height = 2000, res = 600)
plot(qr) # This plots the QR matrix
dev.off()
