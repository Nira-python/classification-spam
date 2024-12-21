

## STEPWISE LINEAR REGRESSION

# Read in Data
spam = read.csv("/Users/kulnir/Downloads/spambase_csv.csv")

df = spam

y = spam$class
x1 = spam$word_freq_make
x2 = spam$word_freq_address
x3 = spam$word_freq_all
x4 = spam$word_freq_3d
x5 = spam$word_freq_our
x6 = spam$word_freq_over
x7 = spam$word_freq_remove
x8 = spam$word_freq_internet
x9 = spam$word_freq_order
x10 = spam$word_freq_mail
x11 = spam$word_freq_receive
x12 = spam$word_freq_will
x13 = spam$word_freq_people
x14 = spam$word_freq_report
x15 = spam$word_freq_addresses
x16 = spam$word_freq_free
x17 = spam$word_freq_business
x18 = spam$word_freq_email
x19 = spam$word_freq_you
x20 = spam$word_freq_credit
x21 = spam$word_freq_your
x22 = spam$word_freq_font
x23 = spam$word_freq_000
x24 = spam$word_freq_money
x25 = spam$word_freq_hp
x26 = spam$word_freq_hpl
x27 = spam$word_freq_george
x28 = spam$word_freq_650
x29 = spam$word_freq_lab
x30 = spam$word_freq_labs
x31 = spam$word_freq_telnet
x32 = spam$word_freq_857
x33 = spam$word_freq_data
x34 = spam$word_freq_415
x35 = spam$word_freq_85
x36 = spam$word_freq_technology
x37 = spam$word_freq_1999
x38 = spam$word_freq_parts
x39 = spam$word_freq_pm
x40 = spam$word_freq_direct
x41 = spam$word_freq_cs
x42 = spam$word_freq_meeting
x43 = spam$word_freq_original
x44 = spam$word_freq_project
x45 = spam$word_freq_re
x46 = spam$word_freq_edu
x47 = spam$word_freq_table
x48 = spam$word_freq_conference
x49 = spam$char_freq_.3B
x50 = spam$char_freq_.28
x51 = spam$char_freq_.5B
x52 = spam$char_freq_.21
x53 = spam$char_freq_.24
x54 = spam$char_freq_.23
x55 = spam$capital_run_length_average
x56 = spam$capital_run_length_longest
x57 = spam$capital_run_length_total


# fit linear model with 57 predictors
model1 = lm(y ~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13 + x14 + x15 + x16
            + x17 + x18 + x19 + x20 + x21 + x22 + x23 + x24 + x25 + x26 + x27 + x28 + x29 + x30 + x31
            + x32 + x33 + x34 + x35 + x36 + x37 + x38 + x39 + x40 + x41 + x42 + x43 + x44 + x45 + x46
            + x47 + x48 + x49 + x50 + x51 + x52 + x53 + x54 + x55 + x56 + x57)
summary(model1)



library(MASS)
model_step = step(model1,direction = "both") # stepwise linear regression model
summary(model_step)


library(tidyr)
library(ggplot2)

library(Hmisc)
par(mfrow=c(2,2))
hist(df) # histogram of original data

df_log = log(df)
hist(df_log) # histogram of log data
