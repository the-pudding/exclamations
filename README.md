## Setup

- Clone the repo
- Put the file `secret.json` in the project directory (see Russell)
- Run the `Getting Emails` command below
- Run the `Analysis` command below

## Getting Emails

        Usage:              rscript get_emails.R "regexp for matching outgoing email ids" max_emails True/False
        Sample command:     rscript get_emails.R "russellgoldenberg" 999999 True
        Requires:           secret.json
        Output:             email_data.csv

On first use, it will take a little bit to install the packages. Then, it will tell you to open a browser window to get permission for email id and password. The authorization will then be cached on the local machine for future use. Sometimes the script returns an error when the token expires. Rerun the script and it will work! If you have multiple address, you can pipe delimit (eg. `"russellgoldenberg|smokey"`). The True/False flag is for storing unprocessed data (for debugging).

## Analysis

        Usage:      rscript analysis.R
        Requires:   email_data.csv (generated via getting emails command)
        Output:     Rplots.pdf
