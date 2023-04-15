import requests
import json

# api_key = "D2BDA4EA-6790-4D70-9197-2CCCD4287810"
# api_key = "44D387A0-FF31-4969-85CA-939EB16C6B13"
#
# url = "https://api.amc.com/v1/movies"

# url = "https://api.amctheatres.com/v2/theatres/610/showtimes/10-22-2014?page-size=1"
url = "https://api.amctheatres.com/v2/movies/views/advance"
#url = "https://api.sandbox.amctheatres.com/"
headers = {

    "X-AMC-Vendor-Key": "7EBC091D-3259-4C83-B06B-8445A4AA15F2"
}

response = requests.get(url, headers=headers)
print(response.text)
