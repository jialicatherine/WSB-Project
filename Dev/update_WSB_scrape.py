import pandas as pd
import requests
import json
import time


old = pd.read_csv("Data/raw-historical/2021_raw_wsb_dd_submissions.csv")

stop_q_time_at = max(old["created_utc"]) #minimum query time, previous max

print("Query till: ", stop_q_time_at)

### Below is identical to previous
data = [] # list that holds scraped data
q_time = 1717505763 #max query time, June 4, 2024 12:56:03 PM

subreddit ="wallstreetbets"
targets = ['title', "link_flair_text", 'selftext', 'author', 'created_utc', 'upvote_ratio', 'score', "num_comments", "all_awardings", 'permalink']
targets = ",".join(targets) #so the API returns only what it needs


def getPushshiftData(sub, before, filters):
    url = 'https://api.pushshift.io/reddit/submission/search?size=1000&subreddit='+sub+"&before="+ before + "&filter=" + filters 
    r = requests.get(url)
    if r.status_code!=200: #if error
      print("Error code: ", r.status_code) #print code
      print("Clean time: ", time.strftime('%Y-%m-%d %H:%M:%S', time.localtime(q_time))) #prints query time stuck
      print("Epoch time: ", q_time)
      time.sleep(25) #pauses for 8.5 seconds to avoid error 429
      return getPushshiftData(sub, before, filters) #trys again
    data = json.loads(r.text)
    return data['data']




print("Starting scrape")
while q_time>stop_q_time_at:
  time.sleep(0.25)
  holder = getPushshiftData(subreddit, str(q_time), targets) #gets data 
  data.append(pd.DataFrame(holder)) #converts to pandas and adds to data list
  q_time = holder[len(holder)-1]["created_utc"]

print("Finshed scraping")

sub_data = pd.concat(data) #join scraped data
#sub_data = sub_data.drop_duplicates(subset=["permalink", "created_utc"]) # remove duplicates
sub_data = sub_data.loc[sub_data['link_flair_text'] == "DD"] # Only DD flair
sub_data = sub_data[['title', 'selftext', 'author', 'created_utc', 'upvote_ratio', 'score', "num_comments", "all_awardings", 'permalink']] #reorder cols

sub_data = pd.concat([sub_data, old], ignore_index=True)

sub_data.to_csv("Data/raw-historical/2021_raw_wsb_dd_submissions.csv", index=False)

print("Finshed saving")
