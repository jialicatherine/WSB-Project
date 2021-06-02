import pandas as pd
import requests
import json
import time


data = [] # list that holds scraped data

q_time = 1717505763 #max query time, June 4, 2024 12:56:03 PM
#stop_q_time_at = 1531177447 #minimum query time, July 9, 2018 11:04:07 PM first WSB DD post
stop_q_time_at = 1609459199 # min query time, December 31, 2020 11:59:59 PM
path_to_file_save = "2021_raw_wsb_dd_submissions.csv"


i=0

subreddit ="wallstreetbets"
targets = ['title', "link_flair_text", 'selftext', 'author', 'created_utc', 'upvote_ratio', 'score', "num_comments", "all_awardings", 'permalink']
targets = ",".join(targets) #so the API returns only what it needs

def getPushshiftData(sub, before, filters):
    url = 'https://api.pushshift.io/reddit/submission/search?size=1000&subreddit='+sub+"&before="+ before + "&filter=" + filters 
    r = requests.get(url)
    if r.status_code!=200: #if error
      print("Error code: ", r.status_code) #print code
      print(time.strftime('%Y-%m-%d %H:%M:%S', time.localtime(q_time))) #prints query time stuck
      time.sleep(25) #pauses for 8.5 seconds to avoid error 429
      return getPushshiftData(sub, before, filters) #trys again
    data = json.loads(r.text)
    return data['data']

def save_data(path_save, data):
  sub_data = pd.concat(data) #join scraped data
  sub_data = sub_data.drop_duplicates(subset=["permalink", "created_utc"]) # remove duplicates
  sub_data = sub_data.loc[sub_data['link_flair_text'] == "DD"] # Only DD flair
  sub_data = sub_data[['title', 'selftext', 'author', 'created_utc', 'upvote_ratio', 'score', "num_comments", "all_awardings", 'permalink']] #reorder cols

  sub_data.to_csv(path_save, index=False)


print("Starting scrape")
while q_time>stop_q_time_at:
  i+=1
  time.sleep(0.25)
  holder = getPushshiftData(subreddit, str(q_time), targets) #gets data 
  data.append(pd.DataFrame(holder)) #converts to pandas and adds to data list
  q_time = holder[len(holder)-1]["created_utc"]
  if i%100==0:
    print("checkpoint saving, i = ", i)
    save_data(path_to_file_save, data)

print("Finished Scraping")
save_data(path_to_file_save, data)