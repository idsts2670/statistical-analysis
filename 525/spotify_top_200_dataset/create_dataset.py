import os
import traceback
import csv
from spotify import Spotify
from client import client_id_secret

# header
PATH_ORIGINAL   = "spotify-dataset-pivot.csv"
PATH_ENHANCED   = "spotify-dataset-enhanced.csv"

col_num = 40
cols = [COL_TR_ID, COL_TR_NAME, COL_TR_POP, COL_TR_NUM, COL_AL_ID, COL_AL_NAME, 
        COL_AL_IMG, COL_AL_TYPE, COL_AL_LABEL, COL_AL_TR_NUM, COL_AL_POP, COL_AR_NUM, 
        COL_AR_NAMES, COL_AR_ID, COL_AR_NAME, COL_AR_IMG, COL_AR_FOL, COL_AR_POP, 
        COL_AR_GENRES, COL_RANK, COL_WEEK, COL_COLLAB, COL_EXPLICIT, COL_RELEASE_DATE, 
        COL_DANCEABILITY, COL_ENERGY, COL_KEY, COL_MODE, COL_TIME_SIG, COL_LOUDNESS, 
        COL_SPEECHINESS, COL_ACOUSTICNESS, COL_INSTRUMENTALNESS, COL_LIVENESS, COL_VALENCE, 
        COL_TEMPO, COL_DURATION, COL_PIVOT, COL_STREAMS, COL_TR_INDEX] = range(col_num)

# init pre-loop
last_track_index = input("#~~ enter start track index:")
client_index = -1 # to be incremented

# run program
while client_index+1 < len(client_id_secret):

    # init loop
    client_index += 1
    client_id = client_id_secret[client_index][0]
    client_secret = client_id_secret[client_index][1]
    print("#~~ current client id: ", client_id)
    
    try:
        # init spotify object
        spotify = Spotify(client_id, client_secret)

        #~~ load progress ~~#
        # manual load
        if (type(last_track_index) == str and last_track_index.isnumeric()):
            last_track_index = int(last_track_index)
            print("#~~ starting from track index: ", last_track_index)
        # automatic load
        else:
            
            # read and delete enhanced file
            rows = None
            with open(PATH_ENHANCED, 'r', encoding="utf-8") as enhanced_file:
                csv_reader = csv.reader(enhanced_file, delimiter=';')
                rows = [row for row in csv_reader]
            os.remove(PATH_ENHANCED)

            # set last track index
            try:
                last_track_index = int(rows[-1][COL_TR_INDEX].strip())
            except:
                last_track_index = 1

            # create and write new enhanced file
            with open(PATH_ENHANCED, 'w+', encoding="utf-8") as enhanced_file:
                csv_writer = csv.writer(enhanced_file, delimiter=';')

                # write rows to new enhanced file
                for i in range(len(rows)):
                    row = rows[i]
                    
                    # write header
                    if i == 0:
                        csv_writer.writerow(row)
                        continue


                    # if catched or skipped last track, stop
                    track_index = int(row[COL_TR_INDEX].strip())
                    if track_index >= last_track_index:
                        break

                    csv_writer.writerow(row)

            print("#~~ loaded successfully, last track index: ", last_track_index)

        #~~ enhancing ~~#
        # csv reader & writer objects
        original_file = open(PATH_ORIGINAL, "r", encoding="utf-8")
        enhanced_file = open(PATH_ENHANCED, "a", encoding="utf-8")
        csv_reader = csv.reader(original_file, delimiter=';')
        csv_writer = csv.writer(enhanced_file, delimiter=';')

        # iterate through rows of original file
        for row in csv_reader:
            # skip 'til last track index
            if csv_reader.line_num-1 < last_track_index:
                continue

            #~~ track id operations ~~#
            track = spotify.track(row[COL_TR_ID])

            row[COL_TR_ID] = track["id"]
            row[COL_TR_POP] = track["popularity"]
            row[COL_TR_NUM] = track["track_number"]
            row[COL_AL_ID] = track["album"]["id"]
            row[COL_AR_NUM] = len(track["artists"])
            row[COL_AR_NAMES] = ', '.join(x["name"] for x in track["artists"])
            row[COL_COLLAB] = row[COL_AR_NUM] > 1
            row[COL_EXPLICIT] = row[COL_EXPLICIT] == "TRUE"
            row[COL_TR_INDEX] = csv_reader.line_num-1


            #~~ album id operations ~~#
            album = spotify.album(row[COL_AL_ID])
            
            row[COL_AL_TYPE] = album["album_type"]
            row[COL_AL_LABEL] = album["label"]
            row[COL_AL_POP] = album["popularity"]
            row[COL_RELEASE_DATE] = album["release_date"]

            # some albums may not have image
            try:
                row[COL_AL_IMG] = album["images"][0]["url"]
            except:
                row[COL_AL_IMG] = ""

            #~~ audio features operations ~~#
            audio_features = spotify.audio_features(row[COL_TR_ID])

            row[COL_DANCEABILITY] = audio_features["danceability"]
            row[COL_ENERGY] = audio_features["energy"]
            row[COL_KEY] = audio_features["key"]
            row[COL_MODE] = audio_features["mode"]
            row[COL_TIME_SIG] = audio_features["time_signature"]
            row[COL_LOUDNESS] = audio_features["loudness"]
            row[COL_SPEECHINESS] = audio_features["speechiness"]
            row[COL_ACOUSTICNESS] = audio_features["acousticness"]
            row[COL_INSTRUMENTALNESS] = audio_features["instrumentalness"]
            row[COL_LIVENESS] = audio_features["liveness"]
            row[COL_VALENCE] = audio_features["valence"]
            row[COL_TEMPO] = audio_features["tempo"]


            #~~ artist id operations ~~#
            pivot = 0
            for i in range(row[COL_AR_NUM]):
                artist = spotify.artist(track["artists"][i]["id"])

                row[COL_AR_ID] = artist["id"]
                row[COL_AR_NAME] = artist["name"]
                row[COL_AR_FOL] = artist["followers"]["total"]
                row[COL_AR_POP] = artist["popularity"]
                row[COL_AR_GENRES] = ', '.join(x for x in artist["genres"])
                row[COL_PIVOT] = pivot

                # some artist may not have image
                try:
                    row[COL_AR_IMG] = artist["images"][0]["url"]
                except:
                    row[COL_AR_IMG] = ""
                
                # write row
                csv_writer.writerow(row)
                print(f"~~ {row[COL_TR_INDEX]} indexed {row[COL_TR_NAME]} written")
                
                pivot = 1


        # close files
        original_file.close()
        enhanced_file.close()

    # if we face any exceptions, simply changing client keys and retrying
    except Exception:
        print("#~~ faced an error: ")
        print(traceback.format_exc())
        print("#~~ starting program with different key")