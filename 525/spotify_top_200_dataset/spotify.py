import requests

class Spotify():
    def __init__(self, client_id, client_secret) -> None:
        self.access_token = requests.post("https://accounts.spotify.com/api/token", {
            'grant_type' : 'client_credentials',
            'client_id' : client_id,
            'client_secret' : client_secret
        }).json()["access_token"]
        self.headers = {"Authorization":f"Bearer {self.access_token}"}
        self.base_url = "https://api.spotify.com/v1/"

    def track(self, id):
        if (len(id) > 50):
            id = id[31:]
        r = requests.get(self.base_url + "tracks/" + id, headers=self.headers)
        return r.json()

    def album(self, id):
        r = requests.get(self.base_url + "albums/" + id, headers=self.headers)
        return r.json()

    def artist(self, id):
        r = requests.get(self.base_url + "artists/" + id, headers=self.headers)
        return r.json()
    
    def audio_features(self, id):
        r = requests.get(self.base_url + "audio-features/" + id, headers=self.headers)
        return r.json()