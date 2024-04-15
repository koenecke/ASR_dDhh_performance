import os
import json
import azure.cognitiveservices.speech as speechsdk
import datetime
from multiprocessing import Pool

def transcribe_audio_file(audio_file_path, _unused_argument=None):
    try:
        print(f"Processing file: {audio_file_path}")
        speech_config = speechsdk.SpeechConfig(subscription="key", region="region")
        audio_config = speechsdk.AudioConfig(filename=audio_file_path)
        speech_recognizer = speechsdk.SpeechRecognizer(speech_config=speech_config, audio_config=audio_config)

        recognized_text = []

        done = False
        def stop_cb(evt):
            speech_recognizer.stop_continuous_recognition()
            nonlocal done
            done = True

        speech_recognizer.recognized.connect(lambda evt: recognized_text.append(evt.result.text))
        speech_recognizer.session_stopped.connect(stop_cb)
        speech_recognizer.start_continuous_recognition()

        while not done:
            pass

        return {"filename": os.path.basename(audio_file_path), "transcript": ' '.join(recognized_text)}
    except Exception as e:
        print(f"An error occurred while processing {audio_file_path}: {e}")
        return None

def main():
    FOLDER_PATH = "hearing/"

    date = str(datetime.date.today())
    json_filename = date + ("_hearing_Azure_transcript.json")

    wav_files = [(os.path.join(FOLDER_PATH, file_name), None) for file_name in os.listdir(FOLDER_PATH) if file_name.endswith(".wav")]

    num_processes = 10

    with Pool(num_processes) as pool:
        transcriptions = pool.starmap(transcribe_audio_file, wav_files)
    transcriptions = [t for t in transcriptions if t is not None]
    with open(json_filename, 'w') as outfile:
        json.dump(transcriptions, outfile)

    print("Transcripts have been successfully stored in json file...")

if __name__ == "__main__":
    main()
