import argparse
import concurrent
import json
import re
from typing import List
from google.cloud import storage
from google.api_core.client_options import ClientOptions
from google.cloud.speech_v2 import SpeechClient
from google.cloud.speech_v2.types import cloud_speech
import os
import datetime
from dotenv import load_dotenv
from google.cloud import storage
from google.api_core.exceptions import NotFound

def list_bucket_files(bucket_name):
    """List all file paths in a Google Cloud Storage bucket.

    Args:
        bucket_name (str): The name of the GCS bucket.

    Returns:
        List of file paths (str) in the bucket.
    """
    # Initialize the GCS client
    storage_client = storage.Client()

    # Get the bucket
    bucket = storage_client.bucket(bucket_name)

    file_paths = []

    # List all objects (files) in the bucket
    blobs = bucket.list_blobs()

    for blob in blobs:
        # The file path is the blob's name
        file_paths.append(f"gs://{bucket_name}/{blob.name}")


    return file_paths


def transcribe_batch_multiple_files_v2(
    project_id: str,
    gcs_uris: List[str],
    gcs_output_path: str,
    json_filename: str,
) -> cloud_speech.BatchRecognizeResponse:
    """Transcribes audio from a Google Cloud Storage URI.
    Args:
        project_id: The Google Cloud project ID.
        gcs_uris: The Google Cloud Storage URIs to transcribe.
        gcs_output_path: The Cloud Storage URI to which to write the transcript.
    Returns:
        The BatchRecognizeResponse message.
    """
    # Instantiates a client
    client = SpeechClient(
        client_options=ClientOptions(
            api_endpoint="us-central1-speech.googleapis.com",
        )
    )
    config = cloud_speech.RecognitionConfig(
        auto_decoding_config=cloud_speech.AutoDetectDecodingConfig(),
        language_codes=["en-US"],
        model="chirp",
        features=cloud_speech.RecognitionFeatures(
            enable_automatic_punctuation=True,
        ),# this is the model for Google Chirp
    )
    files = [
        cloud_speech.BatchRecognizeFileMetadata(uri=uri)
        for uri in gcs_uris
    ]
    request = cloud_speech.BatchRecognizeRequest(
        recognizer=f"projects/{project_id}/locations/us-central1/recognizers/_",
        config=config,
        files=files,
        recognition_output_config=cloud_speech.RecognitionOutputConfig(
            gcs_output_config=cloud_speech.GcsOutputConfig(
                uri=gcs_output_path,
            ),
        ),
    )

    operation = client.batch_recognize(request=request)
    print("Waiting for operation to complete...")

    max_retries = 3
    retry_delay_seconds = 10 
    for retry in range(max_retries):
        try:
            print(f"Attempt {retry + 1}: Waiting for operation to complete...")
            response = operation.result(timeout=300)
            break  
        except concurrent.futures.TimeoutError:
            print(f"Attempt {retry + 1}: Operation timed out. Retrying in {retry_delay_seconds} seconds...")
            time.sleep(retry_delay_seconds)
        except Exception as e:
            print(f"An error occurred: {str(e)}")
            break 
    if retry == max_retries - 1:
        print("Maximum number of retries reached. Operation did not complete successfully.")
    else:
        print("Operation finished successfully.")
    
    print("Operation finished. Fetching results from:")
    transcript_object_list = []
    failed_job_list = []
    for uri in gcs_uris:
        try:
            file_results = response.results[uri]
            if file_results.error:
                response = operation.result(timeout=300)

            output_bucket, output_object = re.match(
                r"gs://([^/]+)/(.*)", file_results.uri
            ).group(1, 2)
            # print(output_bucket, output_object)
            storage_client = storage.Client()
            # Fetch results from Cloud Storage
            bucket = storage_client.bucket(output_bucket)
            blob = bucket.blob(output_object)
            results_bytes = blob.download_as_bytes()
            batch_recognize_results = cloud_speech.BatchRecognizeResults.from_json(
                results_bytes, ignore_unknown_fields=True
            )
            filename = uri.split("/")[-1]
            transcript = ""

            for result in batch_recognize_results.results:
                if result.alternatives:
                    # print(f" Transcript: {result.alternatives[0].transcript}")
                    transcript += result.alternatives[0].transcript
                else:
                    print("No transcription alternatives found.")
                    transcript +=""
            object={"filename":filename, "transcript":transcript}
            transcript_object_list.append(object)
        except NotFound:
            print(f"Transcript for {uri} not found. Skipping.")
            failed_job_list.append(uri)
            continue
 
    print("Saving transcripts to json file...")
    with open(json_filename, 'w') as f:
        json.dump(transcript_object_list, f)
    if len(failed_job_list) > 0:
        if not os.path.exists("GoogleChirp_failed_job_list.json"):
            with open("GoogleChirp_failed_job_list.json", 'w') as outfile:
                json.dump(failed_job_list, outfile)
                print(f"there are {len(failed_job_list)} failed jobs, please check the json file for details")
        else:
            with open("GoogleChirp_failed_job_list.json") as json_file:
                data = json.load(json_file)
                if data is None:
                    data = []
                data.append(failed_job_list)
                new_failed_job_list = data
                new_failed_job_list = data.append(failed_job_list)
            with open("GoogleChirp_failed_job_list.json", 'w') as outfile:
                json.dump(new_failed_job_list, outfile)
                print(f"there are {len(failed_job_list)} failed jobs, please check the json file for details")
    else:
        print("All jobs have been transcribed successfully...")
    return response



def main():

    date = str(datetime.date.today())
    
    json_file_for_transcript = date+"_hearing_Google_Chirp_transcript"
    
    current_directory = os.getcwd()
    print("Current directory:", current_directory)

    data_folder_path = os.path.join(current_directory, "google_transcripts")
    os.makedirs(data_folder_path, exist_ok=True)  # Create the folder if it doesn't exist


    ## set up google credentials
    ## Step 1: Initialize the Google Cloud SDK by running the ‘gcloud init’. This will prompt you to log in to your Google Cloud account and set up the default credentials for your project.
    ## Step 2: Activate the appropriate service account by running following command ‘gcloud auth activate-service-account --key-file=YOUR_SERVICE_ACCOUNT_KEY.json(Replace YOUR_SERVICE_ACCOUNT_KEY.json with the path to your service account key file.).
    ## Step 3: Verify your authentication by running this command ‘gcloud auth list’. This should display the active credentials you’ve set up.
    ## Step 4: Run your python script.
    
    load_dotenv()
    credential_path = os.getenv("GOOGLE_APPLICATION_CREDENTIALS")
    project_id = "dogwood-actor-359017"
    print(f"Project ID: {project_id}")
    print(f"Credential Path: {credential_path}")


    gsc_uris= list_bucket_files("weill_hearing") # change bucket name 

    gsc_output_path = "gs://deaf_transcript" # change to new bucket name that stores transcripts

    # break gsc_uris into batches and each batch size is 15
    total_files = len(gsc_uris) 
    desired_batch_size = 15 # set batch size to 15 because this is the maximum number of files that can be transcribed in one batch

    batch_size = min(total_files, desired_batch_size)

    batch_number = 0  
    for batch_index in range(0, total_files + 1, batch_size):
        batch_number += 1
        # if batch_number < 4:
        #     continue
        print("------Starting batch number ", batch_number)
        if batch_index == total_files:
            print("This is the end of the list")
            break
        batch_pathnames_list = gsc_uris[batch_index: min(batch_index + batch_size, total_files + 1)]

        file = json_file_for_transcript+"_Batch_"+ str(batch_number) + ".json"
        json_filename = os.path.join(data_folder_path, file)

        transcribe_batch_multiple_files_v2(project_id, batch_pathnames_list, gsc_output_path,json_filename)


if __name__ == "__main__":
    main()

