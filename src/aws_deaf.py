import boto3
import json
import time
import os
import requests
import datetime

def transcribe_file(job_name, file_uri):
    transcribe = boto3.client('transcribe')
    try:
        transcribe.start_transcription_job(
            TranscriptionJobName=job_name,
            Media={'MediaFileUri': file_uri},
            MediaFormat='wav',
            LanguageCode='en-US'
        )
    except Exception as e:
        return f"Error while starting transcription job: {str(e)}"
    
    while True:
        try:
            status = transcribe.get_transcription_job(TranscriptionJobName=job_name)
        except Exception as e:
            return f"Error while getting transcription job status: {str(e)}"
        
        if status['TranscriptionJob']['TranscriptionJobStatus'] in ['COMPLETED', 'FAILED']:
            break
        time.sleep(15)

    if status['TranscriptionJob']['TranscriptionJobStatus'] == 'COMPLETED':
        transcript_uri = status['TranscriptionJob']['Transcript']['TranscriptFileUri']
        try:
            response = requests.get(transcript_uri)
        except Exception as e:
            return f"Error while fetching the transcript URI: {str(e)}"
        
        data = response.json()
        return data['results']['transcripts'][0]['transcript']
    else:
        return f"Transcription job failed for {job_name}"

def process_file(file_name):
    job_name = f"{file_name}-{time.time()}"
    file_uri = f"s3://weilldeaf/{file_name}"
    return transcribe_file(job_name, file_uri)

def upload_files_to_s3(directory_path, bucket_name):
    s3_client = boto3.client('s3')
    uploaded_files = []

    for root, dirs, files in os.walk(directory_path):
        for file in files:
            if file.endswith('.wav'):
                file_path = os.path.join(root, file)
                s3_client.upload_file(file_path, bucket_name, file)
                uploaded_files.append(file)

    return uploaded_files

def main():
    os.environ['AWS_ACCESS_KEY_ID'] = 'access_key_id'
    os.environ['AWS_SECRET_ACCESS_KEY'] = 'secret_access_key'
    os.environ['AWS_DEFAULT_REGION'] = 'us-east-1'

    directory_path = "hearing"
    json_file_for_transcript = str(datetime.date.today()) + "_hearing_AWS_transcript.json"

    bucket_name = 'weilldeaf'

    uploaded_files = upload_files_to_s3(directory_path, bucket_name)
    transcription_results = []

    failed_files_list = []

    for file_name in uploaded_files:
        try:
            transcription = process_file(file_name)
            if "Error" not in transcription:
                transcription_results.append({
                    'segment_name': file_name,
                    'aws_transcription': transcription
                })
            else:
                failed_files_list.append({
                    'segment_name': file_name,
                    'error': transcription
                })
        except Exception as e:
            failed_files_list.append({
                'segment_name': file_name,
                'error': f"Unhandled error occurred: {str(e)}"
            })

    if failed_files_list:
        print(f"There are {len(failed_files_list)} failed files in {directory_path}. Please check them individually.")
    else:
        print(f"All files in {directory_path} have been transcribed successfully.")

    with open(json_file_for_transcript, 'w') as json_file:
        json.dump(transcription_results, json_file)
    print(f"Transcription results for {directory_path} have been successfully stored in {json_file_for_transcript}.")

if __name__ == '__main__':
    main()
