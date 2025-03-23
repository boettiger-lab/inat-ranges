#! /bin/bash

# S3 Bucket requires CORS rules before we can easily stream from it on HTTPS 

aws s3api --endpoint-url ${AWS_S3_ENDPOINT} put-bucket-cors --bucket public-data --cors-configuration '{
  "CORSRules": [
    {
      "AllowedOrigins": ["*"],
      "AllowedMethods": ["GET"],
      "ExposeHeaders": ["ETag", "Content-Type", "Content-Disposition"],
      "AllowedHeaders": ["*"]
    }
  ]
}'


aws s3api --endpoint-url https://s3-west.nrp-nautilus.io get-bucket-cors --bucket public-data
