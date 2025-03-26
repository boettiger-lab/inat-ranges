#!/bin/bash

kubectl apply -f service.yaml -n biodiversity
kubectl apply -f ingress.yaml -n biodiversity
kubectl apply -f secret-deployment.yaml -n biodiversity

kubectl get pods -n biodiversity

