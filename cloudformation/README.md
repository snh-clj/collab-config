# Create stack

```
aws cloudformation create-stack \
  --stack-name abrooks-pairing --template-body file://pairinghost.yaml \
  --parameters ParameterKey=KeyName,ParameterValue=$MY_AWS_SSH_KEYNAME
```

# Wait until stack created...

```
watch -d "aws cloudformation describe-stack-events --stack-name abrooks-pairing"
```

# Observe public IP

```
aws cloudformation describe-stacks --stack-name abrooks-pairing
```

# Use / test host

```
ssh -i ~/.ssh/id_rsa ubuntu@35.166.17.223
```

# Delete stack

```
aws cloudformation delete-stack --stack-name abrooks-pairing
```
