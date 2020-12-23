AWS ?= aws --profile restyled

RELEASE_VERSION ?= $(shell git tag | vbump minor | sed 's/^v//')

.PHONY: release
release:
	[ -n "$(RELEASE_VERSION)" ]
	docker build --tag restyled/agent:v$(RELEASE_VERSION) .
	docker push restyled/agent:v$(RELEASE_VERSION)
	git tag --sign --message "v$(RELEASE_VERSION)" "v$(RELEASE_VERSION)"
	git push --follow-tags

DEPLOY_VERSION ?= $(shell git tag | sort -rV | head -n 1 | sed 's/^v//')
DEPLOY_ENV ?= prod

.PHONY: deploy
deploy:
	[ -n "$(DEPLOY_VERSION)" ]
	docker pull "restyled/agent:v$(DEPLOY_VERSION)" >/dev/null
	$(AWS) cloudformation update-stack \
	  --stack-name "$(DEPLOY_ENV)-machines" \
	  --use-previous-template \
	  --parameters \
	    ParameterKey=Environment,UsePreviousValue=true \
	    ParameterKey=InstanceKeyPair,UsePreviousValue=true \
	    ParameterKey=RestyledHost,UsePreviousValue=true \
	    ParameterKey=DesiredCapacity,UsePreviousValue=true \
	    ParameterKey=UserDataVersion,UsePreviousValue=true \
	    ParameterKey=AgentVersion,ParameterValue=$(DEPLOY_VERSION) \
	  --capabilities CAPABILITY_NAMED_IAM
	$(AWS) cloudformation wait stack-update-complete \
	  --stack-name "$(DEPLOY_ENV)-machines"
