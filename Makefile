AGENT_VERSION ?=

.PHONY: release
release:
	[ -n "$(AGENT_VERSION)" ]
	docker build --tag restyled/agent:v$(AGENT_VERSION) .
	docker push restyled/agent:v$(AGENT_VERSION)
