.PHONY: help build build-docker-image deploy run push

DEFAULT_GOAL: help

PROJECT_NAME ?= hfp

require-%:
	@ if [ "${${*}}" = "" ]; then \
		echo "ERROR: Environment variable not set: \"$*\""; \
		exit 1; \
	fi

## Build project
build:
	@mkdir -p build
	@stack build --copy-bins --local-bin-path=build

## Run docker image
run: require-CI_REGISTRY_IMAGE require-CI_PIPELINE_ID
	@docker run -it -p 3000:3000 "${CI_REGISTRY_IMAGE}:pipeline-${CI_PIPELINE_ID}"

## Push docker image
push: require-CI_REGISTRY_IMAGE require-CI_PIPELINE_ID
	@docker push "${CI_REGISTRY_IMAGE}:pipeline-${CI_PIPELINE_ID}"

## Build docker image. Used in CI/CD
build-docker-image: require-CI_REGISTRY_IMAGE require-CI_PIPELINE_ID
	@docker build -t "${CI_REGISTRY_IMAGE}:pipeline-${CI_PIPELINE_ID}" .

## Deploy using helm
deploy: require-CI_ENVIRONMENT_NAME require-CI_PIPELINE_ID
	@echo "Deploying build pipeline: ${CI_PIPELINE_ID}"
	@kubectl set image deployment/hfp-prod hfp="${CI_REGISTRY_IMAGE}:pipeline-${CI_PIPELINE_ID}"

## Show help screen.
help:
	@echo "Please use \`make <target>' where <target> is one of\n\n"
	@awk '/^[a-zA-Z\-\_0-9]+:/ { \
		helpMessage = match(lastLine, /^## (.*)/); \
		if (helpMessage) { \
			helpCommand = substr($$1, 0, index($$1, ":")-1); \
			helpMessage = substr(lastLine, RSTART + 3, RLENGTH); \
			printf "%-30s %s\n", helpCommand, helpMessage; \
		} \
	} \
	{ lastLine = $$0 }' $(MAKEFILE_LIST)
