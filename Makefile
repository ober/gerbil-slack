docker:
	docker build --rm=true -t slack .
	docker tag slack jaimef/slack

push:
	docker push jaimef/slack
