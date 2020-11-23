install:
	stack install

deploy:
	docker build .
	heroku container:push worker
	heroku container:release worker
