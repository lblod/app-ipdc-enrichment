# mu-project

Bootstrap a mu.semte.ch microservices environment in three easy steps.

## Quickstart an mu-project

> [INFO]
> This project was created by running `mu project new awesome-project-name`.  If read on GitHub under mu-semtech/mu-project then it is the template repository for a new project, use `mu project new` instead.

Setting up your environment is done in three easy steps:
1. First configure the running microservices and their names in `docker-compose.yml`
2. Then, configure how requests are dispatched in `config/dispatcher.ex`
3. Lastly, simply start the docker-compose.

### Boot up the system
#### From scratch
Boot your microservices-enabled system using docker-compose.
```
    cd /path/to/app-ipdc-enrichment
    docker compose up -d
```
You can shut down using `docker-compose stop` and remove everything using `docker-compose rm`.

## Troubleshooting
### Full flush and restart
:warning: this takes time. Think about why you need that and how you can optimize before doing this.
```
    cd /path/to/app-ipdc-enrichment
    drc down
    rm -rf data
    git checkout data
    docker compose up -d
```
### Restarting the full ipdc-sync from scratch
```
drc stop ipdc-ldes-consumer
rm -rf ./data/ldes-consumer/*.json
# In triplestore: CLEAR GRAPH <http://mu.semte.ch/graphs/ipdc/ldes-data>
drc up -d ipdc-ldes-consumer
```
### Initial sync of the ldes-producer
In case you have data in the database, that hasn't been published to ldes yet.
Or, you want to flush the feed and restart again.
```
mu script project-scripts init-ldes-data http://localhost/ldes-feed  # the latter argument depends on the deploy environment
```
### Healing ldes-feed
```
drc exec ldes-delta-pusher curl -X POST  http://localhost/manual-healing
```
