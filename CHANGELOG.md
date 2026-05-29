# Changelog
## Unreleased
- predicate split via sparql-parser [DL-7350]

### deploy notes
```
drc restart migrations && drc logs -ft --tail=200 migrations # wait for all migrations to run
drc restart ldes-delta-pusher database
```

## 1.2.0 [2026-05-22]
- bump frontend to v0.5.1 [DL-7282]
- add filters like loket [DL-7227],[DL-7395]

### deploy notes
```
drc up -d frontend
drc restart migrations
```

## 1.1.0 [2026-04-13]
- Frontend tweaks

### deploy notes
```
drc up -d frontend
```

## 1.0.2 [2026-04-09]
- Fix incorrect `BASE_URL` of the `ldes-backend` service
### deploy notes
- Ensure to set the correct `BASE_URL` for the `ldes-backend` service in `docker-compose.override.yml`
    * For DEV: `BASE_URL: "https://dev.ipdc-verrijking.lblod.info/ldes-feed/"`
    * For QA: `BASE_URL: "https://ipdc-verrijking.lblod.info/ldes-feed/"`
    * For PROD: `BASE_URL: "https://ipdc-verrijking.vlaanderen.be/ldes-feed/"`
    
## 1.0.1 [2026-04-08]
 - [DL-7224] bump + beefing up mu-resource
### deploy notes

## 1.0.0
- Publishing functionality
- Re-deploy ipdc-ldes fix
- [DL-7223]: add extra fields to the public-service detail view form
- [DL-7256]: add the RO classification
### deploy notes
```
    drc down
    rm -rf data
    git checkout data
    docker compose up -d
```
## 0.4.0
- Update `ipdc-ldes-consumer` to `feature-update-ldes-client` tag
- Update `init-ldes-data` script to use correct LDES namespace (https instead of http)
- Update `ldes-delta-pusher` to version 1.2.11
- Introduce publish/unpublish button
## deploy notes
```
    drc down
    rm -rf data
    git checkout data
    docker compose up -d
```
## 0.3.3
- bump frontend v0.2.5
### deploy notes
`drc up -d`
## 0.3.2
- bump frontend v0.2.3
### deploy notes
`drc up -d`
## 0.3.1
- bump frontend to v0.2.2
 - `ipdc-ldes-consumer`: enable LDES request throttling
### deploy notes
`drc up -d`
## 0.3.0
 - Setup ldes-feed: DL-7102
 - Update `ldes-delta-pusher` to version 1.2.10
### deploy notes
The easiest will be to full flush everything
```
    drc down
    rm -rf data
    git checkout data
    docker compose up -d
```

## 0.2.1
 - Fix ACMIDM login issues
### deploy notes   `
 - `drc up -d`
 - `drc restart login database`
## 0.2.0
- [DL-7102] Setup ldes streams to re-publish the data by @aatauil in #9
- DL-7137 [IPDC-Enrich] ACIMDM T&I - Make it functional
- DL-7193 Update LDES_BASE URL to new migrated one
- DL-7168 [IPDC-Enrich] Always fetch same @ nl string in fields
- DL-7191 [IPDC Enrich] Cleanup story
- DL-7194 [IPDC Enrich] Add doelgroep filter
### Deploy notes
`drc restart`

## 0.1.0
- [DL-7144] Add public-service edit functionality
- [DL-7101] Introduce concept/concept-scheme model & route
- [DL-7135] Add codelists & list well-known services & products
- [DL-7118] Add ipdc ldes consumer
- [DL-7100] Introduce semantic-forms functionality

### Deploy notes
`drc restart`
