# Changelog
## 0.4.0
- Update `ipdc-ldes-consumer` to `feature-update-ldes-client` tag
- Update `init-ldes-data` script to use correct LDES namespace (https instead of http)
- Update `ldes-delta-pusher` to version 1.2.11
- Introduce publish/unpublish button
## deploy notes

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

