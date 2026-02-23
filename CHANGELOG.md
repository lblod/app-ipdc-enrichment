# Changelog
## Unreleased
 - Setup ldes-feed: DL-7102
 
### Deploy notes
Ensure in `docker-compose.override.yml`
```
services:
  # (...)
  ldes-delta-pusher:
    environment:
      LDES_BASE: "https://qa.ipdc-ldes-mirror.lblod.info/ldes-feed/"
```

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

