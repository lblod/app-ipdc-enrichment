import { RESOURCE_TYPES } from "./constants";

export const initialization = {
  "ipdc-enriched": Object.fromEntries(
    RESOURCE_TYPES.map((type) => [
      type,
      {
        graphFilter: `VALUES ?g {
                        <http://mu.semte.ch/graphs/ipdc/ldes-data>
                        <http://mu.semte.ch/graphs/ipdc/enrichments>
                      }`,
      },
    ]),
  ),
};
