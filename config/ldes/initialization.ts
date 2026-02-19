import { RESOURCE_TYPES } from "./constants";

export const initialization = {
  "ldes-ipdc-enriched": Object.fromEntries(
    RESOURCE_TYPES.map((type) => [type, {}])
  ),
};