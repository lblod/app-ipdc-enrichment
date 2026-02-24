import { RESOURCE_TYPES } from "./constants";

export const initialization = {
  "ipdc-enriched": Object.fromEntries(
    RESOURCE_TYPES.map((type) => [type, {}])
  ),
};
