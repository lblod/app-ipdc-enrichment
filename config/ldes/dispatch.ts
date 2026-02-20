import { moveTriples } from "../support";
import { Changeset, Quad } from "./types";
import { querySudo } from "@lblod/mu-auth-sudo";
import { sparqlEscapeUri } from "mu";
import { RESOURCE_TYPES } from "./constants";
const LDES_BASE = process.env.LDES_BASE;

if (!LDES_BASE) {
  throw "missing LDES_BASE";
}

console.log(LDES_BASE);

export default async function dispatch(changesets: Changeset[]) {
  const filteredChangesets = filterOutIrrelevantChanges(changesets);
  const subjects = mapToSubjects(filteredChangesets);
  if(!subjects.length){
    return;
  }
  console.log("Dispatching...");
  for (const subject of subjects) {
    const {
      results: { bindings },
    } = await querySudo(/* sparql */ `
        CONSTRUCT {
            ?s ?p ?o
        } WHERE {
            GRAPH <http://mu.semte.ch/graphs/ipdc/ldes-data> {
              ?s a ?type;
                ?p ?o.
            }
            VALUES ?s {${sparqlEscapeUri(subject)} }
            FILTER (?type IN (
                ${RESOURCE_TYPES.map((type) => sparqlEscapeUri(type)).join(",\n")}
          ))
        }
        `);
    if (bindings.length) {
      console.log("SUCCESS");
      try {
        await moveTriples([
          {
            inserts: bindings.map(({ s, p, o }) => {
              return { subject: s, predicate: p, object: o };
            }),
          },
        ]);
      } catch (e) {
        console.log("FAILURE");
        console.log("==================================================");
        console.log(e);
        console.log({
          inserts: bindings.map(({ s, p, o }) => {
            return { subject: s, predicate: p, object: o };
          }),
        });
        console.log("==================================================");
      }
    }
  }
}

// Note: the reason we have this filter here:
//  This is to mitigate a side effect of the track-modified-service, which
//   updates the modified date a bit later than the effective change
//  To avoid the multiple re-publish of a resource mearly because the
//   modified date has been updated, we add the filter here.
//  The track-modified-service is needed for the healing to work correctly.
function filterOutIrrelevantChanges(changesets: Changeset[]): Changeset[] {
  return changesets.map((changeset) => {
    const filterFn = (quad: Quad) =>
      quad.predicate.value !== "http://purl.org/dc/terms/modified";
    return {
      inserts: changeset.inserts.filter(filterFn),
      deletes: changeset.deletes.filter(filterFn),
    };
  });
}

export function mapToSubjects(changesets: Changeset[]) {
  const subjects = new Set<string>();
  for (const changeset of changesets) {
    changeset.inserts.forEach((insert) => {
      subjects.add(insert.subject.value as string);
    });
    changeset.deletes.forEach((deleteTriple) => {
      subjects.add(deleteTriple.subject.value as string);
    });
  }
  return Array.from(subjects);
}
