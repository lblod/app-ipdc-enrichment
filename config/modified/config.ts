import { Changeset } from "../types";

export const interestingTypes = [
  "https://productencatalogus.data.vlaanderen.be/ns/ipdc-lpdc#InstancePublicServiceSnapshot",
  "http://data.europa.eu/m8g/Requirement",
  "http://purl.org/vocab/cpsv#Rule",
  "http://data.europa.eu/m8g/Cost",
  "http://data.europa.eu/m8g/Evidence",
  "http://schema.org/WebSite",
  "https://productencatalogus.data.vlaanderen.be/ns/ipdc-lpdc#FinancialAdvantage",
  "http://schema.org/ContactPoint",
  "http://www.w3.org/ns/locn#Address",
  "http://data.europa.eu/eli/ontology#LegalResource",
  "https://www.w3.org/ns/activitystreams#Tombstone"
];

export const filterModifiedSubjects = "";

export async function filterDeltas(changeSets: Changeset[]) {
  const modifiedPred = "http://purl.org/dc/terms/modified";
  const subjectsWithModified = new Set();

  const trackModifiedSubjects = (quad) => {
    if (quad.predicate.value === modifiedPred) {
      subjectsWithModified.add(quad.subject.value);
    }
  };
  changeSets.forEach((changeSet) => {
    changeSet.inserts.forEach(trackModifiedSubjects);
  });

  const isGoodQuad = (quad) => !subjectsWithModified.has(quad.subject.value);
  return changeSets.map((changeSet) => {
    return {
      inserts: changeSet.inserts.filter(isGoodQuad),
      deletes: changeSet.deletes.filter(isGoodQuad),
    };
  });
}
