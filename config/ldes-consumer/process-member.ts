import type { Client } from "ldes-client";
import { DataFactory } from "n3";
const { namedNode } = DataFactory;
const { quad, variable } = DataFactory;
import type * as RDF from "@rdfjs/types";

import { convertBlankNodes } from "../lib/utils";
import { executeDeleteInsertQuery } from "../lib/sparql-queries";


// ldes-client doesn't expose the `Member` type directly...
type Member =
  ReturnType<Client["stream"]> extends ReadableStream<infer M> ? M : never;

export async function processMember(
  member: Member
) {
  member.quads = convertBlankNodes(member.quads);
  const quadsToAdd: RDF.Quad[] = member.quads.filter((quad) => !quad.predicate.equals(namedNode('http://www.w3.org/ns/prov#generatedAtTime')));
  const quadsToRemove: RDF.Quad[] = [
    quad(member.id as RDF.Quad_Subject, variable("p"), variable("o")),
  ];
  await executeDeleteInsertQuery(quadsToRemove, quadsToAdd);
}