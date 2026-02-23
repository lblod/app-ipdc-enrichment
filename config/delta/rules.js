export default [
  {
    match: {
      subject: {},
    },
    callback: {
      url: "http://resource/.mu/delta",
      method: "POST",
    },
    options: {
      resourceFormat: "v0.0.1",
      gracePeriod: 15000,
      foldEffectiveChanges: true,
      ignoreFromSelf: true,
    },
  },
  {
    match: {
      // any
    },
    callback: {
      url: "http://ldes-delta-pusher/publish",
      method: "POST",
    },
    options: {
      resourceFormat: "v0.0.1",
      ignoreFromSelf: true,
      gracePeriod: 15000,
    },
  },
  {
    match: {
    },
    callback: {
      url: 'http://uuid-generation/delta',
      method: 'POST'
    },
    options: {
      resourceFormat: 'v0.0.1',
      gracePeriod: 250,
      foldEffectiveChanges: false,
      ignoreFromSelf: true,
      _comment: "We explicitly don't want to fold effective changes, since uuid-generation service only handles on inserts with pattern '?s a <type-uri>'"
    }
  },
  {
    match: {
      subject: {},
    },
    callback: {
      url: "http://modified/delta",
      method: "POST",
    },
    options: {
      resourceFormat: "v0.0.1",
      gracePeriod: 10000,
      retry: 3,
      ignoreFromSelf: true,
      retryTimeout: 250,
    },
  },
];
