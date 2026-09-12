# OGF v2 proposal: view, replay, and verify

Status: proposal, not an implemented format. Current exports remain OGF v1.

## The contract

A viewing record must accurately reproduce the recorded board positions. A replay
record must additionally reconstruct every legal decision and its consequences
under an identified ruleset. Consumers must report which guarantee they verified.
JSON serialization alone does not establish either guarantee.

OGF v1 already provides a useful portable visual record: board topology, player
order, piece types/owners/food, free food and capture totals. It is used by the
Clojure exporter, Python animation tools and the training dashboard. Keep those
files readable indefinitely.

## Explicit capabilities

Use a new major version for incompatible structure or semantics. Distinguish
`view` and `replay` profiles. A replay file includes all view information plus
complete setup, initial engine state, and semantic decision events. A v1 import
can become a view record; it cannot acquire missing actions or rule provenance by
conversion. Never infer those facts and label the result verified.

Report validation in stages: structurally valid, internally consistent, legal
replay verified, and cross-engine verified. Unsupported rules or extensions are
an explicit unsupported result, not a successful validation.

## Rules and setup

Separate the OGF schema version from the Organism rules version. A rules reference
identifies an immutable behavioral specification and conformance fixtures. Record
implementation name and source revision separately; a code revision is provenance,
not the definition of the rules. Uncommitted implementations need a source digest
or an explicit dirty/unversioned marker.

Required setup includes player/seat order, first player, board topology, home spaces,
capture and organism victory limits, selected mutations and all their parameters,
and initial resources. For this training variant the rule specification must
explicitly include circulation of half the source food rounded up and introduction
clearing all free food on the three home spaces while preserving adjacent food.
Training repetition limits and choice horizons are episode settings, not official
game rules. Record their values separately.

Use stable opaque space and player IDs, independent of display colors. Board edges
must refer to declared spaces. Define an explicit layout or ring/index mapping for
rendering; arbitrary topology plus rotational symmetry does not uniquely specify
the intended artwork or numbering. Validate connectedness and symmetric adjacency
for supported base boards, with any exceptions defined by a ruleset.

## Authoritative initial state and event stream

Define the complete state needed at a decision boundary: pieces and resources,
capture ledger and pending capture marks, current player, round, organism grouping
or its deterministic derivation, action budget, acted organisms, pending choice,
partial growth payments, and terminal state. Unknown fields affecting legal play
cannot be ignored. A normal new game may derive this state from an exact setup;
a midgame-start record must carry the full state.

Give pieces stable identities, including creation/removal events, so movement and
capture animations need not guess correspondence between identical pieces. Specify
which organism identifiers are stable and which are recomputed after topology changes.

The authoritative timeline is an ordered sequence of semantic decisions. Identify
the actor, expected phase, selected operation and its named arguments. Encode spaces,
piece identities, types and food allocations, never the neural network action index.
For example, selecting a circulation source and destination must identify those
spaces explicitly; the rules determine the amount. Record choice substeps when they
matter for the decision process, and attach a common committed-action/turn ID for
viewer navigation. Define every decision kind and its legal phase in a table.

Distinguish event index, committed action index, player-turn index and round index.
Use zero-based indices in files, with user-facing numbering left to viewers. Specify
whether a snapshot is before or after an event; use initial state plus snapshots
AFTER the referenced event. Wall-clock timestamps are optional observation metadata,
never the authority for event ordering.

Automatic conflict/integrity/scoring transitions occur according to the pinned
ruleset before the next decision boundary. They may be exported as derived explanatory
events, but must not be applied twice. If a ruleset uses chance, record the actual
chance outcomes and their legality; a seed without a specified generator is not a
portable replay mechanism. MCTS randomness is unnecessary to verify a chosen move.

## Snapshots, results, and training provenance

Retain snapshots as an index for instant scrubbing. For replay records they are
checkpoints derived from the initial state and events. Verification compares every
provided checkpoint with the replayed state; disagreement is an error. A disagreement
must not be repaired by silently trusting whichever representation is convenient.
Snapshots alone remain legitimate in the view profile.

Record result status, winner(s) or utilities as specified by the ruleset, and a
precise reason: capture victory, organism victory, rule-defined draw, resignation,
training repetition cutoff, choice horizon, interrupted, or still in progress.
Do not turn a training cutoff into an official drawn game. Preserve the separate
learning target policy: neutral, discarded, or shaped.

Put model/checkpoint digest, player count, search budget, temperature and exploration
settings, episode limits and timestamps in an optional training namespace. This
supports comparisons between runs without making a viewer understand PyTorch.
Reproducing training itself still requires its optimizer, replay buffer and RNG
checkpoints; OGF is not a replacement for that data.

## Validation and conformance

1. Publish a machine-checkable JSON schema plus prose semantics. Reject duplicate
   object keys, nonfinite numbers, invalid integer food/counts and duplicate IDs.
   Bound document size, event count and board size before expensive processing.
2. Validate references, piece occupancy, topology, player identities and chronology.
   Resource conservation checks must account for explicitly defined creation,
   consumption and destruction; introduction is a deliberate food-destruction event.
3. Replay each decision through the requested ruleset, checking actor, phase, legal
   arguments, effects, scoring, termination and recorded checkpoints. A legal-looking
   final board does not prove a legal history.
4. Share fixtures between Clojure and Python: odd/even circulation; all three home
   food removals with adjacent food preserved; growth payments; capture chains;
   integrity and sacrifice; tied victories; automatic turn transitions; and two-
   and three-player complete games. Compare legal decisions and normalized states
   at every boundary, not only final scores. Mutation support needs its own fixtures.
5. Include invalid fixtures: unknown spaces, duplicate occupants, negative food,
   missing donors, wrong player/phase, contradictory snapshots and forged results.
   Replay-state export/import must preserve the legal-action set and resulting states.

Define canonical ordering for state comparisons, normalize omitted defaults, and
keep integers exact. Content hashes can later identify rules, checkpoints and
recordings and detect accidental changes. A hash is not proof of authentic authorship
or legal play; signatures are not needed for the first useful version.

## Compatibility and implementation order

1. Document v1 as view-only. Its `turn` field is a frame index. The current Clojure
   `frame->state` returns capture counts where the live engine uses collections and
   omits other state; expose it as a display conversion rather than playable import.
2. Publish the v2 schema, normalized replay-state contract, decision-kind table and
   small fixtures before changing either exporter.
3. Implement Clojure and Python encoders/decoders and cross-engine legal replay checks.
   Start with the base rules supported by training; reject unsupported mutations.
4. Switch training and site exports to replay-capable v2 once they pass the same
   fixtures. Keep the viewer's v1 adapter and clearly label unknown provenance.
5. Add derived snapshots, stable identity animation and annotations to the shared
   viewer. Preserve unknown optional metadata on round-trip; require support for
   extensions that change gameplay. Do not silently ignore a future major version.

The first release is successful when either engine can load the same two-/three-
player record, replay every decision, agree on all supplied checkpoints and the
result, and resume play from a saved decision boundary. The viewer must still open
existing v1 exports without claiming guarantees they cannot provide.
