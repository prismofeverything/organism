# OGF v2 proposal: view, replay, and verify

Status: semantic replay remains a proposal. The v2 **view** profile now implements
ring-letter coordinates and saved palettes; see [ogf-view-v2.md](ogf-view-v2.md).
Existing v1 files remain readable.

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

The authoritative timeline is an ordered sequence of semantic operations: setup
choices, organism activations/mode choices, and committed actions with complete
arguments. Identify the actor and expected logical phase. Encode spaces, stable
piece identities, element types and food allocations, never neural-network action
indices. Each action belongs to a player turn and organism activation; mode choice
and remaining action slots must be reconstructible even when no piece moves.

Keep an optional decision trace for intermediate selections, abandoned choices and
search observations, linked to committed action IDs. This matters because the
Clojure choice engine selects a complete growth-contribution map while Rust picks
individual donor contributions. Both can describe the same committed Grow without
requiring identical internal decision trees. Do not silently equate a Rust training
choice count with a portable action count. A checkpoint inside an unfinished action
must include pending selections/payments; committed actions alone cannot reconstruct
that intermediate boundary. The trace must declare its decision vocabulary if exact
substep reconstruction is claimed.

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
4. Share fixtures between Clojure, Rust and Python: odd/even circulation; all three home
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
3. Implement Clojure and Rust encoders/decoders plus Python reference checks.
   Start with the base rules supported by training; reject unsupported mutations.
4. Switch training and site exports to replay-capable v2 once they pass the same
   fixtures. Keep the viewer's v1 adapter and clearly label unknown provenance.
5. Add derived snapshots, stable identity animation and annotations to the shared
   viewer. Preserve unknown optional metadata on round-trip; require support for
   extensions that change gameplay. Do not silently ignore a future major version.

The first release is successful when the supported engines can load the same two-/three-
player record, replay every decision, agree on all supplied checkpoints and the
result, and resume play from a saved decision boundary. The viewer must still open
existing v1 exports without claiming guarantees they cannot provide.

## Concrete action requirements (September 2026 clarification)

This table is a proposed semantic vocabulary, not an implemented JSON schema.
Every operation needs an ordered event ID and actor. Each committed action also
references its player turn, organism activation and action slot. References must
resolve against the immediately preceding logical state.

| Operation | Required player choices | Determined/checked by pinned rules |
| --- | --- | --- |
| Introduce | Exact mapping of newly identified Eat/Grow/Move pieces to home spaces | Initial piece food; free-food destruction on all three homes; adjacent food preserved |
| Activate organism | The selected component, using an unambiguous piece reference or declared member set | Membership, eligibility, previously acted components |
| Choose mode | Eat, Grow or Move for that activation | Action budget from the selected element count at the prescribed boundary |
| Eat | Receiving eater and the adjacent source space, even when that space has zero food | Legality, generated food, collection of source food |
| Grow | New piece identity and type, destination, donor-piece-to-food-amount allocation | Current growth cost, legal donors and destination, consumed and collected food |
| Move | Existing piece identity and destination; source can be a checked assertion | Mobility, legal destination, food collection and later automatic consequences |
| Circulate | Source and destination piece identities | Half the source food rounded up under the current rules; conservation |
| Pass | Which action slot is passed | Slot consumption, no direct board action; any automatic advancement |

For circulation and growth, an exported computed amount/cost can be an assertion
for explanations, but must not override the rules. For growth, different donor
allocations are different decisions, even if the same piece type appears at the
same destination. The underlying operation of a partially selected action may be
retained in the decision trace when it ends in Pass.

A compact illustrative committed operation (field spelling remains provisional):

```json
{
  "event": 42,
  "actor": "p0",
  "turn": 8,
  "activation": "activation-12",
  "slot": 1,
  "operation": "grow",
  "piece": "piece-19",
  "element": "move",
  "destination": "space-14",
  "payments": {"piece-3": 2, "piece-7": 1}
}
```

This example is not a claim that the move is legal without its setup and preceding
state. Validation must establish the selected Grow mode, correct cost and donors,
available food, empty legal destination, and a fresh identity for the created piece.

Automatic effects need a normative order: action effects, conflict resolution,
integrity, capture credit, victory checks, next-player transition and start-turn
center credit at the exact boundaries required by the rules. The order above is
not a proposed universal ordering; the rule specification must derive and test the
actual schedule. If existing code makes a result depend on insertion/enumeration
order, that behavior must be made explicit or corrected before claiming portable
replay. Never rely on JSON object-member ordering to settle play.

## Beyond legal replay

- Resume from complete logical checkpoints, including pending actions where the
  declared trace vocabulary supports them; distinguish display-only snapshots.
- Viewer navigation by player turn, activation, committed action, decision substep
  and automatic resolution; identity-preserving movement/capture animation.
- Optional derived explanations such as "three food paid", "center credit" or
  "integrity loss", checked against replay rather than applied as extra commands.
- Annotations and alternative lines anchored to event/state identities. Variations
  are separate branches, not extra actions appended to the actual game's main line.
- Presentation preferences (generated palette or saved theme) separate from space,
  player and piece identity. The current viewer may continue generating colors.
- Live-stream chunks with game identity, sequence continuity, a declared starting
  checkpoint, and explicit incomplete status. A sliding 64-frame live window must
  not masquerade as a complete game history.
- Training/search metadata with action-vocabulary version, checkpoint identity,
  value perspective/player order, search budget, and episode termination policy.
  Search trees and tensors can remain optional sidecars so normal files stay small.

The next concrete specification deliverables are the operation/phase table,
canonical state contract, rules/automatic-transition specification, JSON schema,
and small valid/invalid conformance files. Compression, signatures and a richer
analysis tree can follow without blocking the first interoperable replay format.
