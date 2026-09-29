# FLOW — rules

A turn under FLOW is transposed. Instead of one organism taking all its
actions and then the next, every organism declares first, and then the turn
proceeds **one action at a time, simultaneously**: each organism decides its
first action, and only once all of them have decided does that action commit
and resolve. Then the second action, and so on.

Terms: an organism's **type** is what it declared (EAT, GROW or MOVE). An
**action** is one step of the turn in which every organism with actions left
does one thing. An organism's part in an action is its **choice** (its
declared type, or circulate). **Conflict** is the phase where adjacent enemy
elements meet; one element **disrupts** another there (what used to be called
capturing), and a player's tally of **disrupted elements** is what wins.

## Declaring

1. Click any element of an organism to declare that element's type for the
   organism. Organisms can be declared in any order, and clicking another
   element of an already-declared organism changes its type. There is no
   separate "choose organism" step.
2. Once every organism has a type, declaring is over. An organism gets one
   action per element of its type, counted now. Three eaters and EAT is three
   eat actions.
3. The turn has as many actions as the largest count. An organism whose
   actions are used up sits out the remaining ones.

## An action

4. An action begins from the board as it stands — call it **B**.
5. Every organism with actions left makes one choice. They are clicked on the
   board in any order. A choice is **pending**: it shows on the board but
   changes nothing, and clicking it again takes it back.
6. **Every choice is judged against B alone, never against another pending
   choice.** Whether an element is fed, mobile and alive, where it may move or
   grow, what growth costs (counted on the organism as it stands in B), the eat
   threshold, adjacency to enemies — all read from B.
7. When the last organism's choice is complete, the action **commits**: all
   choices resolve together (rules 12–13), and the result is the next action's
   B.

## Choices that want the same thing

Because every choice reads B, two of them can claim the same thing. These
rules make a set of pending choices legal or illegal as a whole, whatever
order they were clicked in.

8. **Destinations.** An empty space in B may be the destination of at most one
   choice per action — one move into it, or one growth into it.
9. **Not yet vacated.** A space occupied in B cannot be a destination during
   this action, even when a pending move is leaving it. It is free from the
   next action. No swaps, no chains.
10. **Free food.** A space holding free food in B may be claimed by at most one
    choice — eaten from, moved into, or grown into. Eating an empty space with
    no free food yields one and is not exclusive.
11. **One move per element**, and **no spending food that is not there**: food
    drawn from an element by all pending choices (growth payments, circulation)
    may not exceed what it held in B. A circulate sends half, rounded up, of
    its food in B. Food arriving in this action can be spent from the next.

    These two only arise when two declarations act through one organism —
    after two organisms have joined.

## Resolving an action

12. The committed choices are applied to B in a fixed sequence. Every step is
    a sum over choices, so the order they were clicked in cannot matter:
    1. **debit** — take away every growth payment and circulation outflow
    2. **move** — relocate moved elements, carrying their food, collecting free
       food where they land
    3. **grow** — create grown elements, with any free food on their space
    4. **credit** — add eaten food and circulation inflow. Credits follow the
       element, not the space, so food reaches an element that just moved.
13. **Regroup.** Elements that now touch are one organism; an organism that has
    come apart is several. This is B for the next action.

## Which declaration a choice belongs to

14. A pending choice belongs to the organism it was clicked in, not to a
    declaration. The action commits once its choices can be matched one to one
    with the declarations that still have actions:
    - a choice of a type matches a declaration of that type
    - a circulate matches any declaration
    - a declaration matches only choices in an organism its elements now
      belong to

    Two organisms declared MOVE and GROW and have since joined: click a growth
    and a circulate in the joined organism, in either order — the growth is
    GROW's, the circulate MOVE's. After a split, click in whichever half should
    act; there is no separate step to pick one.
15. A declaration passes on its own when nothing is legal for it in B. If its
    options are blocked by another pending choice, that is not a pass — the
    action cannot commit until one of them changes.

    The one exception is when no complete set of choices exists at all — two
    organisms whose only possible choice is the same space. Then the player
    picks which one passes. (Settled by searching the action's choices; a
    board too large to search in time is treated as having no complete set,
    so a player is never left unable to go on.)

## After the last action

16. Conflict, as its own phase, once all actions have resolved.
17. Integrity, then victory. An organism count taken mid-turn — after a split,
    before conflict — does not win.

## What this guarantees

The same set of choices produces the same board whatever order they were
clicked in. That is directly testable, and is the test that holds FLOW to its
word: take an action, commit every ordering of its choices, check the
results are identical.
