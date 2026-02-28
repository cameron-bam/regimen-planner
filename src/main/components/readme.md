# Components

## `regimen-event-form`

**File:** `components/regimen_event.cljs`

### Purpose

A generic, data-driven form component that renders any event type (medication, treatment, or footnote) in either **edit mode** (input fields) or **view mode** (read-only display). It does not know which fields exist on a given event — it receives a `field-order` vector and renders whatever fields are present on the event in that order.

### Props

| Prop | Type | Description |
|------|------|-------------|
| `event` | map | The event's `:event` sub-map (e.g., `{:rx "" :timing {...} ...}`) |
| `editing?` | boolean | Whether the form is in edit mode |
| `on-change` | fn | Called with the updated form state map when the user saves |
| `toggle-edit` | fn | Toggles between edit and view mode |
| `drop-event!` | fn | Deletes this event from state |
| `field-order` | vector | Ordered list of field keywords to render (e.g., `[:rx :timing :start ...]`) |

### Internal State

Uses a local Reagent atom `form-state` to hold in-progress edits. This is initialized from the `event` prop and is reset back to the prop value whenever the component exits edit mode. Changes are only committed to the parent state when the user clicks **Save**.

### Rendering Logic

1. **Action buttons** — positioned absolutely in the top-right corner:
   - Edit mode: **Save** (CheckIcon) and **Delete** (TrashIcon)
   - View mode: **Edit** (PencilIcon, shown on hover) and **Delete** (TrashIcon, shown on hover)

2. **Field iteration** — iterates over `field-order`, looks up each key in the form state (after resolving state tags), and for each non-nil, non-hidden field:
   - **Edit mode:** dispatches to `regimen-event-input` (multimethod on `:type`)
   - **View mode:** dispatches to `regimen-event-display` (multimethod on `:type`)

3. **Field type inference** — if a field value is a map, uses its `:type` key. If it's a `LocalDate`, uses `"date"`. Otherwise defaults to `"text"`.

4. **Conditional visibility** — fields with `{:hidden? true}` (resolved from state tags) are skipped entirely.

### Field Input Types (`regimen-event-input` multimethod)

| Type | Widget | Behavior |
|------|--------|----------|
| default (text, date, number) | `<input>` | Standard HTML input; type passed through |
| `"textarea"` | `<textarea>` | Multi-line text |
| `"select"` | `<select>` | Dropdown; options from `:options` key |
| `"pill-selector"` | Custom | Text input + Enter to add pills; X to remove |
| `"time-period"` | Custom | Number input + "days" label; produces a `Period` |
| `"relative-time-period"` | Custom | Number + after/before dropdown; produces a signed `Period` |

### Field Display Types (`regimen-event-display` multimethod)

| Type | Rendering |
|------|-----------|
| default | `"Label: value"` — joins collections with commas, converts periods to `"N days"` |
| `"relative-time-period"` | `"Label: N days before/after"` or `"day of treatment"` |
| `"select"` | `"Label: display-name"` — looks up display text from options |

### State Tags

State tags are keyword placeholders (e.g., `:state-tag/has-related-treatment?`) embedded in field maps. Before rendering, `render-state-tags` walks the form state and resolves each tag via the `render-state-tag` multimethod, which has access to global state and the current event. This powers conditional field visibility — for example, exact date fields are hidden when a related treatment is selected, and relative date fields are shown instead.

### Caller Integration

In `regimen_planner.cljs`, the `main` component renders one `regimen-event-form` per event. It provides:

- `on-change` — merges saved form data back into the event in the state atom, then rebuilds timings. For treatment events, if `:start`, `:occurrences`, or `:frequency` changed and `:date-overrides` is non-empty, prompts the user with a confirm dialog before clearing overrides. If the user declines, the series change is also rejected.
- `toggle-edit` — flips the `:editing?` flag on the event in state
- `drop-event!` — removes the event and clears related-treatment references on dependent medications
- `field-order` — a single shared vector used for all event types; fields not present on a given event are simply skipped
