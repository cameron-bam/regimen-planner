# Regimen Planner - Architecture

## Overview

Regimen Planner is a client-side ClojureScript/React application that generates printable medication calendars. It follows a unidirectional data flow: user input → state atom → event expansion pipeline → calendar rendering. All state is persisted in the URL hash, so there is no backend.

## Directory Structure

```
regimen-planner/
├── public/                              # Static output served by GitHub Pages
│   ├── index.html                       # HTML entry point
│   ├── css/main.css                     # Compiled Tailwind CSS
│   └── js/main.js                       # Compiled ClojureScript bundle
├── src/main/                            # Application source
│   ├── regimen_planner.cljs             # Root component and event expansion
│   ├── state.cljs                       # State atom and URL persistence
│   ├── components.cljs                  # Plus-selector menu, sortable list
│   ├── components/
│   │   ├── regimen_event.cljs           # Event form inputs and display
│   │   └── calendar.cljs               # Calendar grid rendering
│   ├── regimen.cljc                     # Regimen templates and expansion logic
│   ├── time.cljc                        # Date range and period utilities
│   ├── time/
│   │   └── transit_helper.cljc          # Transit serializers for date types
│   └── main.css                         # Tailwind source CSS
├── .github/workflows/static.yml         # GitHub Pages deploy workflow
├── shadow-cljs.edn                      # ClojureScript build config
├── deps.edn                             # Clojure dependencies
├── package.json                         # npm dependencies and scripts
├── tailwind.config.js                   # Tailwind CSS config
└── postcss.config.js                    # PostCSS config
```

## Modules

| Module | File | Responsibility |
|--------|------|----------------|
| `regimen-planner` | `regimen_planner.cljs` | Root component, layout, event expansion orchestration |
| `state` | `state.cljs` | Reagent atom, URL hash serialization via Transit + Base64 |
| `components` | `components.cljs` | Plus-selector menu, sortable list (drag-and-drop) |
| `components.regimen-event` | `components/regimen_event.cljs` | Event form fields (text, date, select, pill-selector, time-period) and display |
| `components.calendar` | `components/calendar.cljs` | Month grid, weekday labels, day cells |
| `regimen` | `regimen.cljc` | Event expansion: treatments → dates, medications → dated entries |
| `time` | `time.cljc` | Date range generation, period arithmetic |
| `time.transit-helper` | `time/transit_helper.cljc` | Transit read/write handlers for LocalDate, Period, etc. |

The `.cljc` files are shared between Clojure and ClojureScript, enabling the date and regimen logic to be tested from either host.

## Component Tree

```
[main]
├── Left sidebar (event management)
│   ├── [sortable-list]              ── drag-and-drop timing order
│   ├── [regimen-event-form] × N     ── one per event
│   │   ├── edit mode: [regimen-event-input] per field
│   │   └── view mode: [regimen-event-display]
│   └── [plus-selector]              ── add Treatment / Medication / Footnote
└── Right panel (calendar)
    └── [calendar-month] × N         ── one per year-month
        ├── [calendar-year-month-label]
        ├── [calendar-weekday-label] × 7
        └── [calendar-day] × 28–42
            └── [date-renderer]      ── medications grouped by timing
```

The `main` component toggles between a split-panel layout (sidebar + calendar) and a full-screen calendar view.

## Data Flow

```
User input
    │
    ▼
Reagent state atom (:events, :timings, :full-screen?)
    │
    ├──▶ state watcher ──▶ Transit encode ──▶ Base64 ──▶ URL hash
    │
    ▼
expand-events
    ├── expand-treatments    (occurrences × frequency → multiple dates, with date-override substitution)
    ├── expand-medications
    │   ├── expand-timing          (one entry per timing label)
    │   ├── expand-exact-dates     (start/stop range → daily entries)
    │   └── expand-relative-dates  (offset from treatment dates)
    └── filter events with :date
    │
    ▼
group-by year-month
    │
    ▼
calendar-month renders each month
    │
    ▼
date-renderer displays medications + footnotes per day; treatment titles are clickable to override dates
```

On page load the flow reverses: the URL hash is decoded from Base64, read through Transit, and used to initialize the state atom.

## State Shape

```clojure
{:events   [{:type :medication   ;; or :treatment, :footnote
             :editing? false
             :event {:rx "..." :start #time/date "..." ...}
             ^{:id #uuid "..."}}  ;; metadata carries the unique ID
            ...]
 :timings  [{:id "AM" :name "AM"} {:id "PM" :name "PM"} ...]
 :full-screen? false}
```

Treatment events carry an additional `:date-overrides` field — a map from occurrence index (int) to `LocalDate` — that lets individual occurrences be rescheduled without changing the series parameters.

State is persisted bidirectionally with the URL hash using Cognitect Transit (with custom handlers for `tick` date types) and Base64 encoding. A Reagent watcher updates the hash on every state change.

## Conditional Field Visibility

Event forms use "state tags" to show or hide fields based on current state:

- `:state-tag/treatment-names` — populates related-treatment dropdowns
- `:state-tag/has-related-treatment?` — shows relative date fields
- `:state-tag/not-has-related-treatment?` — shows absolute date fields
- `:state-tag/multiple-occurrences?` — shows frequency field for recurring treatments

These are resolved via `render-state-tag` multimethods.

## Build and Toolchain

| Tool | Purpose |
|------|---------|
| **shadow-cljs** | ClojureScript → JavaScript compilation, dev server (port 8000), hot reload |
| **Tailwind CSS + PostCSS** | Utility-first CSS with autoprefixer; cssnano for production minification |
| **Clojure CLI (deps.edn)** | Resolves Clojure/ClojureScript library dependencies |
| **npm** | Resolves JavaScript dependencies |

**Build commands:**

```bash
npm run release              # Full production build (JS + CSS)
npm run shadow-cljs:release  # ClojureScript only
npm run postcss:release      # CSS only
npm run postcss:watch        # Dev: watch CSS changes
```

## Key Dependencies

| Dependency | Role |
|------------|------|
| **Reagent** (1.2.0) | React wrapper — components as functions returning Hiccup |
| **React** (19.0.0) | UI rendering |
| **tick** (1.0) / **js-joda** (3.2.0) | Date/time: LocalDate, Period, date ranges |
| **cognitect/transit-cljs** (0.8.280) | Serialization for URL state persistence |
| **react-sortablejs** (6.1.4) | Drag-and-drop reordering of timing labels |
| **@heroicons/react** (2.2.0) | UI icons |
| **@tailwindcss/forms** (0.5.9) | Form element styling |

## Deployment

GitHub Actions workflow (`.github/workflows/static.yml`) runs on push to `main`:

1. Checkout → set up Java (Zulu 21) and Clojure CLI
2. `npm ci` → install dependencies
3. `npm run release` → build JS and CSS into `public/`
4. Deploy `public/` to GitHub Pages

The app is served as a fully static site at `https://cameron-bam.github.io/regimen-planner/`.

## CSS Architecture

- **Tailwind utilities** handle most styling inline via class names
- **Custom CSS** (`src/main/main.css`) defines the calendar grid (7-column CSS Grid), day cell sizing, alternating week shading, weekday header colors, and hover effects
- **Print styles** (`@media print`) hide UI controls, reduce day cell height, and insert page breaks between months
