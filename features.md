# Regimen Planner - Features

Regimen Planner is a web-based tool for building printable medication calendars. It helps healthcare professionals and patients visualize medication schedules and treatment regimens.

**Deployed at:** https://cameron-bam.github.io/regimen-planner/

## Event Types

### Medications
- Create medication entries with a prescription name
- Assign timing labels (e.g., AM, PM, or custom)
- Support for **absolute dates** (exact start/stop dates) or **relative dates** (e.g., "5 days before" or "3 days after" a related treatment)
- Link medications to related treatments

### Treatments
- Create treatment entries with a name and start date
- Define recurring treatments with a number of occurrences and a frequency interval (e.g., every 14 days)
- Medications can be scheduled relative to treatment dates
- **Per-occurrence date overrides**: click a treatment name in a calendar day cell to open an inline date picker and reschedule that single occurrence without affecting the rest of the series
- A "Reset" button next to the date picker reverts an overridden date back to its computed value
- Dependent medications with relative dates automatically recalculate when a treatment date is overridden
- Changing a treatment's start date, frequency, or occurrence count prompts the user (via confirm dialog) to clear any existing date overrides

### Footnotes
- Add explanatory notes that appear at the bottom of calendar pages with numbered markers (*, **, ***, etc.)

## Calendar

- Monthly calendar grid layout (Sunday–Saturday)
- Automatically generates separate pages when events span multiple months
- Medications displayed within each day cell, grouped by timing label
- Visual styling: alternating week shading, distinct weekend/weekday header colors, day numbers in each cell

## Timing Management

- Sidebar with a sortable list of all timing labels
- Drag-and-drop reordering to control the display order of timings on the calendar
- New timing labels added automatically when medications are created

## State Persistence via URL

- Application state is serialized to the URL hash (Base64-encoded Transit format)
- The URL updates in real-time as changes are made
- Calendars can be bookmarked and shared by copying the URL

## User Interface

- **Split-panel view**: left sidebar for event management, right side for the calendar
- **Full-screen toggle**: switch between split-panel and full-screen calendar views
- **Add event menu**: floating "+" button with options to add a Treatment, Medication, or Footnote
- **Inline editing**: edit/save/delete controls on each event, with form fields specific to each event type (text inputs, date pickers, period inputs, pill selectors, dropdowns)

## Printing

- Print-optimized CSS hides UI controls and adds page breaks between months
- Clean, professional printed output with all calendar styling preserved

## Tech Stack

- **ClojureScript** with **Reagent** (React wrapper)
- **Tailwind CSS** for styling
- **Shadow-cljs** for builds
- **react-sortablejs** for drag-and-drop
- **Tick / js-joda** for date/time handling
- **Cognitect Transit** for state serialization
