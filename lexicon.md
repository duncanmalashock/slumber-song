# Project Lexicon & Naming Guide

## 🌎 General Naming Principles

* Use **full dictionary words** (e.g., `position`, not `pos`).
* Use **namespacing by meaning**, not by implementation (e.g., `menuBar` not `menuUi`).
* Favor **verbs for actions**, **nouns for data**, and **adjectives for properties**.
* Prefer **consistency over brevity**.

## 🧩 Consistent Naming Templates

* **Constructors**: `new`
* **Queries**: `get<Property>`, `is<BooleanProperty>`
* **Updates**: `set<Property>`, `update`, `<verb><Noun>`
* **Events**: `on<Event>`, e.g. `onClick`, `onDoubleClick`
* **Transformations**: `to<Something>`, e.g. `toScreenCoordinates`
* **View renderers**: `view`

---

# Domain Concepts

### 🖼️ Screen

| Concept | Lexicon Term | Notes |
| - | - | - |
| The viewable area | `Screen` | Represents the simulated Macintosh monitor. |
| Pixel space | `logical` | The proportions of the screen image in pixels. |
| View location | `Coordinate` | Represents a point (x, y). |
| On-screen container | `Rect` | Bounding area with position and size. |

---

### 📍 UI & Objects

| Concept | Lexicon Term | Notes |
| - | - | - |
| The whole UI | `UI` | The structure or tree of all active `Object`s. |
| A visible element | `Object` | Represents something interactable or displayable. |
| An object’s parent | `Parent` | Represents an `Object` being “attached” to another, i.e. to form a group or container. |
| Rectangle | `Rect` | Includes position & size. |
| Position | `position`, `x`, `y` | Use consistent accessors. |
| Size | `size`, `width`, `height` | Avoid abbreviations like `w`, `h`. |
| Visual style/appearance | `View` | How an `Object` visually appears. |
| Interactions | `SelectOptions` `DragOptions` | How an `Object` appears and behaves during these interactions. |
| Hit testing | `hitTest` | A test for whether a coordinate is within the interactable area of an `Object`. |

---

### 🖱️ Mouse Interaction

| Concept | Lexicon Term | Notes |
| - | - | - |
| The mouse model | `Mouse` | Tracks position and button state. |
| Mouse interaction event | `Event` | |
| Mouse state change | `update`, `lock`, `unlock`, `setCursor` | Use `set` or verb commands for mutation. |
| Locking & unlocking | `locked` | When "locked", the mouse cannot produce events. |
| Cursor styles | `Pointer`, `Watch` | Naming template: `setCursor<Style>` |

---

### 🧱 Layout and Geometry

| Concept | Lexicon Term | Notes |
| - | - | - |
| Coordinate and Rectangle math | `plus`, `minus`, `interpolate` | Functions for tranforming geometry. |

---

### 🧭 Program Structure

| Concept | Lexicon Term | Notes |
| - | - | - |
| OS command | `Instruction` | Single operation to execute. |
---
