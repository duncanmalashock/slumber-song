# Project Lexicon & Naming Guide

## General Naming Principles

* Use **full dictionary words** (e.g., `position`, not `pos`).
* Use **PascalCase** for type names and constructors (e.g., `Screen`, `MouseEvent`).
* Use **camelCase** for values, functions, and fields (e.g., `getDraggable`, `toScreenCoordinates`).
* Use **namespacing by meaning**, not by implementation (e.g., `menuBar` not `menuUi`).
* Favor **verbs for actions**, **nouns for data**, and **adjectives for properties**.
* Prefer **consistency over brevity**.

## 🧩 **Consistent Naming Templates**

* **Constructors**: `new`
* **Queries**: `get<Property>`, `contains<Something>`, `is<BooleanProperty>`
* **Updates**: `set<Property>`, `update`, `bringToFront`, `attachObject`
* **Events**: `on<Event>`, e.g. `onClick`, `onDoubleClick`
* **Transformations**: `to<Something>`, e.g. `toScreenCoordinates`
* **View renderers**: `view`

---

# Domain Concepts

### 🖼️ **UI & Visual**

| Concept | Lexicon Term | Notes |
| - | - | - |
| The viewable area | `Screen` | Represents the simulated Macintosh monitor. |
| Pixel space | `logical` | The proportions of the screen image in pixels. |
| A visible element | `Object` | Represents something interactable or displayable. |
| The whole UI | `UI` | The structure or tree of all active `Object`s. |
| View location | `Coordinate` | Represents a point (x, y). |
| On-screen container | `Rect` | Bounding area with position and size. |
| Visual style/appearance | `View` | Use `view`, `rect`, `image`, `window` etc. to describe how an `Object` appears. |

---

### 🖱️ **Mouse Interaction**

| Concept | Lexicon Term | Notes |
| - | - | - |
| The mouse model | `Mouse` | Tracks position and button state. |
| Mouse interaction event | `Event` | |
| Mouse state change | `update`, `lock`, `unlock`, `setCursor` | Use `set` or verb commands for mutation. |
| Locking & unlocking | `locked` | When "locked", the mouse cannot produce events. |
| Cursor styles | `Pointer`, `Watch` | Naming template: `setCursor<Style>` |

---

### 🧱 **Layout and Geometry**

| Concept | Lexicon Term | Notes |
| - | - | - |
| Rectangle | `Rect` | Includes position & size. |
| Position | `position`, `x`, `y` | Use consistent accessors. |
| Size | `size`, `width`, `height` | Avoid abbreviations like `w`, `h`. |
| Coordinate math | `plus`, `minus`, `interpolate` | Simple arithmetic functions. |
| Hit testing | `hitTest` | A test for whether a coordinate is within the interactable area of an `Object`. |

---

### 🧭 **Program Structure**

| Concept | Lexicon Term | Notes |
| - | - | - |
| OS command | `Instruction` | Single operation to execute. |
---
