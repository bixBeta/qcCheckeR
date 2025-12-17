# QC-CheckeR Reactive Variables - Quick Reference

## Data Flow Diagram

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                              FILE UPLOAD                                     │
│                                  │                                           │
│                                  ▼                                           │
│  ┌─────────────────────────────────────────────────────────────────────┐    │
│  │                         data_rv (Global)                             │    │
│  │  ├── meta        ─────► Current metadata (editable)                  │    │
│  │  ├── orig_meta   ─────► Original metadata (for reset)                │    │
│  │  ├── counts      ─────► Synced to counts_rv on init                  │    │
│  │  └── orig_counts ─────► Original counts (for reset)                  │    │
│  └─────────────────────────────────────────────────────────────────────┘    │
│                                  │                                           │
│                                  ▼                                           │
│  ┌─────────────────────────────────────────────────────────────────────┐    │
│  │                        counts_rv (Global)                            │    │
│  │  ├── counts      ─────► Working count matrix (columns = labels)      │    │
│  │  └── orig_counts ─────► Original for undo/reset                      │    │
│  └─────────────────────────────────────────────────────────────────────┘    │
│                                  │                                           │
│          ┌───────────────────────┼───────────────────────┐                   │
│          ▼                       ▼                       ▼                   │
│  ┌──────────────┐       ┌──────────────┐       ┌──────────────┐             │
│  │ Metadata     │       │ Count Matrix │       │ PCA Analysis │             │
│  │ Editor       │       │ Viewer       │       │              │             │
│  └──────────────┘       └──────────────┘       └──────────────┘             │
│          │                       │                       │                   │
│          ▼                       ▼                       ▼                   │
│  ┌──────────────┐       ┌──────────────┐       ┌──────────────┐             │
│  │ current_data │       │ dynamic_data │       │   pca_rv     │             │
│  │ (reactiveVal)│       │ (reactive)   │       │              │             │
│  └──────────────┘       └──────────────┘       └──────────────┘             │
│          │                                               │                   │
│          └─────────────► meta_out$data ◄─────────────────┘                   │
│                              │                                               │
│                              ▼                                               │
│                      ┌──────────────┐                                        │
│                      │   Outputs    │                                        │
│                      │  (UI/Plots)  │                                        │
│                      └──────────────┘                                        │
└─────────────────────────────────────────────────────────────────────────────┘
```

## Core Reactive Variables

### Global Data Stores (app.R server)

| Variable | Type | Key Fields | Purpose |
|----------|------|------------|---------|
| `data_rv` | reactiveValues | meta, orig_meta, counts, orig_counts | Primary data container |
| `counts_rv` | reactiveValues | counts, orig_counts | Working count matrix |
| `pca_rv` | reactiveValues | pca_result, pca_data, computed | PCA results storage |
| `meta_out` | reactiveValues | data | Bridge to edited metadata |

### Module-Level Variables

| Variable | Module | Type | Purpose |
|----------|--------|------|---------|
| `current_data` | editableTableServer | reactiveVal | Current table state |
| `deleted_stack` | editableTableServer | reactiveVal | Undo stack |
| `dynamic_data` | linkedTableServer | reactive | Filtered counts |

### Module Returns (APIs)

| Module | Returns | Key Functions |
|--------|---------|---------------|
| `storage` | sessionStorageServer | `session_id()`, `load_session()`, `save_initial_data()` |
| `email` | emailNotificationsServer | `notifications_enabled()`, `track_change()` |

## Output Flags for Conditional Panels

| Output | Controls | TRUE When |
|--------|----------|-----------|
| `output$file_uploaded` | Sidebar visibility | Data loaded |
| `output$pca_computed` | PCA results panel | PCA complete |

## Key Relationships

1. **data_rv → counts_rv**: On file upload, `data_rv$counts` syncs to `counts_rv$counts`
2. **current_data → meta_out$data**: Editable table exposes its state via `meta_out`
3. **meta_out$data → dynamic_data**: Linked table filters counts by current labels
4. **counts_rv + data_rv → pca_rv**: PCA uses current counts and metadata
5. **All changes → email$track_change()**: Changes logged for email batching
