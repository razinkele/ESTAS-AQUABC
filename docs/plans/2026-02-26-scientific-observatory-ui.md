# Scientific Observatory UI Redesign — Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Replace the generic Bootstrap UI with a custom "Bathymetric Observatory" dark theme, extract all CSS into an external stylesheet, remove shinyswatch, and restructure Dashboard + Parameters layouts.

**Architecture:** Create `shiny_app/www/aquabc.css` as the single source of styling. Strip all inline `ui.tags.style()` blocks and shinyswatch theme machinery from `app.py`. Restructure Dashboard from cramped 2/2/8 columns to a status-bar + 50/50 layout. Restructure Parameters from 4/8 split to full-width with pill tabs.

**Tech Stack:** Python Shiny, CSS3 (custom properties, backdrop-filter, @keyframes), Google Fonts (Instrument Serif, IBM Plex Sans, IBM Plex Mono), Bootstrap Icons

---

### Task 1: Create the external CSS file with the full Scientific Observatory theme

**Files:**
- Create: `shiny_app/www/aquabc.css`

**Step 1: Create `shiny_app/www/aquabc.css`**

Write the complete stylesheet. This is the largest single deliverable. The file must contain:

```css
/* ═══════════════════════════════════════════════════════════
   AQUABC — Scientific Observatory Theme
   ═══════════════════════════════════════════════════════════ */

/* ── Google Fonts are loaded via <link> in app.py ── */

/* ── CSS Custom Properties ── */
:root {
    --bg-deep:       #0f1923;
    --bg-surface:    #162231;
    --bg-card:       rgba(22, 34, 49, 0.7);
    --bg-card-hover: rgba(22, 34, 49, 0.85);
    --bg-input:      rgba(15, 25, 35, 0.6);
    --border:        rgba(14, 165, 233, 0.15);
    --border-strong: rgba(14, 165, 233, 0.3);
    --accent:        #0ea5e9;
    --accent-glow:   rgba(14, 165, 233, 0.3);
    --accent-dim:    rgba(14, 165, 233, 0.08);
    --text:          #e2e8f0;
    --text-muted:    #94a3b8;
    --text-bright:   #f1f5f9;
    --success:       #10b981;
    --success-glow:  rgba(16, 185, 129, 0.25);
    --danger:        #ef4444;
    --danger-glow:   rgba(239, 68, 68, 0.25);
    --warning:       #f59e0b;
    --warning-glow:  rgba(245, 158, 11, 0.25);
    --info:          #06b6d4;
    --secondary:     #64748b;
    --font-display:  'Instrument Serif', Georgia, serif;
    --font-body:     'IBM Plex Sans', system-ui, sans-serif;
    --font-mono:     'IBM Plex Mono', 'Consolas', monospace;
    --sidebar-width: 230px;
    --sidebar-collapsed-width: 54px;
    --header-height: 52px;
    --radius:        8px;
    --radius-sm:     4px;
}

/* ── Reset & Base ── */
html, body {
    margin: 0;
    padding: 0;
    background: var(--bg-deep);
    color: var(--text);
    font-family: var(--font-body);
    font-size: 14px;
    line-height: 1.5;
    overflow-x: hidden;
}

/* Override Shiny/Bootstrap fill containers */
.container-fluid, .bslib-page-fill {
    padding: 0 !important;
    margin: 0 !important;
    gap: 0 !important;
    background: var(--bg-deep);
}
.shiny-html-output {
    margin: 0;
    padding: 0;
}

/* ── Typography ── */
h1, h2, h3, h4, h5 {
    font-family: var(--font-display);
    color: var(--text-bright);
    font-weight: 400;
    letter-spacing: -0.01em;
}
h6 {
    font-family: var(--font-body);
    font-weight: 600;
    color: var(--text);
    text-transform: uppercase;
    font-size: 0.7rem;
    letter-spacing: 0.08em;
    margin-bottom: 0.5rem;
}
code, pre, .form-control[type="text"][readonly] {
    font-family: var(--font-mono);
}
a {
    color: var(--accent);
}
a:hover {
    color: #38bdf8;
}
hr {
    border-color: var(--border);
    opacity: 1;
}
.text-muted {
    color: var(--text-muted) !important;
}
strong {
    font-family: var(--font-mono);
    font-weight: 600;
    color: var(--text-bright);
}
small, .small {
    color: var(--text-muted);
}

/* ── App Header ── */
.app-header {
    background: linear-gradient(135deg, #0c1520 0%, #162231 50%, #0f1923 100%);
    background-size: 200% 200%;
    animation: headerShift 20s ease-in-out infinite;
    color: var(--text-bright);
    padding: 0 1.5rem;
    height: var(--header-height);
    display: flex;
    align-items: center;
    justify-content: space-between;
    margin: 0;
    border-bottom: 1px solid var(--border);
    position: relative;
    z-index: 100;
}
.app-header::after {
    content: '';
    position: absolute;
    bottom: 0;
    left: 0;
    right: 0;
    height: 2px;
    background: linear-gradient(90deg,
        transparent 0%,
        var(--accent) 20%,
        var(--info) 50%,
        var(--accent) 80%,
        transparent 100%
    );
    opacity: 0.5;
}
@keyframes headerShift {
    0%, 100% { background-position: 0% 50%; }
    50%      { background-position: 100% 50%; }
}
.app-header-title {
    display: flex;
    align-items: center;
    font-family: var(--font-display);
    font-size: 1.3rem;
    font-weight: 400;
    letter-spacing: -0.01em;
}
.app-header-title i {
    color: var(--accent);
    margin-right: 0.6rem;
    font-size: 1.1rem;
}
.app-header .version-badge {
    font-family: var(--font-mono);
    font-size: 0.65rem;
    background: var(--accent-dim);
    color: var(--accent);
    padding: 0.15rem 0.5rem;
    border-radius: 999px;
    margin-left: 0.75rem;
    border: 1px solid var(--border);
}
.app-header .btn.btn-link {
    color: var(--text-muted);
    background: transparent;
    border: none;
    text-decoration: none;
    box-shadow: none;
    padding: 0.4rem;
    border-radius: var(--radius-sm);
    transition: color 0.2s, background 0.2s;
}
.app-header .btn.btn-link:hover,
.app-header .btn.btn-link:focus {
    color: var(--accent);
    background: var(--accent-dim);
    box-shadow: none;
}

/* ── Sidebar ── */
.custom-sidebar {
    width: var(--sidebar-width);
    min-width: var(--sidebar-width);
    background: linear-gradient(180deg, #0c1520 0%, var(--bg-surface) 100%);
    padding: 0;
    transition: width 0.3s ease, min-width 0.3s ease;
    overflow: hidden;
    position: relative;
    flex-shrink: 0;
    display: flex;
    flex-direction: column;
    border-right: 1px solid var(--border);
}
.custom-sidebar.collapsed {
    width: var(--sidebar-collapsed-width);
    min-width: var(--sidebar-collapsed-width);
}
.custom-sidebar.collapsed .nav-link span,
.custom-sidebar.collapsed .sidebar-title {
    display: none;
}
.custom-sidebar.collapsed .nav-link {
    justify-content: center;
    padding: 0.75rem 0;
}
.custom-sidebar.collapsed .nav-link i {
    margin-right: 0;
}
.custom-sidebar.collapsed .sidebar-header {
    justify-content: center;
    padding: 0.75rem 0.5rem;
}
.sidebar-header {
    display: flex;
    align-items: center;
    justify-content: space-between;
    padding: 0.75rem 1rem;
    background: rgba(0, 0, 0, 0.2);
    border-bottom: 1px solid var(--border);
}
.sidebar-title {
    color: var(--accent);
    font-family: var(--font-body);
    font-weight: 700;
    font-size: 0.85rem;
    letter-spacing: 0.04em;
    text-transform: uppercase;
}
.sidebar-toggle {
    background: transparent;
    border: none;
    color: var(--text-muted);
    cursor: pointer;
    font-size: 1.3rem;
    padding: 0.25rem;
    line-height: 1;
    border-radius: var(--radius-sm);
    transition: color 0.2s, background 0.2s;
}
.sidebar-toggle:hover {
    color: var(--accent);
    background: var(--accent-dim);
}
.sidebar-nav {
    padding: 0.5rem 0;
    flex: 1;
}
.custom-sidebar .nav-link {
    color: var(--text-muted);
    padding: 0.6rem 1rem;
    border-radius: 0;
    border-left: 3px solid transparent;
    margin: 1px 0;
    white-space: nowrap;
    display: flex;
    align-items: center;
    text-decoration: none;
    cursor: pointer;
    font-size: 0.85rem;
    font-weight: 500;
    transition: all 0.2s ease;
}
.custom-sidebar .nav-link i {
    margin-right: 0.75rem;
    font-size: 1rem;
    width: 1.25rem;
    text-align: center;
    transition: color 0.2s;
}
.custom-sidebar .nav-link:hover {
    background: rgba(14, 165, 233, 0.06);
    border-left-color: var(--accent);
    color: var(--text-bright);
}
.custom-sidebar .nav-link:hover i {
    color: var(--accent);
}
.custom-sidebar .nav-link.active {
    background: rgba(14, 165, 233, 0.1);
    border-left-color: var(--accent);
    color: var(--text-bright);
    font-weight: 600;
    box-shadow: inset 3px 0 12px var(--accent-glow);
}
.custom-sidebar .nav-link.active i {
    color: var(--accent);
}

/* ── Sidebar Container & Main Content ── */
.sidebar-container {
    display: flex;
    min-height: calc(100vh - var(--header-height));
    margin: 0;
    padding: 0;
}
.main-content {
    flex: 1;
    background: var(--bg-deep);
    padding: 1.25rem;
    min-height: 100%;
    overflow-x: auto;
}

/* ── Cards (Glassmorphism) ── */
.card {
    background: var(--bg-card);
    backdrop-filter: blur(12px);
    -webkit-backdrop-filter: blur(12px);
    border: 1px solid var(--border);
    border-radius: var(--radius);
    margin-bottom: 1rem;
    box-shadow: 0 4px 16px rgba(0, 0, 0, 0.2);
    transition: border-color 0.2s, box-shadow 0.2s;
    animation: cardFadeIn 0.4s ease-out both;
}
.card:hover {
    border-color: var(--border-strong);
    box-shadow: 0 4px 24px rgba(0, 0, 0, 0.3);
}
.card-header {
    font-family: var(--font-display);
    font-weight: 400;
    font-size: 1.05rem;
    background: rgba(14, 165, 233, 0.04);
    border-bottom: 1px solid var(--border);
    color: var(--text-bright);
    padding: 0.65rem 1rem;
}
.card-body {
    color: var(--text);
}

/* Staggered card animation */
@keyframes cardFadeIn {
    from { opacity: 0; transform: translateY(8px); }
    to   { opacity: 1; transform: translateY(0); }
}
.card:nth-child(1) { animation-delay: 0s; }
.card:nth-child(2) { animation-delay: 0.05s; }
.card:nth-child(3) { animation-delay: 0.1s; }
.card:nth-child(4) { animation-delay: 0.15s; }
.card:nth-child(5) { animation-delay: 0.2s; }

/* ── Buttons ── */
.btn {
    font-family: var(--font-body);
    font-weight: 500;
    border-radius: var(--radius-sm);
    transition: all 0.2s ease;
    font-size: 0.85rem;
    letter-spacing: 0.01em;
}
.btn-primary {
    background: var(--accent);
    border-color: var(--accent);
    color: #fff;
}
.btn-primary:hover, .btn-primary:focus, .btn-primary:active {
    background: #0284c7;
    border-color: #0284c7;
    color: #fff;
    box-shadow: 0 0 16px var(--accent-glow);
}
.btn-success {
    background: var(--success);
    border-color: var(--success);
    color: #fff;
}
.btn-success:hover, .btn-success:focus, .btn-success:active {
    background: #059669;
    border-color: #059669;
    color: #fff;
    box-shadow: 0 0 16px var(--success-glow);
}
.btn-danger {
    background: var(--danger);
    border-color: var(--danger);
    color: #fff;
}
.btn-danger:hover, .btn-danger:focus, .btn-danger:active {
    background: #dc2626;
    border-color: #dc2626;
    color: #fff;
    box-shadow: 0 0 16px var(--danger-glow);
}
.btn-warning {
    background: var(--warning);
    border-color: var(--warning);
    color: #fff;
}
.btn-warning:hover, .btn-warning:focus, .btn-warning:active {
    background: #d97706;
    border-color: #d97706;
    color: #fff;
    box-shadow: 0 0 16px var(--warning-glow);
}
.btn-info {
    background: var(--info);
    border-color: var(--info);
    color: #fff;
}
.btn-info:hover, .btn-info:focus, .btn-info:active {
    background: #0891b2;
    border-color: #0891b2;
    color: #fff;
}
.btn-secondary {
    background: var(--secondary);
    border-color: var(--secondary);
    color: #fff;
}
.btn-secondary:hover, .btn-secondary:focus, .btn-secondary:active {
    background: #475569;
    border-color: #475569;
    color: #fff;
}
.btn-outline-primary {
    color: var(--accent);
    border-color: var(--accent);
    background: transparent;
}
.btn-outline-primary:hover, .btn-outline-primary:focus {
    background: var(--accent);
    border-color: var(--accent);
    color: #fff;
}
.btn-outline-secondary {
    color: var(--text-muted);
    border-color: var(--secondary);
    background: transparent;
}
.btn-outline-secondary:hover, .btn-outline-secondary:focus {
    background: var(--secondary);
    border-color: var(--secondary);
    color: #fff;
}
.btn:focus {
    box-shadow: 0 0 0 3px var(--accent-glow);
}
.btn-success:focus { box-shadow: 0 0 0 3px var(--success-glow); }
.btn-danger:focus  { box-shadow: 0 0 0 3px var(--danger-glow); }
.btn-warning:focus { box-shadow: 0 0 0 3px var(--warning-glow); }
.btn-lg {
    font-size: 0.95rem;
    padding: 0.6rem 1.25rem;
}

/* Pulse animation for active run button */
@keyframes runPulse {
    0%, 100% { box-shadow: 0 0 0 0 var(--success-glow); }
    50%      { box-shadow: 0 0 0 8px transparent; }
}
.btn-running {
    animation: runPulse 2s ease-in-out infinite;
}

/* ── Form Controls ── */
.form-control, .form-select {
    background: var(--bg-input);
    border: 1px solid var(--border);
    color: var(--text);
    border-radius: var(--radius-sm);
    font-size: 0.85rem;
    transition: border-color 0.2s, box-shadow 0.2s;
}
.form-control:focus, .form-select:focus {
    background: var(--bg-input);
    border-color: var(--accent);
    color: var(--text);
    box-shadow: 0 0 0 3px var(--accent-glow);
}
.form-control::placeholder {
    color: var(--text-muted);
}
.form-label, label {
    color: var(--text-muted);
    font-size: 0.8rem;
    font-weight: 500;
    margin-bottom: 0.25rem;
}
.form-check-input {
    background-color: var(--bg-input);
    border-color: var(--border-strong);
}
.form-check-input:checked {
    background-color: var(--accent);
    border-color: var(--accent);
}
.form-switch .form-check-input:checked {
    background-color: var(--accent);
}
textarea.form-control {
    font-family: var(--font-mono);
    font-size: 0.8rem;
    background: rgba(0, 0, 0, 0.3);
}

/* Radio buttons */
.shiny-input-radiogroup .radio label,
.shiny-input-checkboxgroup .checkbox label {
    color: var(--text);
}

/* Selectize inputs */
.selectize-input {
    background: var(--bg-input) !important;
    border-color: var(--border) !important;
    color: var(--text) !important;
    border-radius: var(--radius-sm) !important;
}
.selectize-input.focus {
    border-color: var(--accent) !important;
    box-shadow: 0 0 0 3px var(--accent-glow) !important;
}
.selectize-dropdown {
    background: var(--bg-surface) !important;
    border-color: var(--border) !important;
    color: var(--text) !important;
}
.selectize-dropdown .option {
    color: var(--text) !important;
}
.selectize-dropdown .option.active {
    background: var(--accent-dim) !important;
    color: var(--text-bright) !important;
}
.selectize-input .item {
    background: var(--accent-dim) !important;
    color: var(--accent) !important;
    border: 1px solid var(--border) !important;
    border-radius: var(--radius-sm) !important;
}

/* Number inputs / spinbuttons */
input[type="number"] {
    font-family: var(--font-mono);
    background: var(--bg-input);
    border: 1px solid var(--border);
    color: var(--text-bright);
    border-radius: var(--radius-sm);
}
input[type="number"]:focus {
    border-color: var(--accent);
    box-shadow: 0 0 0 3px var(--accent-glow);
}

/* ── Tabs (navset) ── */
.nav-tabs {
    border-bottom: 1px solid var(--border);
}
.nav-tabs .nav-link {
    color: var(--text-muted);
    border: none;
    border-bottom: 2px solid transparent;
    background: transparent;
    font-family: var(--font-body);
    font-size: 0.85rem;
    font-weight: 500;
    padding: 0.6rem 1rem;
    transition: color 0.2s, border-color 0.2s;
}
.nav-tabs .nav-link:hover {
    color: var(--text);
    border-bottom-color: var(--border-strong);
}
.nav-tabs .nav-link.active {
    color: var(--accent);
    border-bottom-color: var(--accent);
    background: transparent;
}
.tab-content {
    background: transparent;
}

/* ── Tables ── */
.table {
    color: var(--text);
    --bs-table-bg: transparent;
    --bs-table-striped-bg: rgba(14, 165, 233, 0.03);
    --bs-table-hover-bg: rgba(14, 165, 233, 0.06);
    border-color: var(--border);
}
.table th {
    font-family: var(--font-body);
    font-weight: 600;
    font-size: 0.75rem;
    text-transform: uppercase;
    letter-spacing: 0.06em;
    color: var(--text-muted);
    border-bottom: 1px solid var(--border-strong);
}
.table td {
    border-color: var(--border);
    font-size: 0.85rem;
}

/* ── Run Log ── */
#dashboard_log_container,
#run_log_mini,
.run-log-container {
    background: rgba(0, 0, 0, 0.4);
    border: 1px solid var(--border);
    border-radius: var(--radius-sm);
    font-family: var(--font-mono);
    font-size: 0.75rem;
    line-height: 1.4;
    color: var(--success);
    padding: 0.75rem;
}
#run_log_mini {
    font-size: 0.75rem !important;
    line-height: 1.3 !important;
    max-height: 600px;
    overflow-y: auto;
}

/* ── Compact Run Parameters card ── */
.run-params-compact .form-group,
.run-params-compact .shiny-input-container {
    margin-bottom: 0.2rem !important;
}
.run-params-compact .form-label,
.run-params-compact label {
    margin-bottom: 0.1rem !important;
    font-size: 0.75rem;
}
.run-params-compact .form-select,
.run-params-compact .form-control {
    padding: 0.2rem 0.4rem;
    font-size: 0.78rem;
    height: auto;
}
.run-params-compact .form-switch {
    margin-bottom: 0.15rem !important;
    min-height: 1.2rem;
}
.run-params-compact hr {
    margin: 0.3rem 0 !important;
}
.run-params-compact .card-header {
    padding: 0.4rem 0.75rem;
    font-size: 0.9rem;
}
.run-params-compact .card-body {
    padding: 0.5rem 0.75rem;
}
.run-params-compact strong.small {
    font-size: 0.72rem;
}
.run-params-compact .btn-lg {
    padding: 0.4rem 0.75rem;
    font-size: 0.88rem;
}
.run-params-compact pre {
    padding: 0.3rem;
    font-size: 0.7rem;
    margin-bottom: 0.3rem;
    background: rgba(0, 0, 0, 0.3);
    border: 1px solid var(--border);
    color: var(--text);
    border-radius: var(--radius-sm);
}

/* ── Offcanvas Panels ── */
.offcanvas {
    background: var(--bg-surface);
    color: var(--text);
    border-left: 1px solid var(--border);
}
.offcanvas-header {
    background: rgba(0, 0, 0, 0.3);
    border-bottom: 1px solid var(--border);
}
.offcanvas-header .offcanvas-title {
    font-family: var(--font-display);
    color: var(--text-bright);
}
.offcanvas-body {
    background: var(--bg-surface);
}
.btn-close-white {
    filter: invert(1);
}

/* ── Tooltips ── */
.tooltip-inner {
    background: var(--bg-surface);
    color: var(--text);
    border: 1px solid var(--border);
    font-size: 0.78rem;
}

/* ── Scrollbar ── */
::-webkit-scrollbar {
    width: 8px;
    height: 8px;
}
::-webkit-scrollbar-track {
    background: var(--bg-deep);
}
::-webkit-scrollbar-thumb {
    background: var(--secondary);
    border-radius: 4px;
}
::-webkit-scrollbar-thumb:hover {
    background: var(--text-muted);
}

/* ── Dashboard Status Bar ── */
.dashboard-status-bar {
    display: flex;
    align-items: center;
    gap: 1.5rem;
    padding: 0.6rem 1rem;
    background: rgba(14, 165, 233, 0.04);
    border: 1px solid var(--border);
    border-radius: var(--radius);
    margin-bottom: 1rem;
    font-size: 0.82rem;
}
.dashboard-status-bar .status-item {
    display: flex;
    align-items: center;
    gap: 0.4rem;
}
.dashboard-status-bar .status-label {
    color: var(--text-muted);
    font-size: 0.72rem;
    text-transform: uppercase;
    letter-spacing: 0.06em;
}
.dashboard-status-bar .status-value {
    font-family: var(--font-mono);
    color: var(--text-bright);
    font-size: 0.82rem;
}
.dashboard-status-bar .status-dot {
    width: 6px;
    height: 6px;
    border-radius: 50%;
    background: var(--success);
    box-shadow: 0 0 6px var(--success-glow);
}
.dashboard-status-bar .status-dot.idle {
    background: var(--secondary);
    box-shadow: none;
}

/* ── Parameter Editor ── */
.param-row {
    display: grid;
    grid-template-columns: 220px 1fr 100px;
    gap: 0.75rem;
    align-items: center;
    padding: 0.5rem 0.75rem;
    border-bottom: 1px solid var(--border);
    transition: background 0.15s;
}
.param-row:hover {
    background: var(--accent-dim);
}
.param-name {
    font-family: var(--font-mono);
    font-size: 0.82rem;
    font-weight: 600;
    color: var(--accent);
}
.param-desc {
    font-size: 0.78rem;
    color: var(--text-muted);
}
.param-value input {
    text-align: right;
}

/* ── Category Pills (for Parameters panel) ── */
.category-pills {
    display: flex;
    flex-wrap: wrap;
    gap: 0.4rem;
    margin-bottom: 1rem;
}
.category-pill {
    padding: 0.3rem 0.75rem;
    border-radius: 999px;
    font-size: 0.78rem;
    font-weight: 500;
    background: var(--accent-dim);
    color: var(--text-muted);
    border: 1px solid var(--border);
    cursor: pointer;
    transition: all 0.2s;
}
.category-pill:hover {
    border-color: var(--accent);
    color: var(--text);
}
.category-pill.active {
    background: var(--accent);
    color: #fff;
    border-color: var(--accent);
}

/* ── Utility ── */
.fade-in {
    animation: cardFadeIn 0.3s ease-out both;
}
.text-accent {
    color: var(--accent);
}
.bg-surface {
    background: var(--bg-surface);
}
.border-accent {
    border-color: var(--border-strong) !important;
}

/* ── Bootstrap Overrides for Dark Theme ── */
.bg-dark { background: rgba(0, 0, 0, 0.3) !important; }
.bg-primary { background: var(--accent) !important; }
.bg-info { background: var(--info) !important; }
.text-light { color: var(--text-bright) !important; }
.text-warning { color: var(--warning) !important; }
.text-danger { color: var(--danger) !important; }
.text-success { color: var(--success) !important; }
.text-primary { color: var(--accent) !important; }

/* Alert boxes */
.alert {
    background: var(--bg-card);
    border: 1px solid var(--border);
    color: var(--text);
    border-radius: var(--radius-sm);
}

/* Modal */
.modal-content {
    background: var(--bg-surface);
    border: 1px solid var(--border);
    color: var(--text);
}

/* Badge */
.badge {
    font-family: var(--font-mono);
    font-weight: 500;
    font-size: 0.7rem;
}

/* Pre/Code blocks */
pre {
    background: rgba(0, 0, 0, 0.3);
    border: 1px solid var(--border);
    color: var(--text);
    border-radius: var(--radius-sm);
    padding: 0.5rem;
    font-family: var(--font-mono);
    font-size: 0.8rem;
}
code {
    color: var(--accent);
    font-family: var(--font-mono);
}

/* ── Plotly chart overrides ── */
.js-plotly-plot .plotly .main-svg {
    background: transparent !important;
}

/* ── ipywidgets in dark mode ── */
.jupyter-widgets {
    color: var(--text);
}

/* ── Responsive ── */
@media (max-width: 768px) {
    .custom-sidebar {
        width: var(--sidebar-collapsed-width);
        min-width: var(--sidebar-collapsed-width);
    }
    .custom-sidebar .nav-link span,
    .custom-sidebar .sidebar-title {
        display: none;
    }
    .custom-sidebar .nav-link {
        justify-content: center;
        padding: 0.75rem 0;
    }
    .custom-sidebar .nav-link i {
        margin-right: 0;
    }
    .main-content {
        padding: 0.75rem;
    }
}
```

**Step 2: Verify the file was created in `shiny_app/www/`**

Run: `ls -la shiny_app/www/aquabc.css`
Expected: File exists with reasonable size (~10-12 KB)

**Step 3: Commit**

```bash
git add shiny_app/www/aquabc.css
git commit -m "feat(ui): add Scientific Observatory external CSS theme"
```

---

### Task 2: Strip shinyswatch and inline CSS from app.py

**Files:**
- Modify: `shiny_app/app.py:131-138` (shinyswatch import)
- Modify: `shiny_app/app.py:300-328` (AVAILABLE_THEMES)
- Modify: `shiny_app/app.py:335-341` (theme logging)
- Modify: `shiny_app/app.py:1145-1156` (get_theme_css function)
- Modify: `shiny_app/app.py:1159` (create_ui signature)
- Modify: `shiny_app/app.py:1199-1413` (nav_css block — DELETE entirely)
- Modify: `shiny_app/app.py:1417-1543` (button_css block — DELETE entirely)
- Modify: `shiny_app/app.py:2840-2843` (bootstrap_icons_css)
- Modify: `shiny_app/app.py:2845-2892` (settings_offcanvas — remove theme selector)
- Modify: `shiny_app/app.py:2976-2996` (content assembly — remove theme CSS, add aquabc.css link and Google Fonts)
- Modify: `shiny_app/app.py:2999-3017` (get_saved_theme + create_ui call)
- Modify: `shiny_app/app.py:4049-4061` (dynamic_theme_css render)
- Modify: `shiny_app/app.py:4303-4326` (handle_theme_change + theme_status)

**Step 1: Remove shinyswatch import block (lines 131-138)**

Replace the try/except import with:
```python
# Theme is handled by custom CSS (shiny_app/www/aquabc.css)
```

**Step 2: Remove AVAILABLE_THEMES (lines 300-328)**

Delete the `AVAILABLE_THEMES = [...]` list and the `THEMES_AVAILABLE` conditional.

**Step 3: Remove theme logging (lines 335-341)**

Delete the theme configuration logging block.

**Step 4: Remove get_theme_css function (lines 1145-1156)**

Delete the entire function.

**Step 5: Simplify create_ui signature (line 1159)**

Change from `def create_ui(theme_name="darkly"):` to `def create_ui():`. Remove the docstring reference to dynamic theme.

**Step 6: Delete nav_css inline style block (lines 1199-1413)**

Delete the entire `nav_css = ui.tags.style("""...""")` block. All these styles are now in `aquabc.css`.

**Step 7: Delete button_css inline style block (lines 1417-1543)**

Delete the entire `button_css = ui.tags.style("""...""")` block.

**Step 8: Add Google Fonts link and aquabc.css link (near line 2840)**

Replace `bootstrap_icons_css` with a block that includes both Bootstrap Icons AND Google Fonts AND the custom CSS:

```python
    # External resources
    external_css = ui.TagList(
        ui.tags.link(
            rel="stylesheet",
            href="https://cdn.jsdelivr.net/npm/bootstrap-icons@1.11.1/font/bootstrap-icons.css"
        ),
        ui.tags.link(
            rel="preconnect",
            href="https://fonts.googleapis.com"
        ),
        ui.tags.link(
            rel="preconnect",
            href="https://fonts.gstatic.com",
            crossorigin=""
        ),
        ui.tags.link(
            rel="stylesheet",
            href="https://fonts.googleapis.com/css2?family=IBM+Plex+Mono:wght@400;500;600&family=IBM+Plex+Sans:wght@400;500;600;700&family=Instrument+Serif&display=swap"
        ),
        ui.tags.link(
            rel="stylesheet",
            href="aquabc.css"
        ),
    )
```

**Step 9: Simplify settings_offcanvas (lines 2845-2892)**

Remove the Appearance card with theme_select and apply_theme. Keep only the About card:

```python
    settings_offcanvas = ui.tags.div(
        ui.tags.div(
            ui.tags.div(
                ui.tags.h5("Settings", class_="offcanvas-title"),
                ui.tags.button(
                    type="button",
                    class_="btn-close btn-close-white",
                    **{"data-bs-dismiss": "offcanvas", "aria-label": "Close"}
                ),
                class_="offcanvas-header"
            ),
            ui.tags.div(
                ui.card(
                    ui.card_header("About"),
                    ui.tags.h5("AQUABC v0.2"),
                    ui.tags.p("Aquatic Biogeochemical Model"),
                    ui.tags.p("A sophisticated water quality simulation tool with:"),
                    ui.tags.ul(
                        ui.tags.li("318 calibratable parameters"),
                        ui.tags.li("36 state variables"),
                        ui.tags.li("Complex biogeochemical processes"),
                    ),
                    fill=False
                ),
                class_="offcanvas-body"
            ),
            class_="offcanvas offcanvas-end",
            tabindex="-1",
            id="settingsOffcanvas",
            **{"aria-labelledby": "settingsOffcanvasLabel"}
        )
    )
```

**Step 10: Update content assembly (lines 2976-2996)**

Replace the content list and return:

```python
    content = [
        external_css,
        nav_js,
        reload_js,
        app_header,
        settings_offcanvas,
        settings_js,
        help_offcanvas,
        help_js,
        changelog_offcanvas,
        changelog_js,
        sidebar_container,
    ]

    return ui.page_fillable(*content, title="AQUABC")
```

Items removed: `bootstrap_icons_css` (replaced by `external_css`), `nav_css`, `button_css`, `ui.output_ui("dynamic_theme_css")`.

**Step 11: Remove get_saved_theme function and simplify app creation (lines 2999-3017)**

Replace:
```python
saved_theme = get_saved_theme()
app_ui = create_ui(saved_theme)
```
With:
```python
app_ui = create_ui()
```

Delete the `get_saved_theme()` function entirely.

**Step 12: Remove dynamic_theme_css render (lines 4049-4061)**

Delete the `current_theme` reactive value, `theme_save_status` reactive value, and the `dynamic_theme_css` render function.

**Step 13: Remove handle_theme_change and theme_status (lines 4303-4326)**

Delete the entire `if THEMES_AVAILABLE:` block containing `handle_theme_change()` and `theme_status()`.

**Step 14: Add a version badge to the header (near line 2790)**

Update the app_header to include a version badge:

```python
    app_header = ui.div(
        {"class": "app-header"},
        ui.div(
            {"class": "app-header-title"},
            ui.tags.i(class_="bi bi-water me-2"),
            "AQUABC",
            ui.tags.span("v0.2", class_="version-badge"),
        ),
        # ... rest stays the same
    )
```

**Step 15: Verify the app starts without errors**

Run: `cd /home/razinka/AQUABCv0.2 && source .venv/bin/activate && timeout 10 python -c "from shiny_app.app import app; print('Import OK')"`
Expected: `Import OK` — no import errors about shinyswatch or missing variables.

**Step 16: Commit**

```bash
git add shiny_app/app.py
git commit -m "refactor(ui): strip shinyswatch and inline CSS, wire external theme"
```

---

### Task 3: Restructure Dashboard layout

**Files:**
- Modify: `shiny_app/app.py` — `panel_dashboard` (lines 1633-1699)

**Step 1: Replace the Dashboard panel definition**

Replace the current `panel_dashboard` (cramped 2/2/8 columns) with:

```python
    panel_dashboard = ui.panel_conditional(
        "input.navigation === 'nav_dashboard'",
        ui.card(
            ui.card_header("Dashboard"),
            # Status bar — horizontal summary strip
            ui.div(
                {"class": "dashboard-status-bar"},
                ui.div(
                    {"class": "status-item"},
                    ui.div({"class": "status-dot idle", "id": "run-status-dot"}),
                    ui.div(
                        ui.div("Status", class_="status-label"),
                        ui.div(ui.output_text("dashboard_status_text", inline=True), class_="status-value"),
                    ),
                ),
                ui.div(
                    {"class": "status-item"},
                    ui.tags.i(class_="bi bi-cpu", style="color: var(--text-muted);"),
                    ui.div(
                        ui.div("Executable", class_="status-label"),
                        ui.div(ui.output_text("dashboard_exe_text", inline=True), class_="status-value"),
                    ),
                ),
                ui.div(
                    {"class": "status-item"},
                    ui.tags.i(class_="bi bi-clock-history", style="color: var(--text-muted);"),
                    ui.div(
                        ui.div("Last Run", class_="status-label"),
                        ui.div(ui.output_text("dashboard_last_run_text", inline=True), class_="status-value"),
                    ),
                ),
                ui.div(
                    {"class": "status-item", "style": "margin-left: auto;"},
                    ui.output_ui("run_timer_display"),
                ),
            ),
            # Two-column layout: actions + system | run log
            ui.layout_columns(
                # Left: Quick actions + system info
                ui.div(
                    ui.layout_columns(
                        ui.tooltip(
                            ui.input_action_button("quick_run", "Quick Run", class_="btn-success btn-lg w-100"),
                            "Run the model with current settings using the selected executable"
                        ),
                        ui.tooltip(
                            ui.input_action_button("dashboard_stop", "Stop", class_="btn-danger btn-lg w-100"),
                            "Stop the currently running model simulation"
                        ),
                        col_widths=[6, 6],
                        class_="mb-3"
                    ),
                    ui.layout_columns(
                        ui.card(
                            ui.card_header("System Status"),
                            ui.div(
                                ui.output_ui("system_status_compact"),
                                style="max-height: 280px; overflow-y: auto; font-size: 0.78rem;"
                            ),
                            fill=False
                        ),
                        ui.card(
                            ui.card_header("Simulation Config"),
                            ui.div(
                                ui.output_ui("input_txt_variables"),
                                style="max-height: 280px; overflow-y: auto; font-size: 0.78rem;"
                            ),
                            fill=False
                        ),
                        col_widths=[6, 6]
                    ),
                    ui.tooltip(
                        ui.input_action_button("goto_model_config", "Model Config", class_="btn-primary btn-sm w-100 mt-2"),
                        "Navigate to Model Control panel to configure simulation settings"
                    ),
                ),
                # Right: Run log
                ui.card(
                    ui.card_header(
                        ui.div(
                            "Run Log",
                            ui.input_action_button("btn_copy_dashboard_log", "Copy", class_="btn-sm btn-outline-secondary float-end"),
                            class_="d-flex justify-content-between align-items-center w-100"
                        )
                    ),
                    ui.div(
                        ui.output_ui("dashboard_run_log"),
                        style="height: 420px; overflow-y: auto; padding: 10px; border-radius: 4px;",
                        class_="run-log-container",
                        id="dashboard_log_container"
                    ),
                    fill=False
                ),
                col_widths=[5, 7]
            )
        )
    )
```

**Step 2: Add the three new dashboard render functions to the server**

Find the existing `system_status_compact` render in the server function and nearby add:

```python
    @render.text
    def dashboard_status_text():
        return "Running" if _model_running[0] else "Ready"

    @render.text
    def dashboard_exe_text():
        try:
            return input.active_executable()
        except Exception:
            return "ESTAS_II"

    @render.text
    def dashboard_last_run_text():
        if _last_run_time[0]:
            return _last_run_time[0].strftime("%Y-%m-%d %H:%M")
        return "Never"
```

Also check if `_last_run_time` exists. If not, add near `_model_running`:

```python
_last_run_time = [None]
```

And set it when a run starts:
```python
_last_run_time[0] = datetime.now()
```

**Step 3: Verify the dashboard renders**

Run the app, navigate to dashboard, check that:
- Status bar shows horizontally
- Buttons are side-by-side
- System Status and Sim Config get 50/50 split
- Run Log gets adequate width

**Step 4: Commit**

```bash
git add shiny_app/app.py
git commit -m "feat(ui): restructure Dashboard with status bar and 50/50 layout"
```

---

### Task 4: Restructure Parameters panel

**Files:**
- Modify: `shiny_app/app.py` — `panel_parameters` (lines 2153-2205)

**Step 1: Replace the Parameters panel definition**

Replace the current `panel_parameters` with a full-width layout:

```python
    panel_parameters = ui.panel_conditional(
        "input.navigation === 'nav_parameters'",
        ui.card(
            ui.card_header("Parameters"),
            # Top bar: file selector + load + category dropdown
            ui.layout_columns(
                ui.tooltip(
                    ui.input_select(
                        "param_file",
                        "Constants file:",
                        choices=["WCONST_04.txt"],
                        selected="WCONST_04.txt"
                    ),
                    "WCONST_04.txt contains calibrated model parameters"
                ),
                ui.tooltip(
                    ui.input_select(
                        "param_category",
                        "Category:",
                        choices=list(PARAMETER_CATEGORIES.keys()),
                        selected="Diatoms"
                    ),
                    "Select parameter category: Diatoms, Cyanobacteria, Zooplankton, etc."
                ),
                ui.tooltip(
                    ui.input_action_button("load_params", "Load", class_="btn-secondary mt-4"),
                    "Load parameters from selected file and category"
                ),
                col_widths=[3, 7, 2]
            ),
            ui.tags.hr(),
            # Category info as a compact inline bar
            ui.div(
                ui.output_text("param_category_info"),
                style="font-size: 0.78rem; padding: 0.4rem 0.75rem; background: rgba(14, 165, 233, 0.04); border-radius: 4px; margin-bottom: 0.75rem; border: 1px solid rgba(14, 165, 233, 0.1);"
            ),
            # Full-width parameter table
            ui.card(
                ui.card_header("Parameters"),
                ui.output_ui("param_table"),
                style="max-height: 550px; overflow-y: auto;"
            ),
            # Save bar
            ui.layout_columns(
                ui.tooltip(
                    ui.input_action_button("save_params", "Save All Changes", class_="btn-success"),
                    "Save modified parameters to file (creates backup)"
                ),
                ui.output_text("param_save_status"),
                col_widths=[3, 9]
            )
        )
    )
```

**Step 2: Verify parameters render with full width**

Run the app, navigate to Parameters. Parameter names and values should have more room.

**Step 3: Commit**

```bash
git add shiny_app/app.py
git commit -m "feat(ui): restructure Parameters panel to full-width layout"
```

---

### Task 5: Update Model Build panel column widths

**Files:**
- Modify: `shiny_app/app.py` — `panel_model_build` (line 1799)

**Step 1: Change the Model Build column widths**

Find `col_widths=[3, 3, 6]` for the model build panel (line 1799) and change to:
```python
col_widths=[3, 3, 6]
```

Actually, the 3/3/6 is fine for build. But let's make sure the build log column has dark log styling:

Find the build log container and ensure it has the `run-log-container` class:
```python
style="height: 400px; overflow-y: auto; padding: 10px; border-radius: 4px;",
class_="run-log-container",
```

**Step 2: Commit**

```bash
git add shiny_app/app.py
git commit -m "fix(ui): ensure build log uses dark log styling"
```

---

### Task 6: Smoke-test the full application

**Step 1: Kill any running instance**

```bash
pkill -f "shiny run.*5001" || true
```

**Step 2: Start the app**

```bash
cd /home/razinka/AQUABCv0.2
source .venv/bin/activate
python -m shiny run --port 5001 shiny_app.app:app &
sleep 5
curl -s -o /dev/null -w "%{http_code}" http://localhost:5001/
```

Expected: `200`

**Step 3: Visual check all panels**

Navigate to each panel in the browser and verify:
- [ ] Header: gradient background, version badge, icon buttons visible
- [ ] Sidebar: dark with glowing active state, collapse works
- [ ] Dashboard: status bar visible, 50/50 layout, run log dark
- [ ] Model Build: cards have glass effect, build log dark
- [ ] Model Config: tabs work, forms styled
- [ ] Input Files: file browser styled, text area readable
- [ ] Parameters: full-width table, category info bar
- [ ] Initial Conditions: forms styled
- [ ] Model Options: toggle switches styled
- [ ] Scenarios: cards styled
- [ ] Plots: tabs work, Plotly charts render
- [ ] Mass Balance: tables styled
- [ ] Observations: upload area styled
- [ ] Map: map widget renders
- [ ] Settings offcanvas: About card only, no theme selector
- [ ] Help offcanvas: content readable
- [ ] Changelog offcanvas: content readable

**Step 4: Check console for errors**

Open browser dev tools, check for:
- No 404s for `aquabc.css`
- No 404s for Google Fonts
- No JavaScript errors from removed theme code

**Step 5: Commit any fixes**

```bash
git add -A
git commit -m "fix(ui): address smoke test issues"
```

---

## Summary of Changes

| File | Action | Description |
|------|--------|-------------|
| `shiny_app/www/aquabc.css` | CREATE | Full Scientific Observatory theme (~650 lines) |
| `shiny_app/app.py` | MODIFY | Remove ~400 lines of inline CSS, ~30 lines of shinyswatch code, restructure Dashboard and Parameters panels, add Google Fonts link |
| `shiny_app/requirements.txt` | MODIFY | Remove `shinyswatch` if listed |

## Estimated net line changes

- `aquabc.css`: +650 lines (new file)
- `app.py`: -450 lines (inline CSS) -30 lines (theme code) +80 lines (new layouts) = ~-400 net
