#!/usr/bin/env bash
# ──────────────────────────────────────────────────────────────
# AFA 2027 AI Workflow Logger
# Records ONLY the user's prompts to a clean, human-readable log.
# Called by Claude Code's UserPromptSubmit hook.
# ──────────────────────────────────────────────────────────────
set -euo pipefail

LOG_FILE="/workspace/session_log.txt"

# Read JSON payload from stdin
INPUT=$(cat)

EVENT=$(echo "$INPUT" | jq -r '.hook_event_name // "unknown"')

# Only record user prompts; ignore every other event.
if [ "$EVENT" = "UserPromptSubmit" ]; then
  TIMESTAMP=$(date -u '+%Y-%m-%d %H:%M:%SZ')
  PROMPT=$(echo "$INPUT" | jq -r '.prompt // "(empty)"')
  {
    echo "[$TIMESTAMP]"
    echo "$PROMPT"
    echo ""
  } >> "$LOG_FILE"
fi

# Always exit 0 so we never block Claude's operation.
exit 0
