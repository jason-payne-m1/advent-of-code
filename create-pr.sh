#!/bin/bash

# create-pr.sh - Simple wrapper for pr-creator.sh with common configurations
#
# This script provides a simplified interface to pr-creator.sh with
# pre-configured settings for the team's common use cases.

set -e

# Load environment variables from .env file
if [ -f .env ]; then
  export $(cat .env | grep -v '#' | awk '/=/ {print $1}')
fi

# Default configuration - Edit these to match your team's setup
DEFAULT_LITE_LLM_ENDPOINT=""
DEFAULT_LITE_LLM_MODEL=""
DEFAULT_LITE_LLM_API_KEY=""
DEFAULT_TARGET_BRANCH="main"

# Custom prompt for Scala backend team
SCALA_PROMPT="You are a Senior Scala Backend Engineer. Create a detailed PR description for a financial services backend application. Focus on:
1. Functional programming patterns used
2. Business logic changes
3. Database schema changes (if any)
4. API contract changes (if any)
5. Security considerations (especially OWASP)
6. Testing approach

Format the description with markdown and organize it into clear sections."

# Export environment variables for pr-creator.sh
export LITE_LLM_ENDPOINT=${LITE_LLM_ENDPOINT:-$DEFAULT_LITE_LLM_ENDPOINT}
export LITE_LLM_MODEL=${LITE_LLM_MODEL:-$DEFAULT_LITE_LLM_MODEL}
export LITE_LLM_API_KEY=${LITE_LLM_API_KEY:-$DEFAULT_LITE_LLM_API_KEY}
export TARGET_BRANCH=${TARGET_BRANCH:-$DEFAULT_TARGET_BRANCH}

# Function to display usage information
usage() {
  echo "Usage: $0 [options]"
  echo ""
  echo "A simplified wrapper for pr-creator.sh with team-specific defaults."
  echo ""
  echo "Options:"
  echo "  -t, --title TITLE       Set PR title"
  echo "  -b, --branch BRANCH     Target branch (default: main)"
  echo "  -a, --auto-title        Auto-generate PR title"
  echo "  -s, --scala             Use Scala-specific prompt template"
  echo "  -k, --api-key KEY       API key for lite-llm"
  echo "  -d, --debug             Enable debug mode for troubleshooting"
  echo "  -h, --help              Display this help message"
  echo ""
  echo "For more advanced options, use scripts/pr-creator.sh directly."
  exit 1
}

# Parse command line arguments
ARGS=""
USE_SCALA_PROMPT=false
API_KEY_ARG=""
DEBUG_MODE=false

while [[ $# -gt 0 ]]; do
  case $1 in
    -t|--title)
      ARGS="$ARGS --title \"$2\""
      shift 2
      ;;
    -b|--branch)
      TARGET_BRANCH="$2"
      shift 2
      ;;
    -a|--auto-title)
      ARGS="$ARGS --auto-title"
      shift
      ;;
    -s|--scala)
      USE_SCALA_PROMPT=true
      shift
      ;;
    -k|--api-key)
      API_KEY_ARG="--api-key \"$2\""
      shift 2
      ;;
    -d|--debug)
      DEBUG_MODE=true
      shift
      ;;
    -h|--help)
      usage
      ;;
    *)
      echo "Unknown option: $1"
      usage
      ;;
  esac
done

# Add Scala prompt if requested
if [ "$USE_SCALA_PROMPT" = true ]; then
  ARGS="$ARGS --prompt \"$SCALA_PROMPT\""
fi

# Add API key if provided
if [ -n "$API_KEY_ARG" ]; then
  ARGS="$ARGS $API_KEY_ARG"
fi

# Add debug flag if enabled
if [ "$DEBUG_MODE" = true ]; then
  ARGS="$ARGS --debug"
fi

# Check if pr-creator.sh exists
if [ ! -f "$(dirname "$0")/pr-creator.sh" ]; then
  echo "Error: pr-creator.sh not found in the same directory as this script."
  exit 1
fi

# Execute the pr-creator.sh script with our arguments
eval "$(dirname "$0")/pr-creator.sh $ARGS" 