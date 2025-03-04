#!/bin/bash

# pr-creator.sh - Automate GitHub PR creation with lite-llm generated descriptions
# 
# This script:
# 1. Gets the diff between the current branch and the target branch
# 2. Sends the diff to lite-llm to generate a PR description
# 3. Creates a GitHub PR with the generated description
#
# Requirements:
# - GitHub CLI (gh) installed: https://cli.github.com/
# - curl for API requests
# - jq for JSON parsing
# - lite-llm endpoint configured

set -e

# Load environment variables from .env file
if [ -f .env ]; then
  export $(cat .env | grep -v '#' | awk '/=/ {print $1}')
fi

# Enable debug mode with -d or --debug flag
DEBUG=false

# Configuration
LITE_LLM_ENDPOINT=${LITE_LLM_ENDPOINT:-""}
LITE_LLM_MODEL=${LITE_LLM_MODEL:-""}
LITE_LLM_API_KEY=${LITE_LLM_API_KEY:-""}
TARGET_BRANCH=${TARGET_BRANCH:-""}
PR_TITLE=${PR_TITLE:-""}
CUSTOM_PROMPT=${CUSTOM_PROMPT:-""}

# Debug function
debug() {
  if [ "$DEBUG" = true ]; then
    echo "[DEBUG] $1"
  fi
}

# Function to display usage information
usage() {
  echo "Usage: $0 [options]"
  echo ""
  echo "Options:"
  echo "  -t, --title TITLE       Set PR title (required if not using --auto-title)"
  echo "  -b, --branch BRANCH     Target branch (default: main)"
  echo "  -e, --endpoint URL      lite-llm endpoint URL"
  echo "  -m, --model MODEL       lite-llm model to use"
  echo "  -k, --api-key KEY       API key for lite-llm"
  echo "  -p, --prompt PROMPT     Custom prompt to prepend to the diff"
  echo "  -a, --auto-title        Auto-generate PR title as well"
  echo "  -d, --debug             Enable debug mode for troubleshooting"
  echo "  -h, --help              Display this help message"
  exit 1
}

# Parse command line arguments
while [[ $# -gt 0 ]]; do
  case $1 in
    -t|--title)
      PR_TITLE="$2"
      shift 2
      ;;
    -b|--branch)
      TARGET_BRANCH="$2"
      shift 2
      ;;
    -e|--endpoint)
      LITE_LLM_ENDPOINT="$2"
      shift 2
      ;;
    -m|--model)
      LITE_LLM_MODEL="$2"
      shift 2
      ;;
    -k|--api-key)
      LITE_LLM_API_KEY="$2"
      shift 2
      ;;
    -p|--prompt)
      CUSTOM_PROMPT="$2"
      shift 2
      ;;
    -a|--auto-title)
      AUTO_TITLE=true
      shift
      ;;
    -d|--debug)
      DEBUG=true
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

debug "Debug mode enabled"
debug "LITE_LLM_ENDPOINT: $LITE_LLM_ENDPOINT"
debug "LITE_LLM_MODEL: $LITE_LLM_MODEL"
debug "API_KEY present: $([ -n "$LITE_LLM_API_KEY" ] && echo 'Yes' || echo 'No')"
debug "TARGET_BRANCH: $TARGET_BRANCH"

# Check if gh CLI is installed
if ! command -v gh &> /dev/null; then
  echo "Error: GitHub CLI (gh) is not installed. Please install it from https://cli.github.com/"
  exit 1
fi

# Check if jq is installed
if ! command -v jq &> /dev/null; then
  echo "Error: jq is not installed. Please install it using your package manager."
  exit 1
fi

# Check if user is authenticated with GitHub
if ! gh auth status &> /dev/null; then
  echo "Error: You are not authenticated with GitHub. Please run 'gh auth login' first."
  exit 1
fi

# Get current branch name
CURRENT_BRANCH=$(git symbolic-ref --short HEAD)
echo "Current branch: $CURRENT_BRANCH"
echo "Target branch: $TARGET_BRANCH"

# Get the diff
echo "Getting diff between $CURRENT_BRANCH and $TARGET_BRANCH..."
DIFF=$(git diff "$TARGET_BRANCH"..."$CURRENT_BRANCH")

if [ -z "$DIFF" ]; then
  echo "No changes detected between $CURRENT_BRANCH and $TARGET_BRANCH"
  exit 1
fi

# Get commit messages
echo "Getting commit messages..."
COMMIT_MESSAGES=$(git log "$TARGET_BRANCH".."$CURRENT_BRANCH" --pretty=format:"%s%n%b" | sed '/^$/d')

# Prepare the prompt for lite-llm
if [ -z "$CUSTOM_PROMPT" ]; then
  PROMPT="You are a helpful assistant that creates concise and informative GitHub pull request descriptions. Based on the following git diff and commit messages, create a well-structured PR description that:
1. Summarizes the changes
2. Lists the key modifications
3. Mentions any important implementation details
4. Includes any testing considerations

Format the description with markdown.

Commit Messages:
$COMMIT_MESSAGES

Git Diff:
$DIFF"
else
  PROMPT="$CUSTOM_PROMPT

Commit Messages:
$COMMIT_MESSAGES

Git Diff:
$DIFF"
fi

# Escape the prompt to be a valid JSON string
ESCAPED_PROMPT=$(jq -Rs . <<< "$PROMPT")

# Truncate the prompt if it's too large (API limits)
MAX_TOKENS=8000
PROMPT_LENGTH=${#ESCAPED_PROMPT}
if [ "$PROMPT_LENGTH" -gt "$MAX_TOKENS" ]; then
  echo "Warning: Diff is very large ($PROMPT_LENGTH chars), truncating to $MAX_TOKENS chars..."
  ESCAPED_PROMPT=$(echo "$ESCAPED_PROMPT" | cut -c1-$MAX_TOKENS)
  ESCAPED_PROMPT+="\""
fi

# Prepare headers for API request
HEADERS=(-H "Content-Type: application/json")
if [ -n "$LITE_LLM_API_KEY" ]; then
  HEADERS+=(-H "Authorization: Bearer $LITE_LLM_API_KEY")
  debug "Added Authorization header"
else
  echo "Warning: No API key provided. API call may fail if authentication is required."
fi

# Prepare the request payload
REQUEST_PAYLOAD="{
  \"model\": \"Bedrock Claude 3.7 Sonnet\",
  \"prompt\": $ESCAPED_PROMPT,
  \"temperature\": 0.7
}"


debug "Request endpoint: $LITE_LLM_ENDPOINT"
debug "Request headers: ${HEADERS[*]}"
debug "Request payload structure: $(echo "$REQUEST_PAYLOAD")"

# Call lite-llm API to generate PR description with verbose output in debug mode
echo "Generating PR description using lite-llm..."
if [ "$DEBUG" = true ]; then
  echo "Making API call with verbose output..."
  RESPONSE=$(curl -s -X POST "$LITE_LLM_ENDPOINT" \
    "${HEADERS[@]}" \
    -d "$REQUEST_PAYLOAD" 2>&1)

  # Extract response body from verbose output
  RESPONSE_BODY=$(echo "$RESPONSE" | sed -n '/^{/,/^}/p' | tr -d '\r')

  # Print the full verbose output for debugging
  echo "Full curl verbose output:"
  echo "$RESPONSE"

  echo "Response body (if available):"
  echo "$RESPONSE_BODY"

  # Try to parse the response body
  if [[ "$RESPONSE_BODY" == {* ]]; then
    PR_DESCRIPTION=$(echo "$RESPONSE_BODY" | jq -r '.choices[0].message.content' 2>/dev/null)
  else
    echo "Error: Could not extract valid JSON response body"
    PR_DESCRIPTION=""
  fi
else
  # Normal non-verbose API call
  RESPONSE=$(curl -s -X POST "$LITE_LLM_ENDPOINT" \
    "${HEADERS[@]}" \
    -d "$REQUEST_PAYLOAD")

  # Extract the generated description from the response
  PR_DESCRIPTION=$(echo "$RESPONSE" | jq -r '.choices[0].text' 2>/dev/null)
fi

# Check if we got a valid response
if [ -z "$PR_DESCRIPTION" ] || [ "$PR_DESCRIPTION" == "null" ]; then
  echo "Error: Failed to generate PR description. API response:"
  echo "$RESPONSE"

  # Additional debugging for common issues
  if [[ "$RESPONSE" == *"unauthorized"* ]] || [[ "$RESPONSE" == *"Unauthorized"* ]]; then
    echo "Authentication error: Check your API key"
  elif [[ "$RESPONSE" == *"not found"* ]] || [[ "$RESPONSE" == *"Not Found"* ]]; then
    echo "Endpoint error: The API endpoint URL may be incorrect"
  elif [[ "$RESPONSE" == *"timeout"* ]]; then
    echo "Timeout error: The API request timed out"
  fi

  # Offer troubleshooting steps
  echo ""
  echo "Troubleshooting steps:"
  echo "1. Check that your lite-llm endpoint is correct: $LITE_LLM_ENDPOINT"
  echo "2. Verify your API key is valid"
  echo "3. Try running with --debug flag for more detailed information"
  echo "4. Check if the lite-llm service is running and accessible"
  echo "5. Try a smaller diff to rule out content length issues"

  exit 1
fi

echo "Successfully generated PR description!"

# Auto-generate title if requested
if [ "$AUTO_TITLE" = true ]; then
  echo "Generating PR title using lite-llm..."
  TITLE_PROMPT="You are a helpful assistant that creates concise PR titles. Based on the following PR description, generate a concise title (maximum 50 characters):

$PR_DESCRIPTION"

TITLE_REQUEST_PAYLOAD="{
  \"model\": \"Bedrock Claude 3.7 Sonnet\",
  \"prompt\": $TITLE_PROMPT,
  \"temperature\": 0.7
}"

  debug "Making title API call..."
  debug "Request payload structure: $(echo "$TITLE_REQUEST_PAYLOAD")"
  TITLE_RESPONSE=$(curl -s -X POST "$LITE_LLM_ENDPOINT" \
    "${HEADERS[@]}" \
    -d "$REQUEST_PAYLOAD" 2>&1)
  
  PR_TITLE=$(echo "$TITLE_RESPONSE" | jq -r '.choices[0].text' 2>/dev/null)
  
  # Remove quotes if present and truncate if needed
  PR_TITLE=$(echo "$PR_TITLE" | sed 's/^"//;s/"$//')
  if [ ${#PR_TITLE} -gt 50 ]; then
    PR_TITLE="${PR_TITLE:0:47}..."
  fi
fi

# Check if PR title is provided
if [ -z "$PR_TITLE" ]; then
  echo "Error: PR title is required. Please provide it with the -t/--title option or use --auto-title."
  exit 1
fi

# Create the PR
echo "Creating PR with title: $PR_TITLE"
echo "Description preview (first 10 lines):"
echo "$PR_DESCRIPTION" | head -n 10
echo "..."

# Confirm with user
read -p "Create PR? [y/N] " -n 1 -r
echo
if [[ ! $REPLY =~ ^[Yy]$ ]]; then
  echo "PR creation aborted."
  exit 1
fi

# Create the PR using GitHub CLI
gh pr create \
  --title "$PR_TITLE" \
  --body "$PR_DESCRIPTION" \
  --base "$TARGET_BRANCH"

echo "PR created successfully!" 