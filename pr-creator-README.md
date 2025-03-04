# PR Creator with lite-llm Integration

This script automates GitHub Pull Request creation with AI-generated descriptions using lite-llm.

## Features

- Automatically generates PR descriptions based on git diff and commit messages
- Optionally auto-generates PR titles
- Customizable prompts for specific PR description styles
- Integration with lite-llm for AI-powered content generation
- Uses GitHub CLI for PR creation
- Debug mode for troubleshooting API issues

## Prerequisites

Before using this script, ensure you have the following installed:

1. **GitHub CLI (gh)**: For creating PRs from the command line
   - Install from: https://cli.github.com/
   - Authenticate with: `gh auth login`

2. **jq**: For parsing JSON responses
   - Install with Homebrew: `brew install jq`
   - Or apt: `sudo apt-get install jq`

3. **curl**: For making API requests (usually pre-installed on most systems)

4. **lite-llm**: Running locally or accessible via an endpoint
   - See: https://github.com/BerriAI/litellm
   - You'll need an API key for authentication

## Setup

1. Make the script executable:
   ```bash
   chmod +x scripts/pr-creator.sh
   ```

2. Configure your lite-llm endpoint:
   - By default, the script uses `https://litellm.develop.m1finance.dev/`
   - You can override this with the `-e` flag or by setting the `LITE_LLM_ENDPOINT` environment variable

3. Set up your API key:
   - You can provide it with the `-k` flag
   - Or set the `LITE_LLM_API_KEY` environment variable
   - For security, prefer using environment variables over command-line arguments

## Usage

Basic usage:

```bash
./scripts/pr-creator.sh --title "Your PR Title" --branch main --api-key "your-api-key"
```

With auto-generated title:

```bash
./scripts/pr-creator.sh --auto-title --branch main --api-key "your-api-key"
```

Using environment variables for sensitive information:

```bash
export LITE_LLM_API_KEY="your-api-key"
./scripts/pr-creator.sh --title "Your PR Title"
```

Debugging API issues:

```bash
./scripts/pr-creator.sh --title "Your PR Title" --debug
```

### Options

- `-t, --title TITLE`: Set PR title (required if not using --auto-title)
- `-b, --branch BRANCH`: Target branch (default: main)
- `-e, --endpoint URL`: lite-llm endpoint URL
- `-m, --model MODEL`: lite-llm model to use (default: claude-3.5-sonnet)
- `-k, --api-key KEY`: API key for lite-llm authentication
- `-p, --prompt PROMPT`: Custom prompt to prepend to the diff
- `-a, --auto-title`: Auto-generate PR title as well
- `-d, --debug`: Enable debug mode for troubleshooting API issues
- `-h, --help`: Display help message

### Environment Variables

You can also configure the script using environment variables:

- `LITE_LLM_ENDPOINT`: The endpoint URL for lite-llm
- `LITE_LLM_MODEL`: The model to use for generation
- `LITE_LLM_API_KEY`: Your API key for lite-llm
- `TARGET_BRANCH`: The default target branch

## Examples

1. Basic usage with custom title:
   ```bash
   ./scripts/pr-creator.sh --title "Add user authentication feature" --api-key "your-api-key"
   ```

2. Auto-generate both description and title:
   ```bash
   ./scripts/pr-creator.sh --auto-title --api-key "your-api-key"
   ```

3. Use a custom lite-llm endpoint and model:
   ```bash
   ./scripts/pr-creator.sh --title "Fix bugs" --endpoint "https://your-litellm-server/v1/chat/completions" --model "gpt-4" --api-key "your-api-key"
   ```

4. Use a custom prompt:
   ```bash
   ./scripts/pr-creator.sh --title "Refactor code" --prompt "Create a PR description focusing on code quality improvements and technical debt reduction." --api-key "your-api-key"
   ```

5. Debug API issues:
   ```bash
   ./scripts/pr-creator.sh --title "Your PR Title" --api-key "your-api-key" --debug
   ```

## Debugging

If you're experiencing issues with the lite-llm API call, use the debug mode:

```bash
./scripts/pr-creator.sh --title "Your PR Title" --debug
```

Debug mode will:
- Show detailed information about the API request
- Display the full curl verbose output
- Provide more specific error messages
- Show troubleshooting suggestions

Common issues and solutions:

1. **Endpoint URL format**: Make sure your endpoint URL includes the full path (`/v1/chat/completions`)
2. **API key format**: The API key should be passed without the "Bearer" prefix
3. **Network connectivity**: Ensure your network can reach the lite-llm server
4. **Request payload**: Check if the model name is supported by your lite-llm instance
5. **Response format**: Verify that the lite-llm server returns responses in the expected format

## Setting Up lite-llm Locally

To use this script with a local lite-llm instance:

1. Install lite-llm:
   ```bash
   pip install litellm
   ```

2. Start the lite-llm server:
   ```bash
   litellm --model gpt-3.5-turbo --api_base https://api.openai.com --api_key your_openai_key
   ```

3. Use the script with the local endpoint:
   ```bash
   ./scripts/pr-creator.sh --title "Your PR Title" --api-key "your-api-key"
   ```

## Security Considerations

- Avoid passing API keys as command-line arguments when possible, as they may be visible in shell history
- Use environment variables for sensitive information
- Consider using a credentials file with restricted permissions for storing API keys

## Troubleshooting

- **Error: Failed to generate PR description**: Check that your lite-llm endpoint is correctly configured and accessible
- **Authentication errors**: Verify your API key is correct and properly formatted
- **No changes detected**: Ensure your branch has changes compared to the target branch
- **GitHub authentication issues**: Run `gh auth login` to authenticate with GitHub
- **Timeout issues**: For large diffs, the API might time out; try with a smaller diff or increase the timeout 