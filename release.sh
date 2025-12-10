#!/usr/bin/env bash
#
# Release script for erlang-sensehat
# Creates a git tag and pushes it to trigger the release workflow
#

set -e

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Script directory
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$SCRIPT_DIR"

# Functions
print_error() {
    echo -e "${RED}ERROR: $1${NC}" >&2
}

print_success() {
    echo -e "${GREEN}✓ $1${NC}"
}

print_info() {
    echo -e "${BLUE}→ $1${NC}"
}

print_warning() {
    echo -e "${YELLOW}⚠ $1${NC}"
}

usage() {
    cat << EOF
Usage: $0 <version> [options]

Create a new release by tagging and pushing to GitHub.

Arguments:
  version       Version number (e.g., 1.0.0 or v1.0.0)

Options:
  -m MESSAGE    Custom release message (optional)
  -f, --force   Force tag creation (overwrite existing tag)
  -n, --dry-run Show what would be done without doing it
  -h, --help    Show this help message

Examples:
  $0 1.0.0
  $0 v1.2.3 -m "Bug fixes and improvements"
  $0 2.0.0 --dry-run

Note: This script will:
  1. Check if working directory is clean
  2. Run local tests (compile, eunit, xref, dialyzer)
  3. Create a git tag
  4. Push the tag to GitHub
  5. GitHub Actions will build and create the release
EOF
    exit 1
}

check_git_clean() {
    if [[ -n $(git status -s) ]]; then
        print_error "Working directory is not clean. Commit or stash changes first."
        git status -s
        exit 1
    fi
    print_success "Working directory is clean"
}

check_git_branch() {
    BRANCH=$(git branch --show-current)
    if [[ "$BRANCH" != "master" ]] && [[ "$BRANCH" != "main" ]]; then
        print_warning "Current branch is '$BRANCH', not 'master' or 'main'"
        read -p "Continue anyway? (y/N) " -n 1 -r
        echo
        if [[ ! $REPLY =~ ^[Yy]$ ]]; then
            exit 1
        fi
    fi
}

check_version_format() {
    local version=$1
    if [[ ! "$version" =~ ^v?[0-9]+\.[0-9]+\.[0-9]+(-[a-zA-Z0-9]+)?$ ]]; then
        print_error "Invalid version format: $version"
        echo "Version should be in format: X.Y.Z or vX.Y.Z (e.g., 1.0.0 or v1.0.0)"
        exit 1
    fi
}

check_tag_exists() {
    local tag=$1
    if git rev-parse "$tag" >/dev/null 2>&1; then
        if [[ "$FORCE" != "true" ]]; then
            print_error "Tag $tag already exists. Use -f to force."
            exit 1
        else
            print_warning "Tag $tag already exists and will be overwritten"
        fi
    fi
}

run_tests() {
    print_info "Running local tests..."
    
    if ! rebar3 do clean, compile, eunit, xref, dialyzer; then
        print_error "Tests failed. Fix issues before releasing."
        exit 1
    fi
    
    print_success "All tests passed"
}

create_tag() {
    local tag=$1
    local message=$2
    
    if [[ "$DRY_RUN" == "true" ]]; then
        print_info "[DRY RUN] Would create tag: $tag"
        print_info "[DRY RUN] Message: $message"
        return
    fi
    
    if [[ "$FORCE" == "true" ]] && git rev-parse "$tag" >/dev/null 2>&1; then
        git tag -d "$tag"
        print_info "Deleted existing local tag $tag"
    fi
    
    git tag -a "$tag" -m "$message"
    print_success "Created tag $tag"
}

push_tag() {
    local tag=$1
    
    if [[ "$DRY_RUN" == "true" ]]; then
        print_info "[DRY RUN] Would push tag: $tag"
        return
    fi
    
    if [[ "$FORCE" == "true" ]]; then
        git push origin "$tag" --force
    else
        git push origin "$tag"
    fi
    
    print_success "Pushed tag $tag to origin"
}

# Parse arguments
VERSION=""
MESSAGE=""
FORCE=false
DRY_RUN=false

while [[ $# -gt 0 ]]; do
    case $1 in
        -m)
            MESSAGE="$2"
            shift 2
            ;;
        -f|--force)
            FORCE=true
            shift
            ;;
        -n|--dry-run)
            DRY_RUN=true
            shift
            ;;
        -h|--help)
            usage
            ;;
        -*)
            print_error "Unknown option: $1"
            usage
            ;;
        *)
            if [[ -z "$VERSION" ]]; then
                VERSION="$1"
            else
                print_error "Unexpected argument: $1"
                usage
            fi
            shift
            ;;
    esac
done

# Validate version argument
if [[ -z "$VERSION" ]]; then
    print_error "Version number is required"
    usage
fi

# Normalize version (add 'v' prefix if missing)
if [[ ! "$VERSION" =~ ^v ]]; then
    VERSION="v$VERSION"
fi

# Validate version format
check_version_format "$VERSION"

# Set default message if not provided
if [[ -z "$MESSAGE" ]]; then
    MESSAGE="Release $VERSION"
fi

# Main execution
echo
print_info "Preparing release $VERSION"
echo

# Pre-flight checks
if [[ "$DRY_RUN" != "true" ]]; then
    check_git_clean
    check_git_branch
    check_tag_exists "$VERSION"
    
    # Pull latest changes
    print_info "Pulling latest changes..."
    git pull --rebase
    print_success "Up to date with remote"
    
    # Run tests
    run_tests
fi

# Create and push tag
create_tag "$VERSION" "$MESSAGE"
push_tag "$VERSION"

# Done
echo
if [[ "$DRY_RUN" == "true" ]]; then
    print_info "Dry run complete. No changes were made."
else
    print_success "Release process initiated!"
    print_info "GitHub Actions will now build and create the release."
    print_info "Monitor progress at: https://github.com/mteinum/erlang-sensehat/actions"
fi
echo
