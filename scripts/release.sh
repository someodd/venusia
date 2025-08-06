#!/bin/bash
set -e

# Simple release script for Venusia
# Usage: ./scripts/release.sh <version>

if [ $# -eq 0 ]; then
    echo "Usage: $0 <version>"
    echo "Example: $0 0.2.0"
    exit 1
fi

VERSION=$1
TAG="v$VERSION"

echo "Preparing release $TAG..."

# Update version in package.yaml (has many spaces after colon)
sed -i "s/^version:.*$/version:             $VERSION/" package.yaml

# Update version in .cabal file (has fewer spaces after colon)
sed -i "s/^version:.*$/version:        $VERSION/" Venusia.cabal

# Update CHANGELOG.md
DATE=$(date +%Y-%m-%d)
sed -i "s/## Unreleased/## Unreleased\n\n## $VERSION - $DATE/" CHANGELOG.md

echo "Updated package.yaml, Venusia.cabal, and CHANGELOG.md"
echo "Please review the changes, then run:"
echo "  git add package.yaml Venusia.cabal CHANGELOG.md"
echo "  git commit -m 'Release $TAG'"
echo "  git tag $TAG"
echo "  git push origin main --tags"
echo ""
echo "The GitHub workflow will automatically build and release the Debian package."