#!/bin/bash
if [ -z "$GIT_USER" ]; then
  echo "You must set the GIT_USER variable to publish to GitHub"
  exit 1
fi
make website
cd website/website
npm install docusaurus
USE_SSH=true npm run publish-gh-pages
