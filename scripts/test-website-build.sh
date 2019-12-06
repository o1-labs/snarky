#!/bin/bash

set -e

cd website/website
# Restore packages to the state in package-lock.json
npm ci
npm run-script build
