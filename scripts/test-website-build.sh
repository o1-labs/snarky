#!/bin/bash

set -e

cd website/website
npm install docusaurus
npm run-script build
