/**
 * Copyright (c) 2017-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

// See https://docusaurus.io/docs/site-config for all the possible
// site configuration options.

// List of projects/orgs using your project for the users page.
const users = [
  {
    caption: 'Coda',
    // You will need to prepend the image path with your baseUrl
    // if it is not '/', like: '/test-site/img/docusaurus.svg'.
    image: '/snarky/img/coda.svg',
    infoLink: 'https://codaprotocol.com',
    pinned: true,
  },
];

const siteConfig = {
  title: 'Snarky', // Title for your website.
  tagline: 'OCaml DSL for verifiable computation',
  url: 'https://o1-labs.github.io', // Your website URL
  baseUrl: '/snarky/', // Base URL for your project */
  // For github.io type URLs, you would set the url and baseUrl like:
  //   url: 'https://facebook.github.io',
  //   baseUrl: '/test-site/',

  // Used for publishing and more
  projectName: 'snarky',
  organizationName: 'o1-labs',
  // For top-level user or org sites, the organization is still the same.
  // e.g., for the https://JoelMarcey.github.io site, it would be set like...
  //   organizationName: 'JoelMarcey'

  // For no header links in the top nav bar -> headerLinks: [],
  headerLinks: [
    {doc: 'overview', label: 'Docs'},
    {page: 'api/snarky/Snarky/index', label: 'API'},
    {page: 'help', label: 'Help'},
    //{blog: true, label: 'Blog'},
  ],

  // If you have users set above, you add it here:
  users,

  /* path to images for header/footer */
  headerIcon: 'img/zklambda.svg',
  footerIcon: 'img/zklambda.svg',
  favicon: 'img/favicon.png',

  /* Colors for website */
  colors: {
    primaryColor: '#662d91',
    secondaryColor: '#58027F',
  },

  /* Custom fonts for website */
  /*
  fonts: {
    myFont: [
      "Times New Roman",
      "Serif"
    ],
    myOtherFont: [
      "-apple-system",
      "system-ui"
    ]
  },
  */

  // This copyright info is used in /core/Footer.js and blog RSS/Atom feeds.
  copyright: `Copyright © ${new Date().getFullYear()} O(1) Labs`,

  highlight: {
    // Highlight.js theme to use for syntax highlighting in code blocks.
    // See https://github.com/highlightjs/highlight.js/tree/master/src/styles
    theme: 'atom-one-light',
  },

  // Add custom scripts here that would be placed in <script> tags.
  scripts: ['https://buttons.github.io/buttons.js'],

  // On page navigation for the current documentation page.
  onPageNav: 'separate',
  // No .html extensions for paths.
  cleanUrl: true,

  // Open Graph and Twitter card images.
  ogImage: 'img/zklambda.png',
  twitterImage: 'img/zklambda.png',

  separateCss: ['static/css/odoc.css'],

  // Show documentation's last contributor's name.
  // enableUpdateBy: true,

  // Show documentation's last update time.
  // enableUpdateTime: true,

  repoUrl: 'https://github.com/o1-labs/snarky',
};

module.exports = siteConfig;
