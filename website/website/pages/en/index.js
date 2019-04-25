/**
 * Copyright (c) 2017-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

const React = require('react');

const CompLibrary = require('../../core/CompLibrary.js');

const MarkdownBlock = CompLibrary.MarkdownBlock; /* Used to read markdown */
const Container = CompLibrary.Container;
const GridBlock = CompLibrary.GridBlock;

class HomeSplash extends React.Component {
  render() {
    const {siteConfig, language = ''} = this.props;
    const {baseUrl, docsUrl} = siteConfig;
    const docsPart = `${docsUrl ? `${docsUrl}/` : ''}`;
    const langPart = `${language ? `${language}/` : ''}`;
    const docUrl = doc => `${baseUrl}${docsPart}${langPart}${doc}`;

    const SplashContainer = props => (
      <div className="homeContainer lightBackground">
        <div className="homeSplashFade">
          <div className="wrapper homeWrapper">{props.children}</div>
        </div>
      </div>
    );

    const Logo = props => (
      <div className="projectLogo">
        <img src={props.img_src} alt="Project Logo" />
      </div>
    );

    const ProjectTitle = () => (
      <h2 className="projectTitle">
        {siteConfig.title}
        <small>{siteConfig.tagline}</small>
      </h2>
    );

    const PromoSection = props => (
      <div className="section promoSection">
        <div className="promoRow">
          <div className="pluginRowBlock">{props.children}</div>
        </div>
      </div>
    );

    const Button = props => (
      <div className="pluginWrapper buttonWrapper">
        <a className="button" href={props.href} target={props.target}>
          {props.children}
        </a>
      </div>
    );

    return (
      <SplashContainer>
        <Logo img_src={`${baseUrl}img/zklambda.svg`} />
        <div className="inner">
          <ProjectTitle siteConfig={siteConfig} />
          <PromoSection>
            <Button href="#quickstart">Quick Start</Button>
          </PromoSection>
        </div>
      </SplashContainer>
    );
  }
}

const example_code = `\`\`\`reasonml
module M = Snarky.Snark.Run.Make(Backends.Mnt4.Default);
open M;

// Compute division by guessing the result and checking with a
// multiplication
let div = (x, y) => {
  // Non-deterministically choose a result
  let z =
    exists(Field.typ, ~compute= () => {
      Field.Constant.Infix.(
        read_var(x) / read_var(y)) // How to actually compute the result
    });

  assert_r1cs(z, y, x); // assert (z * y = x), so the result is correct
  z;
};
\`\`\``

class Index extends React.Component {
  render() {
    const {config: siteConfig, language = ''} = this.props;
    const {baseUrl} = siteConfig;

    const Block = props => (
      <Container
        padding={['bottom', 'top']}
        id={props.id}
        background={props.background}>
        <GridBlock
          align="center"
          contents={props.children}
          layout={props.layout}
        />
      </Container>
    );

    const CodeSample = () => (
      <Container id="codesample" padding={['bottom', 'top']} background="light">
        <div style={{width:'100%', display:'flex', flexDirection:'row'}}>
          <div style={{width:'50%'}}>
            <MarkdownBlock>
              **snarky** lets you write zk-SNARKs as if you were writing ordinary code.
              It's easy to use, efficient, and comes with a bunch of pre-built eunctionality.
            </MarkdownBlock>
            <MarkdownBlock>
              It is modular over the backend SNARK library, and
              comes with backends from [libsnark](https://github.com/scipr-lab/libsnark).
            </MarkdownBlock>
          </div>
          <div style={{width:'50%', overflowX:'scroll'}}>
            <MarkdownBlock>{example_code}</MarkdownBlock>
          </div>
        </div>
      </Container>
    );

    const installCommand = `\`\`\`bash
bash <(curl -sL https://raw.githubusercontent.com/ocaml/opam/master/shell/install.sh)
\`\`\``

      /*
            {`
Grab the <a href="https://github.com/scipr-lab/libsnark#dependencies" target="_blank">libsnark dependencies</a> and <a href="https://opam.ocaml.org/doc/Install.html" target="_blank">opam</a>, then run <span style="white-space: nowrap;">\`opam pin add git@github.com:o1-labs/snarky.git\`</span>
            `}
            */
    const QuickStart = () => (
      <Container id="quickstart" padding={['bottom', 'top']}>
        <div style={{'textAlign': 'center'}}>
          <h2>Quick Start</h2>
        </div>
      <MarkdownBlock>
      You can easily install snarky and all its dependencies with
      </MarkdownBlock>
      <MarkdownBlock>{installCommand}</MarkdownBlock>
      </Container>
    );

    const FeatureCallout = () => /*(
      <div
        className="productShowcaseSection paddingBottom"
        style={{textAlign: 'center'}}>
        <h2>Feature Callout</h2>
        <MarkdownBlock>These are features of this project</MarkdownBlock>
      </div>
    )*/ null;

    const TryOut = () => /*(
      <Block id="try">
        {[
          {
            content: 'Talk about trying this out',
            title: 'Try it Out',
          },
        ]}
      </Block>
    )*/ null;

    const Description = () => /*(
      <Block background="dark">
        {[
          {
            content:
              'This is another description of how this project is useful',
            image: `${baseUrl}img/zklambda.svg`,
            imageAlign: 'right',
            title: 'Description',
          },
        ]}
      </Block>
    )*/ null;

    const LearnHow = () => /*(
      <Block background="light">
        {[
          {
            content: 'Talk about learning how to use this',
            image: `${baseUrl}img/zklambda.svg`,
            imageAlign: 'right',
            title: 'Learn How',
          },
        ]}
      </Block>
    )*/ null;

    const ExampleCode = () => (
      <Block layout="fourColumn">
        {[
          {
            content: 'Check out out [this page](docs/try-it-out) to get started with a simple snarky project.',
            /*image: `${baseUrl}img/zklambda.svg`,
            imageAlign: 'top',*/
            title: 'Try it out',
          },
          {
            content: 'Operations on an [elliptic curve](https://github.com/o1-labs/snarky/blob/master/examples/elliptic_curve_operations/elliptic_curve_operations.re)',
            /*image: `${baseUrl}img/zklambda.svg`,
            imageAlign: 'top',*/
            title: 'Examples',
          },
        ]}
      </Block>
    );

    const Showcase = () => {
      if ((siteConfig.users || []).length === 0) {
        return null;
      }

      const showcase = siteConfig.users
        .filter(user => user.pinned)
        .map(user => (
          <a href={user.infoLink} key={user.infoLink}>
            <img src={user.image} alt={user.caption} title={user.caption} />
          </a>
        ));

      const pageUrl = page => baseUrl + (language ? `${language}/` : '') + page;

      return (
        <div className="productShowcaseSection paddingBottom">
          <h2>Who is using Snarky?</h2>
          <p>This project is used by</p>
          <div className="logos">{showcase}</div>
          <div className="more-users">
            <a className="button" href={pageUrl('users.html')}>
              More {siteConfig.title} Users
            </a>
          </div>
        </div>
      );
    };

    return (
      <div>
        <HomeSplash siteConfig={siteConfig} language={language} />
        <CodeSample />
        <div className="mainContainer">
          <QuickStart />
          <ExampleCode />
          <FeatureCallout />
          <LearnHow />
          <TryOut />
          <Description />
          <Showcase />
        </div>
      </div>
    );
  }
}

module.exports = Index;
