mod running_server;

use std::{
    collections::{HashSet, VecDeque},
    path::PathBuf,
    process::Command,
};

use anyhow::{Context, Result};
use html5ever::{
    tendril::StrTendril,
    tokenizer::{BufferQueue, Token, TokenSink, TokenSinkResult, Tokenizer, TokenizerOpts},
};
use reqwest::header::CONTENT_TYPE;
use running_server::RunningServer;

struct App {
    /// Paths already processed or in the queue
    processed: HashSet<String>,
    /// Paths waiting to be processed
    queue: VecDeque<String>,
    /// Root output directory
    root: PathBuf,
    /// The running server process
    running_server: RunningServer,
    /// HTTP client
    client: reqwest::blocking::Client,
}

impl App {
    fn add(&mut self, route: &str) {
        // Only take URLs relative to the current domain, and ignore some specific redirecting URLs
        if route.starts_with("/wiki") {
            return;
        }
        if let Some(route) = route.strip_prefix("/") {
            if !self.processed.contains(route) {
                self.processed.insert(route.to_owned());
                self.queue.push_back(route.to_owned());
            }
        }
    }
}

fn main() -> Result<()> {
    let mut root = PathBuf::new();
    root.push("..");
    root.push("public");

    anyhow::ensure!(
        !root.exists(),
        "{} exists, please remove before running",
        root.display()
    );

    fs_err::create_dir_all("../public")?;
    fs_err::copy("assets/_redirects", "../public/_redirects")?;

    // Really lazy...
    let status = Command::new("cp")
        .args(["-r", "../static", "../public/static"])
        .status()?;
    anyhow::ensure!(status.success(), "Failed copying static dir");
    let status = Command::new("cp")
        .args(["-r", "../content/static", "../public/assets"])
        .status()?;

    let client = reqwest::blocking::Client::new();
    let running_server = RunningServer::new(&client)?;
    let mut app = App {
        processed: HashSet::new(),
        queue: VecDeque::new(),
        root,
        running_server,
        client,
    };
    app.add("/");
    app.add("/favicon.ico");
    app.add("/robots.txt");
    app.add("/feed");
    app.add("/404");

    while let Some(route) = app.queue.pop_front() {
        process(&mut app, route)?;
    }
    anyhow::ensure!(status.success(), "Failed copying assets dir");

    Ok(())
}

fn process(app: &mut App, route: String) -> Result<()> {
    anyhow::ensure!(!route.contains(".."), "No double dots allowed!");
    anyhow::ensure!(!route.starts_with("/"), "No leading slashes: {route}");
    let url = format!("{}/{route}", app.running_server.host_root());
    let res = app
        .client
        .get(&url)
        .send()
        .with_context(|| format!("Unable to request URL: {url:?}"))?;
    let content_type = res
        .headers()
        .get(CONTENT_TYPE)
        .with_context(|| format!("No content-type found for {url}"))?
        .as_bytes();
    let content_type = std::str::from_utf8(content_type)
        .with_context(|| format!("Non-UTF8 content type for {url}"))?
        .to_owned();
    let is_html = content_type.starts_with("text/html");
    let body = res
        .bytes()
        .with_context(|| format!("Error reading body for {url}"))?;

    let mut dest_path = app.root.clone();
    if route.is_empty() {
        dest_path.push("index.html");
    } else if route == "feed" {
        dest_path.push("feed.xml");
    } else if is_html {
        dest_path.push(format!("{route}.html"));
    } else {
        dest_path.push(route);
    }
    if let Some(parent) = dest_path.parent() {
        fs_err::create_dir_all(parent)?;
    }
    fs_err::write(&dest_path, &body)?;

    if is_html {
        let body =
            std::str::from_utf8(&body).with_context(|| format!("Non-UTF8 content in {url}"))?;
        handle_html(app, &body)?;
    }

    Ok(())
}

fn handle_html(app: &mut App, body: &str) -> Result<()> {
    let mut tokenizer = Tokenizer::new(AppTokenSink(app), TokenizerOpts::default());
    let mut queue = BufferQueue::new();
    queue.push_back(StrTendril::from_slice(body));
    match tokenizer.feed(&mut queue) {
        html5ever::tokenizer::TokenizerResult::Done => (),
        html5ever::tokenizer::TokenizerResult::Script(_) => todo!(),
    }
    tokenizer.end();
    Ok(())
}

struct AppTokenSink<'a>(&'a mut App);

impl TokenSink for AppTokenSink<'_> {
    type Handle = ();

    fn process_token(&mut self, token: Token, _line_number: u64) -> TokenSinkResult<Self::Handle> {
        if let Token::TagToken(tag) = token {
            if &tag.name == "a" {
                for attr in tag.attrs {
                    if &attr.name.local == "href" {
                        self.0.add(&attr.value);
                    }
                }
            }
        }
        TokenSinkResult::Continue
    }
}
