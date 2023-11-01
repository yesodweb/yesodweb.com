use std::process::{Child, Command};

pub(crate) struct RunningServer {
    child: Child,
    host_root: String,
}

const PORT: &str = "4545";

impl RunningServer {
    pub(crate) fn new(client: &reqwest::blocking::Client) -> anyhow::Result<Self> {
        println!("Launching yesodweb.com...");
        let child = Command::new("stack")
            .args(["run", "--", "Development", "--port", PORT])
            .current_dir("..")
            // .stdout(Stdio::null())
            // .stderr(Stdio::null())
            .spawn()?;
        let running_server = RunningServer {
            child,
            host_root: format!("http://localhost:{PORT}"),
        };
        println!("Waiting for process to respond...");
        for i in 1..=100 {
            match client.get(&running_server.host_root).send() {
                Ok(_) => {
                    println!("yesodweb.com process responding, starting");
                    return Ok(running_server);
                }
                Err(_) => {
                    println!("Not ready yet, attempt {i}. Sleeping a bit...");
                    std::thread::sleep(std::time::Duration::from_millis(300));
                }
            }
        }
        Err(anyhow::anyhow!("yesodweb.com server never started"))
    }

    pub(crate) fn host_root(&self) -> &str {
        &self.host_root
    }
}

impl Drop for RunningServer {
    fn drop(&mut self) {
        println!("Killing yesodweb.com process...");
        if let Err(e) = self.child.kill() {
            eprintln!("Unable to kill yesodweb.com process: {e:?}");
        }
        if let Err(e) = self.child.wait() {
            eprintln!("Failed waiting on yesodweb.com process: {e:?}");
        }
    }
}
