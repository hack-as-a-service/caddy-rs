pub use caddy_types as types;

use reqwest::Client;
pub use reqwest::Url;
use serde::{de::DeserializeOwned, ser::Serialize};
use thiserror::Error;

#[derive(Error, Debug)]
pub enum CaddyError {
	#[error("Provided API base cannot be a base")]
	CannotBeABase,
	#[error("Reqwest error: {0}")]
	Reqwest(#[from] reqwest::Error),
	#[error("Caddy error: {0}")]
	Caddy(String),
}

pub type Result<T> = std::result::Result<T, CaddyError>;

fn joining_path(u: &Url, path: &[&str]) -> Url {
	let new_path = u
		.path_segments()
		.unwrap()
		.chain(path.iter().copied())
		.collect::<Vec<_>>()
		.join("/");
	let mut u2 = u.clone();
	u2.set_path(&new_path);
	u2
}

#[derive(serde::Serialize, serde::Deserialize)]
struct CaddyErrorJSON {
	error: String,
}

impl From<CaddyErrorJSON> for CaddyError {
	fn from(e: CaddyErrorJSON) -> Self {
		Self::Caddy(e.error)
	}
}

async fn response_error_for_status(
	r: reqwest::Response,
) -> Result<std::result::Result<reqwest::Response, CaddyErrorJSON>> {
	if r.status().is_client_error() || r.status().is_server_error() {
		let e = r.json().await?;
		Ok(Err(e))
	} else {
		Ok(Ok(r))
	}
}

/// A Caddy client using a specified API base.
pub struct CaddyClient {
	api_base: Url,
	client: Client,
}

impl CaddyClient {
	pub fn new(api_base: Url) -> Result<Self> {
		if api_base.cannot_be_a_base() {
			return Err(CaddyError::CannotBeABase);
		}
		let client = Client::new();
		Ok(Self { api_base, client })
	}

	/// Gets the snippet of Caddy configuration at the given path.
	pub async fn get_config<Q: DeserializeOwned>(&self, path: &[&str]) -> Result<Q> {
		Ok(response_error_for_status(
			self.client
				.get(joining_path(
					&joining_path(&self.api_base, &["config"]),
					path,
				))
				.send()
				.await?,
		)
		.await??
		.json()
		.await?)
	}

	/// Gets the snippet of Caddy configuration referred to by the given ID.
	pub async fn get_id_config<Q: DeserializeOwned>(&self, id: &str) -> Result<Option<Q>> {
		let r = self
			.client
			.get(joining_path(&self.api_base, &["id", id]))
			.send()
			.await?;
		match response_error_for_status(r).await? {
			Ok(r) => Ok(Some(r.json().await?)),
			Err(e) => {
				if e.error.starts_with("unknown object ID") {
					Ok(None)
				} else {
					Err(e.into())
				}
			}
		}
	}

	/// Adds the snippet of Caddy configuration at the given path.
	pub async fn add_config<Q: Serialize>(&self, path: &[&str], config: &Q) -> Result<()> {
		let _ = response_error_for_status(
			self.client
				.post(joining_path(
					&joining_path(&self.api_base, &["config"]),
					path,
				))
				.json(config)
				.send()
				.await?,
		)
		.await??;
		Ok(())
	}

	/// Puts the snippet of Caddy configuration at the given path.
	pub async fn put_config<Q: Serialize>(&self, path: &[&str], config: &Q) -> Result<()> {
		let _ = response_error_for_status(
			self.client
				.put(joining_path(
					&joining_path(&self.api_base, &["config"]),
					path,
				))
				.json(config)
				.send()
				.await?,
		)
		.await??;
		Ok(())
	}

	/// Deletes the snippet of Caddy configuration at the given path.
	pub async fn delete_config(&self, path: &[&str]) -> Result<()> {
		let _ = response_error_for_status(
			self.client
				.delete(joining_path(
					&joining_path(&self.api_base, &["config"]),
					path,
				))
				.send()
				.await?,
		)
		.await??;
		Ok(())
	}

	/// Stops the Caddy server gracefully and exits the process.
	pub async fn stop(&self) -> Result<()> {
		let _ = response_error_for_status(
			self.client
				.post(joining_path(&self.api_base, &["stop"]))
				.send()
				.await?,
		)
		.await??;
		Ok(())
	}

	/// Loads a new Caddy configuration; if the load fails, the old configuration stays running with no downtime.
	pub async fn load<Q: Serialize>(&self, config: &Q) -> Result<()> {
		let _ = response_error_for_status(
			self.client
				.post(joining_path(&self.api_base, &["load"]))
				.json(config)
				.send()
				.await?,
		)
		.await??;
		Ok(())
	}
}
