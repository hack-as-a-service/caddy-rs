pub use caddy_types as types;

use reqwest::{Client, Url};
use serde::{de::DeserializeOwned, ser::Serialize};
use thiserror::Error;

#[derive(Error, Debug)]
pub enum CaddyError {
	#[error("Provided API base cannot be a base")]
	CannotBeABase,
	#[error("Reqwest error: {0}")]
	Reqwest(#[from] reqwest::Error),
}

pub type Result<T> = std::result::Result<T, CaddyError>;

fn joining_path(u: &Url, path: &[&str]) -> Url {
	let new_path = u
		.path_segments()
		.unwrap()
		.chain(path.into_iter().copied())
		.collect::<Vec<_>>()
		.join("/");
	let url = {
		let mut u2 = u.clone();
		u2.set_path(&new_path);
		u2
	};
	url
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
		Ok(self
			.client
			.get(joining_path(&self.api_base, path))
			.send()
			.await?
			.error_for_status()?
			.json()
			.await?)
	}

	/// Sets the snippet of Caddy configuration at the given path.
	pub async fn set_config<Q: Serialize>(&self, path: &[&str], config: &Q) -> Result<()> {
		let _ = self
			.client
			.post(joining_path(&self.api_base, path))
			.json(config)
			.send()
			.await?
			.error_for_status()?;
		Ok(())
	}

	/// Puts the snippet of Caddy configuration at the given path.
	pub async fn put_config<Q: Serialize>(&self, path: &[&str], config: &Q) -> Result<()> {
		let _ = self
			.client
			.put(joining_path(&self.api_base, path))
			.json(config)
			.send()
			.await?
			.error_for_status()?;
		Ok(())
	}

	/// Deletes the snippet of Caddy configuration at the given path.
	pub async fn delete_config(&self, path: &[&str]) -> Result<()> {
		let _ = self
			.client
			.delete(joining_path(&self.api_base, path))
			.send()
			.await?
			.error_for_status()?;
		Ok(())
	}

	/// Stops the Caddy server gracefully and exits the process.
	pub async fn stop(&self) -> Result<()> {
		let _ = self
			.client
			.post(joining_path(&self.api_base, &["stop"]))
			.send()
			.await?
			.error_for_status()?;
		Ok(())
	}

	/// Loads a new Caddy configuration; if the load fails, the old configuration stays running with no downtime.
	pub async fn load<Q: Serialize>(&self, config: &Q) -> Result<()> {
		let _ = self
			.client
			.post(joining_path(&self.api_base, &["load"]))
			.json(config)
			.send()
			.await?
			.error_for_status()?;
		Ok(())
	}
}
