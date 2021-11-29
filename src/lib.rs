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

async fn make_request_handling_errors<Q: Serialize>(
	url: Url,
	client: &Client,
	method: reqwest::Method,
	body: Option<&Q>,
) -> Result<reqwest::Response> {
	let mut builder = client.request(method, url);
	if let Some(body) = body {
		builder = builder.json(body);
	}
	let response = builder.send().await?;
	if response.status().is_client_error() || response.status().is_server_error() {
		// Response contains a caddy error
		let err = response.json::<CaddyErrorJSON>().await?;
		Err(err.into())
	} else {
		Ok(response)
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

	/// Creates a handle to the root Caddy configuration.
	pub fn config_root(&self) -> ConfigHandle {
		let new_url = joining_path(&self.api_base, &["config"]);
		ConfigHandle::new(new_url, &self.client)
	}

	/// Creates a handle to the snippet of Caddy configuration at the given path.
	pub fn config_by_path(&self, path: &[&str]) -> ConfigHandle {
		let new_url = joining_path(
			&self.api_base,
			["config"]
				.iter()
				.copied()
				.chain(path.iter().copied())
				.collect::<Vec<_>>()
				.as_slice(),
		);
		ConfigHandle::new(new_url, &self.client)
	}

	/// Creates a handle to the snippet of Caddy configuration referred to by the given id.
	pub fn config_by_id(&self, id: &str) -> ConfigHandle {
		let new_url = joining_path(&self.api_base, &["id", id]);
		ConfigHandle::new(new_url, &self.client)
	}

	/// Stops the Caddy server gracefully and exits the process.
	pub async fn stop(self) -> Result<()> {
		let url = joining_path(&self.api_base, &["stop"]);
		let _ = make_request_handling_errors::<()>(url, &self.client, reqwest::Method::POST, None)
			.await?;
		Ok(())
	}

	/// Loads a new Caddy configuration; if the load fails, the old configuration stays running with no downtime.
	pub async fn load<Q: Serialize>(&self, config: &Q) -> Result<()> {
		let url = joining_path(&self.api_base, &["load"]);
		let _ = make_request_handling_errors::<Q>(
			url,
			&self.client,
			reqwest::Method::POST,
			Some(config),
		)
		.await?;
		Ok(())
	}
}

/// A handle to a snippet of Caddy configuration, from a specific client.
pub struct ConfigHandle<'a> {
	url: Url,
	client: &'a Client,
}

impl<'a> ConfigHandle<'a> {
	fn new(url: Url, client: &'a Client) -> Self {
		Self { url, client }
	}

	/// Appends path components to this handle.
	pub fn appending_path(self, path: &[&str]) -> Self {
		Self {
			url: joining_path(&self.url, path),
			client: self.client,
		}
	}

	/// Gets the snippet of Caddy configuration referred to by this handle, if any.
	pub async fn get<Q: DeserializeOwned>(&self) -> Result<Option<Q>> {
		let response = match make_request_handling_errors::<()>(
			self.url.clone(),
			self.client,
			reqwest::Method::GET,
			None,
		)
		.await
		{
			Ok(r) => r,
			Err(err) => {
				if let CaddyError::Caddy(ref s) = err {
					if s.starts_with("unknown identifier") {
						return Ok(None);
					} else {
						return Err(err);
					}
				} else {
					return Err(err);
				}
			}
		};
		Ok(Some(response.json().await?))
	}

	/// POSTs the snippet of Caddy configuration referred to by this handle.
	pub async fn post<Q: Serialize>(&self, config: &Q) -> Result<()> {
		let _ = make_request_handling_errors::<Q>(
			self.url.clone(),
			self.client,
			reqwest::Method::POST,
			Some(config),
		)
		.await?;
		Ok(())
	}

	/// PUTs the snippet of Caddy configuration referred to by this handle.
	pub async fn put<Q: Serialize>(&self, config: &Q) -> Result<()> {
		let _ = make_request_handling_errors::<Q>(
			self.url.clone(),
			self.client,
			reqwest::Method::PUT,
			Some(config),
		)
		.await?;
		Ok(())
	}

	/// PATCHes the snippet of Caddy configuration referred to by this handle.
	pub async fn patch<Q: Serialize>(&self, config: &Q) -> Result<()> {
		let _ = make_request_handling_errors::<Q>(
			self.url.clone(),
			self.client,
			reqwest::Method::PATCH,
			Some(config),
		)
		.await?;
		Ok(())
	}

	/// Deletes the snippet of Caddy configuration referred to by this handle.
	pub async fn delete<Q: Serialize>(self, config: &Q) -> Result<()> {
		let _ = make_request_handling_errors::<Q>(
			self.url,
			self.client,
			reqwest::Method::DELETE,
			Some(config),
		)
		.await?;
		Ok(())
	}
}
