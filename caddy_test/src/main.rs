use caddy::{types, CaddyClient, Url};

#[tokio::main]
async fn main() -> anyhow::Result<()> {
	let client = CaddyClient::new(Url::parse("http://localhost:2019/")?)?;
	let config = client.config_root().get::<types::Config>().await?;
	println!("{:#?}", config);
	Ok(())
}
