use caddy::{types, CaddyClient, Url};

#[tokio::main]
async fn main() -> anyhow::Result<()> {
	let client = CaddyClient::new(Url::parse("http://localhost:2019/")?)?;
	let config = client.get_config::<types::Config>(&[]).await?;
	println!("{:#?}", config);
	Ok(())
}
