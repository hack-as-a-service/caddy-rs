//! Caddy's types, automatically generated from the JSON docs.
//! The docs for each item is copied from the JSON, so there may be inaccuracies.
//! Not all modules are included; only the ones that come with Caddy (and make sense to type) are included.

// HACK: the "" namespace is renamed to "apps"
caddy_types_codegen::caddy_types!("json/root.json" => {
	"apps" => (module_map) {
		http => "json/caddy.apps/http.json",
		pki => "json/caddy.apps/pki.json",
		tls => "json/caddy.apps/tls.json"
	},
	"caddy.storage" => module (module) {
		file_system => "json/caddy.storage/file_system.json"
	},
	"caddy.config_loaders" => module (module) {
		http => "json/caddy.config_loaders/http.json"
	},
	"caddy.logging.writers" => output (module) {
		discard => "json/caddy.logging.writers/discard.json",
		stderr => "json/caddy.logging.writers/stderr.json",
		stdout => "json/caddy.logging.writers/stdout.json",
		file => "json/caddy.logging.writers/file.json",
		net => "json/caddy.logging.writers/net.json"
	},
	"caddy.logging.encoders" => format (module) {
		console => "json/caddy.logging.encoders/console.json",
		// FIXME: filter excluded since it uses a special format that we can't properly type
		// filter => "json/caddy.logging.encoders/filter.json",
		json => "json/caddy.logging.encoders/json.json",
		logfmt => "json/caddy.logging.encoders/logfmt.json",
		single_field => "json/caddy.logging.encoders/single_field.json"
		// formatted excluded - not part of main Caddy repo
		// https://caddyserver.com/api/docs/config/logging/logs/encoder/formatted
	},
	"caddy.listeners" => wrapper (module) {
		tls => "json/caddy.listeners/tls.json"
		// proxy_protocol excluded - not part of main Caddy repo
		// https://caddyserver.com/api/docs/config/apps/http/servers/listener_wrappers/proxy_protocol
	},
	"tls.issuance" => module (module) {
		acme => "json/tls.issuance/acme.json",
		internal => "json/tls.issuance/internal.json",
		zerossl => "json/tls.issuance/zerossl.json"
	},
	"dns.providers" => name (module) {
		// Empty - none of the dns providers are part of main Caddy repo
		// https://caddyserver.com/api/docs/config/admin/identity/issuers/acme/challenges/dns/provider
	},
	"http.matchers" => (module_map) {
		// NOTE: changed type of this to string to align with docs
		expression => "json/http.matchers/expression.json",
		header => "json/http.matchers/header.json",
		header_regexp => "json/http.matchers/header_regexp.json",
		host => "json/http.matchers/host.json",
		method => "json/http.matchers/method.json",
		// NOTE: changed type of this to array to align with docs
		not => "json/http.matchers/not.json",
		path => "json/http.matchers/path.json",
		path_regexp => "json/http.matchers/path_regexp.json",
		protocol => "json/http.matchers/protocol.json",
		query => "json/http.matchers/query.json",
		remote_ip => "json/http.matchers/remote_ip.json",
		vars_regexp => "json/http.matchers/vars_regexp.json",
		vars => "json/http.matchers/vars.json",
		file => "json/http.matchers/file.json"
	},
	"http.handlers" => handler (module) {
		error => "json/http.handlers/error.json",
		static_response => "json/http.handlers/static_response.json",
		subroute => "json/http.handlers/subroute.json",
		vars => "json/http.handlers/vars.json",
		authentication => "json/http.handlers/authentication.json",
		encode => "json/http.handlers/encode.json",
		file_server => "json/http.handlers/file_server.json",
		headers => "json/http.handlers/headers.json",
		map => "json/http.handlers/map.json",
		push => "json/http.handlers/push.json",
		request_body => "json/http.handlers/request_body.json",
		reverse_proxy => "json/http.handlers/reverse_proxy.json",
		rewrite => "json/http.handlers/rewrite.json",
		templates => "json/http.handlers/templates.json",
		acme_server => "json/http.handlers/acme_server.json",
		metrics => "json/http.handlers/metrics.json"
	},
	"http.authentication.providers" => (module_map) {
		http_basic => "json/http.authentication.providers/http_basic.json"
	},
	"http.authentication.hashes" => algorithm (module) {
		bcrypt => "json/http.authentication.hashes/bcrypt.json",
		scrypt => "json/http.authentication.hashes/scrypt.json"
	},
	"http.precompressed" => (module_map) {
		br => "json/http.precompressed/br.json",
		gzip => "json/http.precompressed/gzip.json",
		zstd => "json/http.precompressed/zstd.json"
	},
	"http.encoders" => (module_map) {
		gzip => "json/http.encoders/gzip.json",
		zstd => "json/http.encoders/zstd.json"
	},
	"http.reverse_proxy.transport" => protocol (module) {
		http => "json/http.reverse_proxy.transport/http.json",
		fastcgi => "json/http.reverse_proxy.transport/fastcgi.json"
	},
	"http.reverse_proxy.circuit_breakers" => r#type (module) {
		// No built-in ones
	},
	"http.reverse_proxy.selection_policies" => policy (module) {
		cookie => "json/http.reverse_proxy.selection_policies/cookie.json",
		first => "json/http.reverse_proxy.selection_policies/first.json",
		header => "json/http.reverse_proxy.selection_policies/header.json",
		ip_hash => "json/http.reverse_proxy.selection_policies/ip_hash.json",
		least_conn => "json/http.reverse_proxy.selection_policies/least_conn.json",
		random_choose => "json/http.reverse_proxy.selection_policies/random_choose.json",
		random => "json/http.reverse_proxy.selection_policies/random.json",
		round_robin => "json/http.reverse_proxy.selection_policies/round_robin.json",
		uri_hash => "json/http.reverse_proxy.selection_policies/uri_hash.json"
	},
	"tls.certificates" => (module_map) {
		automate => "json/tls.certificates/automate.json",
		load_files => "json/tls.certificates/load_files.json",
		load_folders => "json/tls.certificates/load_folders.json",
		load_pem => "json/tls.certificates/load_pem.json",
		load_storage => "json/tls.certificates/load_storage.json"
	},
	"tls.stek" => provider (module) {
		distributed => "json/tls.stek/distributed.json",
		standard => "json/tls.stek/standard.json"
	},
	"tls.handshake_match" => (module_map) {
		remote_ip => "json/tls.handshake_match/remote_ip.json",
		sni => "json/tls.handshake_match/sni.json"
	}
});

/// Support for [Caddy's @id field](https://caddyserver.com/docs/api#using-id-in-json).
#[derive(Debug, serde::Serialize, serde::Deserialize)]
#[serde(untagged)]
pub enum Identifiable<T> {
	Identified { id: String, value: T },
	Unidentified(T),
}

impl<T: Default> Default for Identifiable<T> {
	fn default() -> Self {
		Self::Unidentified(T::default())
	}
}

impl<T> From<T> for Identifiable<T> {
	fn from(value: T) -> Self {
		Self::Unidentified(value)
	}
}

impl<T> std::ops::Deref for Identifiable<T> {
	type Target = T;
	fn deref(&self) -> &Self::Target {
		match self {
			Self::Identified { value, .. } => value,
			Self::Unidentified(value) => value,
		}
	}
}
