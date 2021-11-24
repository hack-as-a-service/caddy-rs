use std::borrow::Cow;
use std::collections::{HashMap, HashSet};

use proc_macro2::TokenStream;
use quote::quote;
//use reqwest::blocking as reqwest;
use serde::Deserialize;

//type ReqwestResult<T> = Result<T, ::reqwest::Error>;

// MARK: Codegen

#[derive(Debug, Deserialize)]
struct ConfigStructure {
	status_code: usize,
	result: ConfigStructure2,
}

type Namespaces = HashMap<String, Vec<NamespaceStructure>>;

#[derive(Debug, Deserialize)]
struct ConfigStructure2 {
	structure: ConfigTypeValue,
	namespaces: Namespaces,
	breadcrumb: HashMap<String, ConfigTypeValue>,
	repo: String,
}

#[derive(Debug, Deserialize, Clone)]
struct ConfigTypeValue {
	doc: Option<String>,
	#[serde(flatten)]
	value: ConfigTypeValueInner,
}

#[derive(Debug, Deserialize, Clone)]
#[serde(tag = "type")]
#[serde(rename_all = "snake_case")]
enum ConfigTypeValueInner {
	Struct {
		// This is always present except for the top-level struct,
		// so add the default name for the top-level
		#[serde(default = "default_type_name")]
		type_name: String,
		#[serde(default = "Vec::new")]
		struct_fields: Vec<KeyValuePair>,
	},
	Bool,
	String,
	Int,
	Float,
	Array {
		elems: Box<ConfigTypeValue>,
	},
	Map {
		map_keys: Box<ConfigTypeValue>,
		elems: Box<ConfigTypeValue>,
	},
	Module {
		/* NB: Type name for modules is derived from the module namespace, since it's an enum */
		module_namespace: String,
		module_inline_key: String,
	},
	ModuleMap {
		type_name: String,
		module_namespace: String,
	},
}

fn default_type_name() -> String {
	"CaddyConfig".to_owned()
}

#[derive(Debug, Deserialize)]
struct NamespaceStructure {
	name: String,
	docs: Option<String>,
	package: String,
	repo: String,
}

#[derive(Debug, Deserialize, Clone)]
struct KeyValuePair {
	key: String,
	value: ConfigTypeValue,
}

// (actual output, auxillary output)
type Generated = (TokenStream, TokenStream);

fn clean_up_go_type_name_path(path: &str) -> Cow<str> {
	// FIXME: make try expression once stabilized
	let tmp_path = (move || Some(path.split("/").last()?.split(".").last()?))().unwrap_or(path);
	if tmp_path == "Handler" {
		// heuristic: merge together dots and try again
		clean_up_go_type_name_path(&path.replace(".", ""))
			.to_string()
			.into()
	} else {
		tmp_path.into()
	}
}

/// Make a prefixed type (crate-local)
// fn mk_prefixed_ty(ty: TokenStream, prefix: Option<&TokenStream>, add_crate: bool) -> TokenStream {
// 	let tmp = if let Some(prefix) = prefix {
// 		quote! { #prefix::#ty }
// 	} else {
// 		// quote! { crate::#ty }
// 		ty
// 	};
// 	if add_crate {
// 		quote! { crate::#tmp }
// 	} else {
// 		tmp
// 	}
// }

/// Hack: fix up empty module namespace into "apps"
fn fixup_module_namespace(module_namespace: &str) -> &str {
	if module_namespace.is_empty() {
		"apps"
	} else {
		module_namespace
	}
}

/// Hack: Inverse of fixup_module_namespace
fn fixup_module_namespace2(module_namespace: &str) -> &str {
	if module_namespace == "apps" {
		""
	} else {
		module_namespace
	}
}

/// Generate a module (type) name from the given namespace name
fn mk_module_ty_name(namespace: &str) -> String {
	use heck::CamelCase;
	fixup_module_namespace(&namespace)
		.replace(".", "_")
		.to_camel_case()
}

/// Generate a module map (type) name from the given namespace name
fn mk_module_map_ty_name(namespace: &str) -> String {
	use heck::CamelCase;
	format!(
		"{}_map",
		fixup_module_namespace(&namespace).replace(".", "_")
	)
	.to_camel_case()
}

/// Generate a proper type name from the given config
/// Invariant: must be a /pure/ type name(!)
fn mk_type_name(config: &ConfigTypeValueInner) -> Cow<str> {
	use ConfigTypeValueInner::*;
	match config {
		Struct { type_name, .. } => clean_up_go_type_name_path(type_name).into(),
		Module {
			module_namespace, ..
		} => mk_module_ty_name(&module_namespace).into(),
		ModuleMap {
			module_namespace, ..
		} => mk_module_map_ty_name(&module_namespace).into(),
		/* Primitive types, Vecs, HashMaps */
		_ => panic!("mk_type_name got config without type name!"),
	}
}

/// Generate a module name from a type name
fn mk_module_name(ty_name: &str) -> String {
	use heck::SnakeCase;
	ty_name.to_snake_case()
}

/// Generate a type from the given config
fn mk_type(
	config: &ConfigTypeValueInner,
	boxed: bool, /* , prefix: Option<&TokenStream> */
) -> TokenStream {
	use ConfigTypeValueInner::*;
	match config {
		/* big int? */
		Struct { type_name, .. } if type_name.to_lowercase().ends_with("bigint") => quote! { i64 },
		Struct { .. } | Module { .. } | ModuleMap { .. } => {
			let ty_name = mk_type_name(config);
			let ty_name_lit = quote::format_ident!("{}", ty_name);
			// let is_mod = matches!(config, Module { .. } | ModuleMap { .. });
			// mk_prefixed_ty(
			// 	quote! { #ty_name_lit },
			// 	/* Modules / module maps are always top level */
			// 	if is_mod { None } else { prefix },
			// 	is_mod,
			// )
			let q = quote! { crate::#ty_name_lit };
			if boxed {
				quote! { crate::Identified<::std::boxed::Box<#q>> }
			} else {
				quote! { crate::Identified<#q> }
			}
		}
		Bool => quote! { bool },
		String => quote! { ::std::string::String },
		Int => quote! { i32 },
		Float => quote! { f32 },
		Array { elems } => {
			let elems_ty = mk_type(&elems.value, false /* , prefix */);
			quote! { ::std::vec::Vec<#elems_ty> }
		}
		Map { map_keys, elems } => {
			let key_ty = mk_type(&map_keys.value, false /* , prefix */);
			let val_ty = mk_type(&elems.value, false /* , prefix */);
			quote! { ::std::collections::HashMap<#key_ty, #val_ty> }
		}
	}
}

/// Generate the inner struct-like block for a struct
/// Invariant assumed that the passed config is a struct config
fn mk_struct_block(
	config: &ConfigTypeValue,
	mod_name: Option<&str>,
	ty_names_set: &mut HashSet<String>,
) -> Generated {
	use ConfigTypeValueInner::*;
	if let Struct { struct_fields, .. } = &config.value {
		let mut aux = vec![];
		let mut field_names = vec![];
		let mut field_attrs = vec![];
		let mut field_tys = vec![];
		let mut field_docs = vec![];
		let mod_name_lit = mod_name
			.map(|n| quote::format_ident!("{}", n))
			.map(|i| quote! { #i });
		for field in struct_fields {
			// let should_insert_defs = if matches!(&field.value.value, Struct { .. }) {
			// 	let ty_name = mk_type_name(&field.value.value);
			// 	let has_ty = ty_names_set.insert(ty_name.to_string());
			// 	!has_ty
			// } else {
			// 	true
			// };
			let (field_def, field_aux) = mk_def(&field.value, ty_names_set);
			aux.push(field_aux);
			aux.push(field_def);
			// if should_insert_defs {
			// 	aux.push(field_aux);
			// 	aux.push(field_def);
			// }
			// let crate_quote = quote! { crate };
			let is_boxed = if matches!(
				&field.value.value,
				Struct { .. } | Module { .. } | ModuleMap { .. }
			) {
				let ty_name1 = mk_type_name(&config.value);
				let ty_name2 = mk_type_name(&field.value.value);
				// eprintln!("{} {}", ty_name1, ty_name2);
				if ty_name1 == "SingleFieldEncoder" && ty_name2 == "CaddyLoggingEncoders" {
					// eprintln!("Boxing");
					true
				} else {
					false
				}
			} else {
				false
			};
			let ty = mk_type(
				&field.value.value,
				is_boxed, /* Modules and module maps are always defined at crate level */
						  /* if matches!(&field.value.value, Module { .. } | ModuleMap { .. }) {
							  None
						  } else {
							  mod_name_lit.as_ref()
						  }, */
			);
			// Fixes reserved keywords as field names
			use heck::SnakeCase;
			let snake_key = field.key.to_snake_case();
			let field_key = &field.key;
			let name_lit = quote::format_ident!("r#{}", &snake_key);
			field_names.push(quote! { #name_lit });
			field_attrs.push(quote! { #[serde(rename = #field_key)] });
			field_tys.push(ty);
			// if matches!(&field.value.value, Struct { .. }) {
			// 	let ty_name1 = mk_type_name(&config.value);
			// 	let ty_name2 = mk_type_name(&field.value.value);
			// 	if ty_name1 == "SingleFieldEncoder" && ty_name2 == "CaddyLoggingEncoders" {
			// 		/* Hack: Prevent infinitely sized type */
			// 		field_tys.push(quote! { ::std::boxed::Box<#ty> });
			// 	} else {
			// 		field_tys.push(ty);
			// 	}
			// } else {
			// 	field_tys.push(ty);
			// }
			let field_doc = field.value.doc.as_deref().unwrap_or("");
			field_docs.push(if field_doc.is_empty() {
				quote! {}
			} else {
				quote! { #[doc = #field_doc] }
			});
		}
		/* Prune empty modules */
		let aux_collected = aux.into_iter().fold(quote! {}, |acc, x| quote! { #acc #x });
		// let aux = if aux_collected.is_empty() {
		// 	quote! {}
		// } else {
		// 	aux_collected
		// 	// /* Put aux in a module if a name was provided */
		// 	// if let Some(mod_name_lit) = mod_name_lit {
		// 	// 	quote! {
		// 	// 		pub mod #mod_name_lit {
		// 	// 			#aux_collected
		// 	// 		}
		// 	// 	}
		// 	// } else {
		// 	// 	aux_collected
		// 	// }
		// };
		(
			quote! {
				{
					#(
						#field_docs
						#field_attrs
						#[serde(skip_serializing_if = "Option::is_none")]
						pub #field_names: ::std::option::Option<#field_tys>
					),*
				}
			},
			aux_collected,
		)
	} else {
		panic!("mk_struct_block requires a struct config!")
	}
}

/// Generate a struct definition, with docs
/// Invariant assumed that the passed config is a struct config
fn mk_struct_def(
	config: &ConfigTypeValue,
	rename_ty: Option<&str>,
	mod_name: Option<&str>,
	ty_names_set: &mut HashSet<String>,
) -> Generated {
	// let original_ty_name = mk_type_name(&config.value);
	let ty_name = rename_ty
		.map(|x| x.into())
		.unwrap_or_else(|| mk_type_name(&config.value));
	let ty_name_lit = quote::format_ident!("{}", ty_name);
	let ty_name_lit = quote! { #ty_name_lit };
	/* Prune empty docs */
	let doc = config.doc.as_deref().unwrap_or("");
	let doc = if doc.is_empty() {
		quote! {}
	} else {
		quote! { #[doc = #doc] }
	};
	let (block, aux) = mk_struct_block(
		config,
		mod_name.or(Some(&mk_module_name(&ty_name))),
		ty_names_set,
	);
	//let has_ty = false; //ty_names_set.insert(ty_name.to_string());
	(
		quote! {
			#doc
			#[derive(Debug, Default, ::serde::Serialize, ::serde::Deserialize)]
			#[serde(rename_all = "snake_case")]
			pub struct #ty_name_lit #block
		},
		aux,
	)
}

fn mk_def(config: &ConfigTypeValue, ty_names_set: &mut HashSet<String>) -> Generated {
	use ConfigTypeValueInner::*;
	match &config.value {
		/* Special cased in mk_ty */
		Struct { type_name, .. } if type_name.to_lowercase().ends_with("bigint") => {
			(quote! {}, quote! {})
		}
		Struct { .. } => {
			let ty_name = mk_type_name(&config.value);
			let has_ty = ty_names_set.contains(&*ty_name);
			if has_ty {
				/* Don't repeat definitions */
				(quote! {}, quote! {})
			} else {
				ty_names_set.insert(ty_name.to_string());
				mk_struct_def(config, None, None, ty_names_set)
			}
		}
		/* Modules and module maps should be defined by top level, which don't use this function to generate definitions */
		Module { .. } | ModuleMap { .. } => (quote! {}, quote! {}),
		/* Primitive types don't need defs (std) */
		Bool | String | Int | Float => (quote! {}, quote! {}),
		/* Arrays / maps need definitions of their inner types (which goes in aux) */
		Array { elems } => {
			let (inner_def, inner_aux) = mk_def(&elems, ty_names_set);
			(quote! {}, quote! { #inner_def #inner_aux })
		}
		Map { map_keys, elems } => {
			let (keys_def, keys_aux) = mk_def(&map_keys, ty_names_set);
			let (val_def, val_aux) = mk_def(&elems, ty_names_set);
			(quote! {}, quote! { #keys_def #keys_aux #val_def #val_aux })
		}
	}
}

enum NamespaceDefTypes {
	Module,
	ModuleMap,
	// easier than using bitflags or something
	ModuleAndModuleMap,
}

impl NamespaceDefTypes {
	fn module(&self) -> bool {
		matches!(self, Self::Module | Self::ModuleAndModuleMap)
	}
	fn module_map(&self) -> bool {
		matches!(self, Self::ModuleMap | Self::ModuleAndModuleMap)
	}
}

struct NamespaceDef {
	inline_tag: Option<String>,
	defn_types: NamespaceDefTypes,
	values: HashMap<String, ConfigTypeValue>,
}

/// Create aux definitions for module (maps)
fn mk_module_aux(
	name: &str,
	values: &NamespaceDef,
	ty_names_set: &mut HashSet<String>,
) -> Generated {
	let name_fixed = mk_module_map_ty_name(name);
	// let name_temp = mk_module_ty_name(name);
	let config = ConfigTypeValue {
		/* Modules don't have docs (for now), FIXME */
		doc: None,
		value: ConfigTypeValueInner::Struct {
			type_name: name_fixed,
			struct_fields: values
				.values
				.iter()
				.map(|(k, v)| KeyValuePair {
					key: k.clone(),
					value: v.clone(),
				})
				.collect(),
		},
	};
	/* rename the module using the passed name */
	let mod_name = mk_module_name(&mk_module_ty_name(name));
	mk_struct_def(&config, None, Some(&mod_name), ty_names_set)
}

/// Create an actual module definition (enum)
fn mk_module_def(
	name: &str,
	values: &NamespaceDef,
	ty_names_set: &mut HashSet<String>,
) -> Generated {
	use heck::CamelCase;
	let ty_name = mk_module_ty_name(name);
	let ty_name_lit = quote::format_ident!("{}", ty_name);
	let ty_name_lit = quote! { #ty_name_lit };
	let mod_name = mk_module_name(&ty_name);
	let mod_name_lit = quote::format_ident!("{}", mod_name);
	let mod_name_lit = quote! { #mod_name_lit };
	/* We don't need the struct, just the aux definitions */
	let (_, aux) = mk_module_aux(name, values, ty_names_set);
	let mut variants = Vec::new();
	for (field_name, field_config) in &values.values {
		let camel_case_field_name = field_name.to_camel_case();
		let camel_case_field_name_lit = quote::format_ident!("{}", camel_case_field_name);
		let camel_case_field_name_lit = quote! { #camel_case_field_name_lit };
		/* Use type as inner value */
		let field_ty = mk_type(&field_config.value, false /* , Some(&mod_name_lit) */);
		variants.push(quote! {
			#camel_case_field_name_lit(#field_ty)
		});
	}
	// let mod_name = mk_module_name(&ty_name);
	// let mod_name_lit = quote::format_ident!("{}", mod_name);
	// let mod_name_lit = quote! { #mod_name_lit };
	// let mut aux = Vec::new();
	// let mut variants = Vec::new();
	// for (field_name, field_config) in &values.values {
	// 	let camel_case_field_name = field_name.to_camel_case();
	// 	let camel_case_field_name_lit = quote::format_ident!("{}", camel_case_field_name);
	// 	let camel_case_field_name_lit = quote! { #camel_case_field_name_lit };
	// 	let (block, field_aux) = mk_struct_block(field_config, Some(&mod_name));
	// 	aux.push(field_aux);
	// 	variants.push(quote! {
	// 		#camel_case_field_name_lit #block
	// 	});
	// }
	// /* Prune empty modules */
	// // let aux_collected = aux.into_iter().fold(quote! {}, |acc, x| quote! { #acc #x });
	// // let aux = if aux_collected.is_empty() {
	// // 	quote! {}
	// // } else {
	// // 	quote! {
	// // 		pub mod #mod_name_lit {
	// // 			#aux_collected
	// // 		}
	// // 	}
	// // };
	let tag = values
		.inline_tag
		.as_ref()
		.expect("mk_module_def: expected inline tag value");
	(
		quote! {
			#[derive(Debug, ::serde::Serialize, ::serde::Deserialize)]
			#[serde(rename_all = "snake_case")]
			#[serde(tag = #tag)]
			pub enum #ty_name_lit {
				#(#variants),*
			}
		},
		aux,
	)
}

/// Create an actual module map definition (struct)
fn mk_module_map_def(
	name: &str,
	values: &NamespaceDef,
	ty_names_set: &mut HashSet<String>,
) -> Generated {
	/* This function uses both outputs, not just the aux */
	mk_module_aux(name, values, ty_names_set)
}

fn mk_toplevel(
	structure: &ConfigTypeValue,
	namespace_defs: &HashMap<String, NamespaceDef>,
) -> TokenStream {
	let mut ty_names_set = HashSet::new();
	let (struct_def, struct_defs_aux) = mk_struct_def(&structure, None, None, &mut ty_names_set);
	let mut module_defs = vec![];
	let mut module_defs_aux = vec![];
	for (name, value) in namespace_defs {
		if value.defn_types.module_map() {
			let (map_def, map_def_aux) = mk_module_map_def(&name, value, &mut ty_names_set);
			module_defs.push(map_def);
			module_defs_aux.push(map_def_aux);
		}
		if value.defn_types.module() {
			let (module_def, module_def_aux) = mk_module_def(&name, value, &mut ty_names_set);
			module_defs.push(module_def);
			/* If we are generating a module map as well, the aux definitions are already there */
			if !value.defn_types.module_map() {
				module_defs_aux.push(module_def_aux);
			}
		}
	}
	quote! {
		#struct_defs_aux
		#(#module_defs_aux)*
		#(#module_defs)*
		#struct_def
	}
}

// fn get_structure(url: &str) -> ReqwestResult<ConfigStructure> {
// 	let c = reqwest::Client::new();
// 	let body = c
// 		.get(url)
// 		.header(::reqwest::header::ACCEPT, "application/json")
// 		.send()?;
// 	Ok(body.json()?)
// 	//.bytes()?;
// 	//println!("bytes = {:?}", body);
// 	//Ok(serde_path_to_error::deserialize(&mut serde_json::Deserializer::from_reader(body)).unwrap())
// }

// MARK: proc macro

use std::env;
use std::fs::File;
use std::path::PathBuf;
use syn::parse::{Error as SynError, Parse, ParseStream};
use syn::punctuated::Punctuated;
use syn::Result as SynResult;
use syn::{Ident, LitStr, Token};

trait ReadConfigTypeValue {
	fn read_config(&self) -> Result<ConfigTypeValue, SynError>;
}

// impl<S: AsRef<std::path::Path> + std::fmt::Display> ReadConfigTypeValue for S {
// 	fn read_config(&self) -> Result<ConfigTypeValue, ()> {
// 		let manifest_dir = PathBuf::from(env::var("CARGO_MANIFEST_DIR").unwrap());
// 		let mut f = File::open(manifest_dir.join(self))
// 			.map_err(|_| eprintln!("Failed to open {}", self))?;
// 		let cfg = serde_json::from_reader(f)
// 			.map_err(|_| eprintln!("Failed to deserialize {} as ConfigTypeValue", self))?;
// 		Ok(cfg)
// 	}
// }

impl ReadConfigTypeValue for LitStr {
	fn read_config(&self) -> Result<ConfigTypeValue, SynError> {
		(|| {
			let value = self.value();
			let manifest_dir = PathBuf::from(env::var("CARGO_MANIFEST_DIR").unwrap());
			let f = File::open(manifest_dir.join(&value))
				.map_err(|_| "Failed to open file".to_owned())?;
			let cfg = serde_json::from_reader(f)
				.map_err(|e| format!("Failed to deserialize as ConfigTypeValue: {}", e))?;
			Ok(cfg)
		})()
		.map_err(|e: String| SynError::new_spanned(self, e))
	}
}

struct NamespaceFieldDef2 {
	name: Ident,
	def: LitStr,
}

impl Parse for NamespaceFieldDef2 {
	fn parse(input: ParseStream) -> SynResult<Self> {
		let name = input.parse::<Ident>()?;
		input.parse::<Token![=>]>()?;
		let def = input.parse::<LitStr>()?;
		Ok(Self { name, def })
	}
}

struct NamespaceDef2 {
	values: HashMap<Ident, LitStr>,
	inline_tag: Option<String>,
	defn_types: NamespaceDefTypes,
}

impl Parse for NamespaceDef2 {
	fn parse(input: ParseStream) -> SynResult<Self> {
		let inline_tag = if input.peek(Ident) {
			// let content;
			// syn::parenthesized!(content in input);
			let tag_ident = input.parse::<Ident>()?;
			Some(tag_ident.to_string())
		} else {
			None
			// Some("".to_owned()) // Hack: make mk_module_def happy
		};
		let content2;
		let _parenthesized = syn::parenthesized!(content2 in input);
		let values: Punctuated<Ident, Token![,]> = content2.parse_terminated(Ident::parse)?;
		let has_module = values.iter().any(|x| x == "module");
		let has_modmap = values.iter().any(|x| x == "module_map");
		let defn_types = match (has_module, has_modmap) {
			(true, false) => NamespaceDefTypes::Module,
			(false, true) => NamespaceDefTypes::ModuleMap,
			(true, true) => NamespaceDefTypes::ModuleAndModuleMap,
			_ => {
				return Err(SynError::new_spanned(
					&values,
					"expected at least one of module, module_map",
				))
			}
		};
		// let inline_tag = if input.peek(syn::Ident) {
		// 	let tag_ident = input.parse::<syn::Ident>()?;
		// 	Some(tag_ident.to_string())
		// } else {
		// 	None
		// };
		let content;
		let _braced = syn::braced!(content in input);
		let values: Punctuated<NamespaceFieldDef2, Token![,]> =
			content.parse_terminated(NamespaceFieldDef2::parse)?;
		let values = values.into_iter().map(|f| (f.name, f.def)).collect();
		Ok(Self {
			inline_tag,
			values,
			defn_types,
		})
	}
}

impl NamespaceDef2 {
	fn to_def(self) -> Result<NamespaceDef, SynError> {
		let mut values = HashMap::new();
		for (k, v) in self.values {
			values.insert(k.to_string(), v.read_config()?);
		}
		Ok(NamespaceDef {
			values,
			inline_tag: self.inline_tag,
			defn_types: self.defn_types,
		})
	}
}

struct NamespaceField {
	name: String,
	def: NamespaceDef2,
}

impl Parse for NamespaceField {
	fn parse(input: ParseStream) -> SynResult<Self> {
		let lit_name = input.parse::<syn::LitStr>()?;
		input.parse::<syn::Token![=>]>()?;
		let def = input.parse()?;
		Ok(Self {
			name: fixup_module_namespace2(&(lit_name.value())).to_string(),
			def,
		})
	}
}

struct CaddyTypesInput {
	root: LitStr,
	namespaces: HashMap<String, NamespaceDef2>,
}

impl Parse for CaddyTypesInput {
	fn parse(input: ParseStream) -> SynResult<Self> {
		let lit_root = input.parse::<syn::LitStr>()?;
		input.parse::<syn::Token![=>]>()?;
		let content;
		let _braced = syn::braced!(content in input);
		let namespaces: syn::punctuated::Punctuated<NamespaceField, syn::Token![,]> =
			content.parse_terminated(NamespaceField::parse)?;
		let namespaces = namespaces.into_iter().map(|x| (x.name, x.def)).collect();
		Ok(Self {
			root: lit_root,
			namespaces,
		})
	}
}

impl CaddyTypesInput {
	fn to_structure(self) -> Result<(ConfigTypeValue, HashMap<String, NamespaceDef>), SynError> {
		let root_config = self.root.read_config()?;
		let mut namespaces = HashMap::new();
		for (k, v) in self.namespaces.into_iter() {
			namespaces.insert(k, v.to_def()?);
		}
		Ok((root_config, namespaces))
	}
}

#[proc_macro]
pub fn caddy_types(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
	let input2 = input.clone();
	let input_parsed = syn::parse_macro_input!(input2 as CaddyTypesInput);
	let (cfg, namespaces) = match input_parsed.to_structure() {
		Ok(s) => s,
		/* Errors have already been printed / spanned */
		Err(e) => return e.into_compile_error().into(),
	};
	let out = mk_toplevel(&cfg, &namespaces);
	// println!("{}", out);
	out.into()
}
