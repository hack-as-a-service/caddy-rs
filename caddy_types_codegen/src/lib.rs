use std::borrow::Cow;
use std::collections::{HashMap, HashSet};

use proc_macro2::TokenStream;
use quote::quote;
use serde::Deserialize;

// MARK: Codegen

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
		// This is always present and the default type name here is
		// basically used as a code smell for invalid JSON
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
		// NB: Type name for modules is derived from the module namespace, since it's an enum
		module_namespace: String,
	},
	ModuleMap {
		module_namespace: String,
	},
}

fn default_type_name() -> String {
	"CaddyConfig".to_owned()
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
	let tmp_path = (move || path.split('/').last()?.split('.').last())().unwrap_or(path);
	if tmp_path == "Handler" {
		// heuristic: merge together dots and try again
		clean_up_go_type_name_path(&path.replace(".", ""))
			.to_string()
			.into()
	} else {
		tmp_path.into()
	}
}

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
	fixup_module_namespace(namespace)
		.replace(".", "_")
		.to_camel_case()
}

/// Generate a module map (type) name from the given namespace name
fn mk_module_map_ty_name(namespace: &str) -> String {
	use heck::CamelCase;
	format!(
		"{}_map",
		fixup_module_namespace(namespace).replace(".", "_")
	)
	.to_camel_case()
}

/// Generate a proper type name from the given config
/// Invariant: must be a /pure/ type name(!)
fn mk_type_name(config: &ConfigTypeValueInner) -> Cow<str> {
	use ConfigTypeValueInner::*;
	match config {
		Struct { type_name, .. } => clean_up_go_type_name_path(type_name),
		Module {
			module_namespace, ..
		} => mk_module_ty_name(module_namespace).into(),
		ModuleMap {
			module_namespace, ..
		} => mk_module_map_ty_name(module_namespace).into(),
		// Primitive types, Vecs, HashMaps
		_ => panic!("mk_type_name got config without type name!"),
	}
}

/// Generate a type from the given config
fn mk_type(config: &ConfigTypeValueInner, boxed: bool) -> TokenStream {
	use ConfigTypeValueInner::*;
	match config {
		// big int?
		Struct { type_name, .. } if type_name.to_lowercase().ends_with("bigint") => quote! { i64 },
		Struct { .. } | Module { .. } | ModuleMap { .. } => {
			let ty_name = mk_type_name(config);
			let ty_name_lit = quote::format_ident!("{}", ty_name);
			let q = quote! { crate::#ty_name_lit };
			if boxed {
				quote! { ::std::boxed::Box<#q> }
			} else {
				q
			}
		}
		Bool => quote! { bool },
		String => quote! { ::std::string::String },
		Int => quote! { i32 },
		Float => quote! { f32 },
		Array { elems } => {
			let elems_ty = mk_type(&elems.value, false);
			quote! { ::std::vec::Vec<#elems_ty> }
		}
		Map { map_keys, elems } => {
			let key_ty = mk_type(&map_keys.value, false);
			let val_ty = mk_type(&elems.value, false);
			quote! { ::std::collections::HashMap<#key_ty, #val_ty> }
		}
	}
}

/// Generate the inner struct-like block for a struct
/// Invariant assumed that the passed config is a struct config
fn mk_struct_block(config: &ConfigTypeValue, ty_names_set: &mut HashSet<String>) -> Generated {
	use ConfigTypeValueInner::*;
	if let Struct { struct_fields, .. } = &config.value {
		let mut aux = vec![];
		let mut field_names = vec![];
		let mut field_attrs = vec![];
		let mut field_tys = vec![];
		let mut field_docs = vec![];
		for field in struct_fields {
			let (field_def, field_aux) = mk_def(&field.value, ty_names_set);
			aux.push(field_aux);
			aux.push(field_def);
			let is_boxed = if matches!(
				&field.value.value,
				Struct { .. } | Module { .. } | ModuleMap { .. }
			) {
				let ty_name1 = mk_type_name(&config.value);
				let ty_name2 = mk_type_name(&field.value.value);
				ty_name1 == "SingleFieldEncoder" && ty_name2 == "CaddyLoggingEncoders"
			} else {
				false
			};
			let ty = mk_type(&field.value.value, is_boxed);
			// Fixes reserved keywords as field names, and dashes in field names
			use heck::SnakeCase;
			let snake_key = field.key.to_snake_case();
			let field_key = &field.key;
			let name_lit = quote::format_ident!("r#{}", &snake_key);
			field_names.push(quote! { #name_lit });
			field_attrs.push(quote! { #[serde(rename = #field_key)] });
			field_tys.push(ty);
			let field_doc = field.value.doc.as_deref().unwrap_or("");
			field_docs.push(if field_doc.is_empty() {
				quote! {}
			} else {
				quote! { #[doc = #field_doc] }
			});
		}
		// Prune empty modules
		let aux_collected = aux.into_iter().fold(quote! {}, |acc, x| quote! { #acc #x });
		(
			quote! {
				{
					#(
						#field_docs
						#field_attrs
						#[serde(skip_serializing_if = "Option::is_none")]
						pub #field_names: ::std::option::Option<crate::Identifiable<#field_tys>>
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
	ty_names_set: &mut HashSet<String>,
) -> Generated {
	let ty_name = rename_ty
		.map(|x| x.into())
		.unwrap_or_else(|| mk_type_name(&config.value));
	let ty_name_lit = quote::format_ident!("{}", ty_name);
	let ty_name_lit = quote! { #ty_name_lit };
	// Prune empty docs
	let doc = config.doc.as_deref().unwrap_or("");
	let doc = if doc.is_empty() {
		quote! {}
	} else {
		quote! { #[doc = #doc] }
	};
	let (block, aux) = mk_struct_block(config, ty_names_set);
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
		// Special cased in mk_ty
		Struct { type_name, .. } if type_name.to_lowercase().ends_with("bigint") => {
			(quote! {}, quote! {})
		}
		Struct { .. } => {
			let ty_name = mk_type_name(&config.value);
			let has_ty = ty_names_set.contains(&*ty_name);
			if has_ty {
				// Don't repeat definitions
				(quote! {}, quote! {})
			} else {
				ty_names_set.insert(ty_name.to_string());
				mk_struct_def(config, None, ty_names_set)
			}
		}
		// Modules and module maps should be defined by top level, which don't use this function to generate definitions
		Module { .. } | ModuleMap { .. } => (quote! {}, quote! {}),
		// Primitive types don't need defs (already defined in std)
		Bool | String | Int | Float => (quote! {}, quote! {}),
		// Arrays / maps need definitions of their inner types (which goes in aux)
		Array { elems } => {
			let (inner_def, inner_aux) = mk_def(elems, ty_names_set);
			(quote! {}, quote! { #inner_def #inner_aux })
		}
		Map { map_keys, elems } => {
			let (keys_def, keys_aux) = mk_def(map_keys, ty_names_set);
			let (val_def, val_aux) = mk_def(elems, ty_names_set);
			(quote! {}, quote! { #keys_def #keys_aux #val_def #val_aux })
		}
	}
}

#[allow(clippy::enum_variant_names)]
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
	let config = ConfigTypeValue {
		// FIXME: Modules don't have docs
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
	mk_struct_def(&config, None, ty_names_set)
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
	// We don't need the struct, just the aux definitions
	let (_, aux) = mk_module_aux(name, values, ty_names_set);
	let mut variants = Vec::new();
	for (field_name, field_config) in &values.values {
		let camel_case_field_name = field_name.to_camel_case();
		let camel_case_field_name_lit = quote::format_ident!("{}", camel_case_field_name);
		let camel_case_field_name_lit = quote! { #camel_case_field_name_lit };
		// Use type as inner value
		let field_ty = mk_type(&field_config.value, false);
		variants.push(quote! {
			#camel_case_field_name_lit(#field_ty)
		});
	}
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
	// This function uses both outputs, not just the aux
	mk_module_aux(name, values, ty_names_set)
}

fn mk_toplevel(
	structure: &ConfigTypeValue,
	namespace_defs: &HashMap<String, NamespaceDef>,
) -> TokenStream {
	let mut ty_names_set = HashSet::new();
	let (struct_def, struct_defs_aux) = mk_struct_def(structure, None, &mut ty_names_set);
	let mut module_defs = vec![];
	let mut module_defs_aux = vec![];
	for (name, value) in namespace_defs {
		if value.defn_types.module_map() {
			let (map_def, map_def_aux) = mk_module_map_def(name, value, &mut ty_names_set);
			module_defs.push(map_def);
			module_defs_aux.push(map_def_aux);
		}
		if value.defn_types.module() {
			let (module_def, module_def_aux) = mk_module_def(name, value, &mut ty_names_set);
			module_defs.push(module_def);
			// If we are generating a module map as well, the aux definitions are already there
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
			let tag_ident = input.parse::<Ident>()?;
			Some(tag_ident.to_string())
		} else {
			None
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
	fn into_def(self) -> Result<NamespaceDef, SynError> {
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
	fn into_structure(self) -> Result<(ConfigTypeValue, HashMap<String, NamespaceDef>), SynError> {
		let root_config = self.root.read_config()?;
		let mut namespaces = HashMap::new();
		for (k, v) in self.namespaces.into_iter() {
			namespaces.insert(k, v.into_def()?);
		}
		Ok((root_config, namespaces))
	}
}

#[proc_macro]
pub fn caddy_types(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
	let input_parsed = syn::parse_macro_input!(input as CaddyTypesInput);
	let (cfg, namespaces) = match input_parsed.into_structure() {
		Ok(s) => s,
		Err(e) => return e.into_compile_error().into(),
	};
	let out = mk_toplevel(&cfg, &namespaces);
	out.into()
}
