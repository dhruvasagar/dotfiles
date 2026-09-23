//! The adapter between `#[derive(ConfigShape)]` and this config's generated
//! WIT bindings — written once, not per option.
//!
//! Design: `docs/dev/architecture/typed-configuration.md` in the lattice repo;
//! `docs/user/init.md` for how a config uses it.
//!
//! `lattice-plugin-sdk` is deliberately WIT-agnostic: a proc-macro crate cannot
//! name a per-world WIT type, which is why `#[derive(PluginOption)]` hands back
//! an `OptionKind` you map at the call site. `#[derive(ConfigShape)]` is the
//! same, so this file is that mapping.
//!
//! What it also hides is the ARENA. WIT has no recursive types, so a config
//! value crosses as a flat node list plus the index of its root. The SDK
//! flattens; this renames the nodes. Nothing here should ever be written by
//! hand.
//!
//! Only the WRITE direction is here, because setting options is all an
//! `init.rs` does — a config declares no options of its own. The SDK carries
//! the read and register directions for a plugin that needs them.

use lattice_plugin_sdk::shape::{self, ConfigShape, Value, flatten_value};

use crate::lattice::plugin_host::config;

/// Set an option from a value built out of ordinary Rust.
///
/// The structured peer of `config::set_option`, for an option whose value has
/// shape — org's capture templates, say. `false` when the host refused: an
/// unknown option, or a value that does not fit the schema the plugin declared,
/// which the host reports with a path (`[2].target.file: expected string, got
/// integer`) in the log.
pub fn set_option_value<T: ConfigShape>(name: &str, value: &T) -> bool {
    config::set_option_value(name, &to_wit(&value.to_value()))
}

fn to_wit(value: &Value) -> config::ConfigValue {
    let (nodes, root) = flatten_value(value);
    config::ConfigValue {
        nodes: nodes.iter().map(node_to_wit).collect(),
        root,
    }
}

fn node_to_wit(node: &shape::ValueNode) -> config::ValueNode {
    match node {
        shape::ValueNode::Bool(b) => config::ValueNode::Bool(*b),
        shape::ValueNode::Int(i) => config::ValueNode::Int(*i),
        shape::ValueNode::Str(s) => config::ValueNode::String(s.clone()),
        shape::ValueNode::List(children) => config::ValueNode::List(children.clone()),
        shape::ValueNode::Record(fields) => config::ValueNode::Record(fields.clone()),
    }
}
