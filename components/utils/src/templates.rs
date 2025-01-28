use std::{
    collections::HashMap,
    io::Write,
    process::{Command, Stdio},
    sync::LazyLock,
};

use libs::{
    regex::Regex,
    tera::{Context, Tera},
};

use errors::{bail, ensure, Result};

static DEFAULT_TPL: &str = include_str!("default_tpl.html");

macro_rules! render_default_tpl {
    ($filename: expr, $url: expr) => {{
        let mut context = Context::new();
        context.insert("filename", $filename);
        context.insert("url", $url);
        Tera::one_off(DEFAULT_TPL, &context, true).map_err(std::convert::Into::into)
    }};
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ShortcodeFileType {
    Markdown,
    Html,
}

#[derive(Debug, Clone)]
pub struct ShortcodeDefinition {
    pub file_type: ShortcodeFileType,
    pub tera_name: String,
}
impl ShortcodeDefinition {
    pub fn new(file_type: ShortcodeFileType, tera_name: &str) -> ShortcodeDefinition {
        let tera_name = tera_name.to_string();

        ShortcodeDefinition { file_type, tera_name }
    }
}

/// Fetches all the shortcodes from the Tera instances
pub fn get_shortcodes(tera: &Tera) -> HashMap<String, ShortcodeDefinition> {
    let mut shortcode_definitions = HashMap::new();

    for (identifier, template) in tera.templates.iter() {
        let (file_type, ext_len) = if template.name.ends_with(".md") {
            (ShortcodeFileType::Markdown, "md".len())
        } else {
            (ShortcodeFileType::Html, "html".len())
        };

        if template.name.starts_with("shortcodes/") {
            let head_len = "shortcodes/".len();
            shortcode_definitions.insert(
                identifier[head_len..(identifier.len() - ext_len - 1)].to_string(),
                ShortcodeDefinition::new(file_type, &template.name),
            );
            continue;
        }

        if template.name.starts_with("__zola_builtins/shortcodes/") {
            let head_len = "__zola_builtins/shortcodes/".len();
            let name = &identifier[head_len..(identifier.len() - ext_len - 1)];
            // We don't keep the built-ins one if the user provided one
            if shortcode_definitions.contains_key(name) {
                continue;
            }
            shortcode_definitions
                .insert(name.to_string(), ShortcodeDefinition::new(file_type, &template.name));
        }
    }

    shortcode_definitions
}

/// Renders the given template with the given context, but also ensures that, if the default file
/// is not found, it will look up for the equivalent template for the current theme if there is one.
/// Lastly, if it's a default template (index, section or page), it will just return an empty string
/// to avoid an error if there isn't a template with that name
pub fn render_template(
    name: &str,
    tera: &Tera,
    context: Context,
    theme: &Option<String>,
) -> Result<String> {
    let rendered = if let Some(template) = check_template_fallbacks(name, tera, theme) {
        tera.render(template, &context)?
    } else {
        // maybe it's a default one?
        let x: Result<String> = match name {
            "index.html" | "section.html" => render_default_tpl!(
                name,
                "https://www.getzola.org/documentation/templates/pages-sections/#section-variables"
            ),
            "page.html" => render_default_tpl!(
                name,
                "https://www.getzola.org/documentation/templates/pages-sections/#page-variables"
            ),
            "single.html" | "list.html" => render_default_tpl!(
                name,
                "https://www.getzola.org/documentation/templates/taxonomies/"
            ),
            _ => bail!("Tried to render `{}` but the template wasn't found", name),
        };

        x?
    };

    Ok(rendered)
}

pub fn render_typst(mut rendered: String) -> Result<String> {
    static REGEX: LazyLock<Regex> = LazyLock::new(|| Regex::new(r"(?s)\$\$.*?\$\$").unwrap());
    static TYPST: LazyLock<String> =
        LazyLock::new(|| std::env::var("TYPST_BINARY").unwrap_or_else(|_| "typst".to_string()));

    while let Some(match_) = REGEX.find(&rendered) {
        // println!("Found match: {:?}", match_);

        let process = Command::new(&*TYPST)
            .arg("c")
            .args(["-f", "svg"])
            .arg("-")
            .arg("-")
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .spawn()?;

        process.stdin.as_ref().unwrap().write_all(
            match_
                .as_str()
                .strip_prefix("$$")
                .unwrap()
                .strip_suffix("$$")
                .unwrap()
                .trim()
                .as_bytes(),
        )?;

        let output = process.wait_with_output()?;

        if output.status.success() {
            let svg = String::from_utf8(output.stdout)?;
            rendered.replace_range(
                match_.range(),
                &format!(r#"<div class="typst-document">{svg}</div>"#),
            );
        } else {
            rendered.replace_range(
                match_.range(),
                &format!(
                    r#"<div style="font-size: 50pt; color: red;">failed to render</div>"#
                ),
            );
        }

    }

    Ok(rendered)
}

/// Rewrites the path of duplicate templates to include the complete theme path
/// Theme templates  will be injected into site templates, with higher priority for site
/// templates. To keep a copy of the template in case it's being extended from a site template
/// of the same name, we reinsert it with the theme path prepended
pub fn rewrite_theme_paths(tera_theme: &mut Tera, theme: &str) {
    let theme_basepath = format!("{}/templates/", theme);
    let mut new_templates = HashMap::new();
    for (key, template) in &tera_theme.templates {
        let mut tpl = template.clone();
        tpl.name = format!("{}{}", theme_basepath, key);
        new_templates.insert(tpl.name.clone(), tpl);
    }
    // Contrary to tera.extend, hashmap.extend does replace existing keys
    // We can safely extend because there's no conflicting paths anymore
    tera_theme.templates.extend(new_templates);
}

/// Checks for the presence of a given template. If none is found, also looks for a
/// fallback in theme and default templates. Returns the path of the most specific
/// template found, or none if none are present.
pub fn check_template_fallbacks<'a>(
    name: &'a str,
    tera: &'a Tera,
    theme: &Option<String>,
) -> Option<&'a str> {
    // check if it is in the templates
    if tera.templates.contains_key(name) {
        return Some(name);
    }

    // check if it is part of a theme
    if let Some(ref t) = *theme {
        let theme_template_name = format!("{}/templates/{}", t, name);
        if let Some((key, _)) = tera.templates.get_key_value(&theme_template_name) {
            return Some(key);
        }
    }

    // check if it is part of ZOLA_TERA defaults
    let default_name = format!("__zola_builtins/{}", name);
    if let Some((key, _)) = tera.templates.get_key_value(&default_name) {
        return Some(key);
    }

    None
}

#[cfg(test)]
mod tests {
    use crate::templates::{check_template_fallbacks, get_shortcodes};

    use super::rewrite_theme_paths;
    use libs::tera::Tera;

    #[test]
    fn can_rewrite_all_paths_of_theme() {
        let mut tera = Tera::parse("test-templates/*.html").unwrap();
        rewrite_theme_paths(&mut tera, "hyde");
        // special case to make the test work: we also rename the files to
        // match the imports
        for (key, val) in &tera.templates.clone() {
            tera.templates.insert(format!("hyde/templates/{}", key), val.clone());
        }
        // Adding our fake base
        tera.add_raw_template("base.html", "Hello").unwrap();
        tera.build_inheritance_chains().unwrap();

        assert_eq!(
            tera.templates["hyde/templates/index.html"].parent,
            Some("base.html".to_string())
        );
        assert_eq!(
            tera.templates["hyde/templates/child.html"].parent,
            Some("index.html".to_string())
        );
    }

    #[test]
    fn template_fallback_is_successful() {
        let mut tera = Tera::parse("test-templates/*.html").unwrap();
        tera.add_raw_template("hyde/templates/index.html", "Hello").unwrap();
        tera.add_raw_template("hyde/templates/theme-only.html", "Hello").unwrap();

        // Check finding existing template
        assert_eq!(check_template_fallbacks("index.html", &tera, &None), Some("index.html"));

        // Check trying to find non-existant template
        assert_eq!(check_template_fallbacks("not-here.html", &tera, &None), None);

        // Check theme fallback
        assert_eq!(
            check_template_fallbacks("theme-only.html", &tera, &Some("hyde".to_string())),
            Some("hyde/templates/theme-only.html")
        );
    }

    #[test]
    fn can_overwrite_builtin_shortcodes() {
        let mut tera = Tera::parse("test-templates/*.html").unwrap();
        tera.add_raw_template("__zola_builtins/shortcodes/youtube.html", "Builtin").unwrap();
        tera.add_raw_template("shortcodes/youtube.html", "Hello").unwrap();
        let definitions = get_shortcodes(&tera);
        assert_eq!(definitions["youtube"].tera_name, "shortcodes/youtube.html");
    }
}
