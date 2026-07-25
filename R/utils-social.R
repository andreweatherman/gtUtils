# internal fontawesome helpers for gt_social_tag(). not exported

# friendly aliases -> fontawesome brand icon names
.social_aliases <- c(
  x = "x-twitter", twitter = "x-twitter",
  ig = "instagram", instagram = "instagram",
  bsky = "bluesky", bluesky = "bluesky",
  threads = "threads",
  gh = "github", github = "github",
  linkedin = "linkedin",
  yt = "youtube", youtube = "youtube",
  tiktok = "tiktok", mastodon = "mastodon",
  fb = "facebook", facebook = "facebook",
  twitch = "twitch", substack = "substack",
  web = "globe", website = "globe", link = "globe",
  email = "envelope", mail = "envelope"
)

# one icon -> svg string, or abort with something useful
.social_icon <- function(icon, fill, height) {
  tryCatch(
    as.character(fontawesome::fa(icon, fill = fill, height = height)),
    error = function(e) {
      cli::cli_abort(c(
        "Icon {.val {icon}} was not found in your installed {.pkg fontawesome} \\
         ({.val {as.character(utils::packageVersion('fontawesome'))}}).",
        "i" = "Update {.pkg fontawesome} to a version that includes it, or use a \\
               different icon/alias."
      ), call = rlang::caller_env())
    }
  )
}

# build "<icon> handle" spans from a named accounts vector
.social_items <- function(accounts, fill = "currentColor", icon_height = "0.9em") {
  vapply(seq_along(accounts), function(i) {
    key <- names(accounts)[[i]]
    handle <- accounts[[i]]
    icon <- if (tolower(key) %in% names(.social_aliases)) .social_aliases[[tolower(key)]] else key
    sprintf(
      "<span style='display:inline-flex; align-items:center; gap:0.3em; white-space:nowrap;'>%s%s</span>",
      .social_icon(icon, fill, icon_height), handle
    )
  }, character(1))
}
