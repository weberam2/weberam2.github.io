rmarkdown::render_site()
#file.rename(from = "docs/index.html", to = "docs/tmp")
#shinylive::export(appdir = "appfiles", destdir = "docs")
#file.rename(from = "docs/index.html", to = "docs/futureclimbs.html")
#file.rename(from = "docs/tmp", to = "docs/index.html")

post_files <- list.files("blog_posts", pattern = "*.Rmd", full.names = TRUE)
for(f in post_files) {
  rmarkdown::render(f, output_dir = "docs/blog_posts")
}
