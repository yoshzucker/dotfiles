## Set locale to ensure consistent date/time formats
Sys.setlocale("LC_TIME", "en_US.UTF-8")

## Set CRAN mirror
options(repos = "https://cran.rstudio.com")

## Use quartz device on macOS
if (capabilities("aqua")) {
  options(device = "quartz")
}

## Hook for graphics device options after grDevices loads
setHook(packageEvent("grDevices", "onLoad"), function(...) {
  if (capabilities("aqua")) {
    grDevices::quartz.options(width = 5, height = 5)
  } else if (capabilities("X11")) {
    ## e.g., grDevices::X11.options(width = 6, height = 4)
  }
})
