# Tiny generator script (run on each version bump)
# Creates useful stuff for review

dir.create("dev/structure", recursive = TRUE, showWarnings = FALSE)

pkg <- basename(normalizePath("."))
ns  <- getNamespace(pkg)

exports   <- sort(getNamespaceExports(pkg))
all_syms  <- sort(ls(ns, all.names = TRUE))
internals <- setdiff(all_syms, exports)

writeLines(exports,   "dev/structure/exports.txt")
writeLines(internals, "dev/structure/internals.txt")

if (requireNamespace("pkgnet", quietly = TRUE)) {
    pkgnet::CreatePackageReport(
        report_path = "dev/structure/pkgnet_report.html",
        pkg_path = getwd(),
        pkg_name = "gptr"
    )
}
