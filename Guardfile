guard :haskell, all_on_start: true, ghci_options: ["-ignore-dot-ghci", "-DTEST"] do
  watch(%r{test/.+Spec\.l?hs$})
  watch(%r{src/.+\.l?hs$})
end
