
export type UrlFragment = string

export function listenClickAction(cb:(action:UrlFragment) => void): void {
  document.addEventListener("click", function(e) {
    let el = e.target as HTMLInputElement

    // Find the nearest source that has a click handler
    let source:HTMLElement = el.closest("[data-action]");

    if (source?.dataset.action) {
      cb(source.dataset.action)
    }
  })
}

