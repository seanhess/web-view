
export type UrlFragment = string

export function listenClickAction(cb:(target:HTMLElement, action:UrlFragment) => void): void {
  document.addEventListener("click", function(e) {
    let el = e.target as HTMLInputElement

    // Find the nearest source that has a click handler
    let source:HTMLElement = el.closest("[data-action]");
    let target:HTMLElement = el.closest("[data-target]");

    if (source?.dataset.action && target?.dataset.target) {
      cb(target, source.dataset.action)
    }
  })
}

export function targetId(el:HTMLElement): string {
  return el.dataset.target
}
