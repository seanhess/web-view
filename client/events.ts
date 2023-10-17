
export type UrlFragment = string

export function listenClick(cb:(target:HTMLElement, action:string) => void): void {
  document.addEventListener("click", function(e) {
    let source = e.target as HTMLInputElement

    // they should all have an action and target
    if (source?.dataset.action && source?.dataset.target) {
      let target = document.getElementById(source.dataset.target)

      if (!target) {
        console.error("Could not find target: ", source.dataset.target)
        return
      }

      e.preventDefault()
      cb(target, source.dataset.action)
    }
  })
}

export function listenFormSubmit(cb:(target:HTMLElement, action:string, form:FormData) => void): void {
  document.addEventListener("submit", function(e) {
    let form = e.target as HTMLFormElement

    // they should all have an action and target
    if (form?.dataset.action && form?.dataset.target) {
      let target = document.getElementById(form.dataset.target)

      if (!target) {
        console.error("Could not find target: ", form.dataset.target)
        return
      }

      e.preventDefault()
      const formData = new FormData(form)
      cb(target, form.dataset.action, formData)
    }
  })
}
