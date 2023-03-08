/// <reference types="vite/client" />
// .elm files are not recognized by default
declare module "*.elm" {
  export const Elm: {
    [moduleName: string]: ElmMain<unknown>
  }
}
