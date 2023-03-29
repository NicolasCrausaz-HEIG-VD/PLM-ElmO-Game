/// <reference types="vite/client" />
// .elm files are not recognized by default
declare module "*.elm" {

  type Port = {
    [portName: string]: {
      subscribe?: (handler: (value?: any) => void) => void
      send?: (value?: any) => void
    }
  }

  type ElmMain<Flags> = {
    init: <P extends Port>(flags: Flags) => ElmApp<P>
  }

  export const Elm: {
    [moduleName: string]: ElmMain<unknown>
  }
}
