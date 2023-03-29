import { defineConfig } from 'vite'
import {plugin as elm} from 'vite-plugin-elm'

// https://vitejs.dev/config/
export default defineConfig({
  plugins: [elm({ debug: true })],
})
