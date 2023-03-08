import { defineConfig } from 'vite'
import react from '@vitejs/plugin-react'
import {plugin as elm} from 'vite-plugin-elm'

// https://vitejs.dev/config/
export default defineConfig({
  plugins: [react(), elm({ debug: true })],
})
