import { defineConfig } from 'vite'
import {plugin as elm} from 'vite-plugin-elm'
import { VitePWA } from 'vite-plugin-pwa'
// https://vitejs.dev/config/
export default defineConfig(({ mode }) => {
  const isDev = mode === 'development';

  return {
    plugins: [
      VitePWA({ registerType: 'autoUpdate' }),
      elm({ debug: isDev })
    ],
  };
});