import { defineConfig } from 'vite'
import {plugin as elm} from 'vite-plugin-elm'

// https://vitejs.dev/config/
export default defineConfig(({ mode }) => {
  const isDev = mode === 'development';

  return {
    plugins: [elm({ debug: isDev })],
  };
});