import { defineConfig } from "vite";
import elm from 'vite-plugin-elm-watch';

export default defineConfig({
  plugins: [elm({ mode: process.env.NODE_ENV === "production" ? 'minify' : 'standard' })]
})