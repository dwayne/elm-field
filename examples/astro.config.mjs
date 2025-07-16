import { defineConfig } from "astro/config";
import elmPlugin from "vite-plugin-elm";

// https://astro.build/config
export default defineConfig({
  vite: {
    plugins: [
      elmPlugin()
    ]
  }
});
