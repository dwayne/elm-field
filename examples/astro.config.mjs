import { defineConfig } from "astro/config";
import elmPlugin from "vite-plugin-elm";

// https://astro.build/config
export default defineConfig({
  base: "/elm-field",
  vite: {
    plugins: [
      elmPlugin()
    ]
  }
});
