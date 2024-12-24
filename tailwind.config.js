/** @type {import('tailwindcss').Config} */
module.exports = {
  content: (process.env.NODE_ENV == 'production' ? ["./public/js/main.js"] : ["./src/main/**/*.cljs", "./public/js/cljs-runtime/*.js"]),
  theme: {
    extend: {},
  },
  plugins: [
    require("@tailwindcss/forms")
  ],
}

