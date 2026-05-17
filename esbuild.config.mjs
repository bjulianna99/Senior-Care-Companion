import { cpSync, readdirSync, existsSync } from 'fs'
import { build } from 'esbuild'

cpSync('./build/', './wwwroot/Scripts/', { recursive: true });

const prebundles = readdirSync('./build/');

prebundles.forEach(file => {
  if (file.endsWith('.js')) {
    var options =
    {
      entryPoints: ['./build/' + file],
      bundle: true,
      minify: true,
      format: 'iife',
      outfile: 'wwwroot/Scripts/' + file,
      globalName: 'wsbundle',
      loader: {
            ".png": "file",
            ".jpg": "file",
            ".jpeg": "file",
            ".svg": "file"
      }
    };

    console.log("Bundling:", file);
    build(options);
  }
});

if (existsSync('./build/workers/')) {
  const workers = readdirSync('./build/workers/');

  workers.forEach(file => {
    if (file.endsWith('.js')) {
      var options =
      {
        entryPoints: ['./build/workers/' + file],
        bundle: true,
        minify: true,
        format: 'iife',
        outfile: 'wwwroot/Scripts/workers/' + file,
        loader: {
            ".png": "file",
            ".jpg": "file",
            ".jpeg": "file",
            ".svg": "file"
        }
      };

      console.log("Bundling worker:", file);
      build(options);
    }
  });
}