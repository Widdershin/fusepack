# fusepack
A build system powered by Fuse

Usage
----

Say we have a source file, `index.js`, with the following contents:

```
console.log('hi!');
```

First, create a `Fusepack` file.

```
build/

index.js -> browserify - -> bundle.js
styles.css -> postcss -> bundle.css
src/index.ts
```

Now, open `build/bundle.js`. You should see the browserified output of `index.js`.

If you update `index.js`, and reopen `bundle.js`, it will update!
