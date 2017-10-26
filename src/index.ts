const fuse = require('fuse-bindings');
const fs = require('fs');
const path = require('path');

const mountPath = process.argv[2];
const sourceFile = process.argv[3];
const sourceName = path.basename(sourceFile);

const browserify = require('browserify');
let result = null;
let mtime = new Date(0);

function compile (cb) {
  const m = fs.statSync(sourceFile).mtime;

  if (result && m === mtime) {
    return cb(result);
  }

  const b = browserify();

  b.add(sourceFile);

  b.bundle((err, buffer) => {
    if (err) { throw err; }
    result = buffer;
    mtime = m;
    cb(buffer);
  });
}

fuse.mount(mountPath, {
  readdir: function (path, cb) {
    //console.log('readdir(%s)', path)
    if (path === '/') return cb(0, [sourceName])
    cb(0)
  },
  getattr: function (path, cb) {
    //console.log('getattr(%s)', path)
    if (path === '/') {
      cb(0, {
        mtime: new Date(),
        atime: new Date(),
        ctime: new Date(),
        nlink: 1,
        size: 100,
        mode: 16877,
        uid: process.getuid ? process.getuid() : 0,
        gid: process.getgid ? process.getgid() : 0
      })
    }

    if (path === '/' + sourceName) {
      compile((buf) => {
        cb(0, {
          mtime: new Date(mtime),
          atime: new Date(mtime),
          ctime: new Date(mtime),
          size: buf.length,
          mode: 33188,
          uid: process.getuid ? process.getuid() : 0,
          gid: process.getgid ? process.getgid() : 0
        })
      });
    }

    cb(fuse.ENOENT)
  },
  open: function (path, flags, cb) {
    //console.log('open(%s, %d)', path, flags)
    cb(0, 42) // 42 is an fd
  },
  read: function (path, fd, buf, len, pos, cb) {
    //console.log('read(%s, %d, %d, %d)', path, fd, len, pos)
    compile((result) => {
      var part = result.slice(pos);
      if (part.length === 0) return cb(0)
      part.copy(buf);

      return cb(part.length)
    });
  }
}, function (err) {
  if (err) throw err
  console.log('filesystem mounted on ' + mountPath)
})

process.on('SIGINT', function () {
  fuse.unmount(mountPath, function (err) {
    if (err) {
      console.log('filesystem at ' + mountPath + ' not unmounted', err)
    } else {
      console.log('filesystem at ' + mountPath + ' unmounted')
    }
  })
})
