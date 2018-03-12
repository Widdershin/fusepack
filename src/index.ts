const fuse = require('fuse-bindings');
const fs = require('fs');
const path = require('path');
const child = require('child_process');

const browserify = require('browserify');
const results = {};
const mtimes = {};

const packFile = fs.readFileSync(path.join(process.cwd(), 'Fusepack'), 'utf-8');

function parseInstruction (line) {
  const parts = line.split('->').map(part => part.trim());

  if (parts.length < 2) {
    throw new Error(`Could not parse line ${line}`);
  }

  const source = parts[0];
  let output = parts[parts.length - 1];
  const commands = parts.slice(1, -1);

  return {source, output, commands};
}

function readPackFile (str) {
  const lines = str.split('\n').filter(line => line.trim() !== '');
  const directory = lines[0];
  const instructions = lines.slice(1).map(parseInstruction);

  return {
    directory,
    instructions,
    instructionsBySource: indexBy(instructions, (i: any) => i.source),
    instructionsByOutput: indexBy(instructions, (i: any) => '/' + i.output),
  }
}

const config = readPackFile(packFile);

function indexBy<T>(array: T[], indexSelector: (t: T) => string): {[key: string]: T} {
  const result: {[key: string]: T} = {};

  array.forEach(item => result[indexSelector(item)] = item);

  return result;
}

function compile (instruction, cb) {
  const m = fs.statSync(instruction.source).mtime;

  if (results[instruction.output] && m.getTime() === mtimes[instruction.output].getTime()) {
    return cb(results[instruction.output]);
  }

  console.log('compiling: ', instruction.source, '->', instruction.commands.join(' -> '), '->', path.join(config.directory, instruction.output));
  const command = `cat ${instruction.source} | ${instruction.commands.join(' | ')}`
  child.exec(command, {maxBuffer: 1024 * 1024, encoding: 'buffer'}, (err, stdout, stderr) => {
    if (err) console.error(err.message);

    results[instruction.output] = stdout;
    mtimes[instruction.output] = m;
    cb(stdout);
  });
}

try {
  fs.mkdirSync(config.directory);
} catch (e) {}

fuse.mount(config.directory, {
  readdir: function (path, cb) {
    //console.log('readdir(%s)', path)
    if (path === '/') return cb(0, config.instructions.map(i => i.output))
    cb(0)
  },
  getattr: function (path, cb) {
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
      return;
    }

    const instruction = config.instructionsByOutput[path] as any;
    if (instruction) {
      const mtime = mtimes[instruction.output];
      const date = mtime || new Date(0);

      compile(instruction, (buf) => {
        cb(0, {
          mtime: date,
          atime: date,
          ctime: date,
          size: buf.length,
          mode: 33188,
          uid: process.getuid ? process.getuid() : 0,
          gid: process.getgid ? process.getgid() : 0
        })
      });
      return;
    }

    cb(fuse.ENOENT)
  },
  open: function (path, flags, cb) {
    //console.log('open(%s, %d)', path, flags)
    cb(0, 42) // 42 is an fd
  },
  read: function (path, fd, buf, len, pos, cb) {
    //console.log('read(%s, %d, %d, %d)', path, fd, len, pos)
    const instruction = config.instructionsByOutput[path] as any;

    compile(instruction, (result) => {
      var part = result.slice(pos);
      if (part.length > 1024) {
        part = part.slice(0, 1024);
      }

      if (part.length === 0) return cb(0)
      part.copy(buf);

      return cb(part.length)
    });
  }
}, function (err) {
  if (err) throw err
  //console.log('fusepacking to' + config.directory)
})

function cleanup () {
  fuse.unmount(config.directory, function (err) {
    if (err) {
      console.log('filesystem at ' + config.directory + ' not unmounted', err)
    } else {
      console.log('filesystem at ' + config.directory + ' unmounted')
    }
  })
}

process.on('SIGINT', cleanup);

process.on('uncaughtException', cleanup);
