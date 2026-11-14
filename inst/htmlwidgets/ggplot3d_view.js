(function () {
  'use strict';
  const rad = Math.PI / 180;
  const dot = (a, b) => a.reduce((s, v, i) => s + v * b[i], 0);
  const sub = (a, b) => a.map((v, i) => v - b[i]);
  const norm = a => { const n = Math.hypot(...a); return a.map(v => v / (n || 1)); };
  const cross = (a, b) => [a[1]*b[2]-a[2]*b[1], a[2]*b[0]-a[0]*b[2], a[0]*b[1]-a[1]*b[0]];
  const arr = x => x == null ? [] : Array.isArray(x) ? x : [x];
  const css = c => 'rgba(' + Array.from(c).slice(0, 3).map(v => Math.round(v*255)).join(',') + ',' + c[3] + ')';
  const clone = x => JSON.parse(JSON.stringify(x));
  // Viewers in one link group share every orbit, zoom and reset: angle
  // changes add and zoom factors multiply, so each keeps its offset.
  const linkGroups = new Map();
  const FACE = 0, STROKE = 1, MARKER = 2, TEXT = 3, DISC = 4, KINDS = [FACE, STROKE, MARKER, TEXT, DISC];
  const DRAW_KINDS = [FACE, STROKE, MARKER, DISC];
  const INSTANCED = new Set([DISC]);
  const MAX_SHEET_PASSES = 32;

  // Table columns arrive as base64 little-endian typed arrays.
  function column(c) {
    if (c == null || typeof c !== 'object' || Array.isArray(c) || c.b64 == null) return arr(c);
    const s = atob(c.b64), bytes = new Uint8Array(s.length);
    for (let i = 0; i < s.length; i++) bytes[i] = s.charCodeAt(i);
    if (c.type === 'f32') return new Float32Array(bytes.buffer);
    if (c.type === 'i32') return new Int32Array(bytes.buffer);
    return bytes;
  }

  function cameraBasis(c, vp, framing) {
    const scale = c.aspect.map(v => v/Math.max(...c.aspect));
    const extent = scale.map((v, i) => v*Math.max(Math.abs(c.target[i]), Math.abs(1-c.target[i])));
    const radius = Math.hypot(...extent), az = c.azimuth*rad, el = c.elevation*rad, roll = c.roll*rad;
    // Angles turn about the vertical axis; a cyclic permutation carries the
    // z-up frame onto it. An oblique shear tilts the sight ray.
    const axes = [0, 1, 2].map(k => (k + (c.up || 3)) % 3);
    const place = v => { const w = [0, 0, 0]; v.forEach((x, k) => { w[axes[k]] = x; }); return w; };
    const r = place([-Math.sin(az), Math.cos(az), 0]);
    const u = place([-Math.sin(el)*Math.cos(az), -Math.sin(el)*Math.sin(az), Math.cos(el)]);
    const orthoRight = r.map((v, i) => Math.cos(roll)*v-Math.sin(roll)*u[i]);
    const orthoUp = r.map((v, i) => Math.sin(roll)*v+Math.cos(roll)*u[i]);
    const forward = place([-Math.cos(el)*Math.cos(az), -Math.cos(el)*Math.sin(az), -Math.sin(el)]);
    const shear = arr(c.shear).length === 2 ? arr(c.shear) : [0, 0];
    const right = orthoRight.map((v, i) => v+shear[0]*forward[i]);
    const up = orthoUp.map((v, i) => v+shear[1]*forward[i]);
    const ray = forward.map((v, i) => v-shear[0]*orthoRight[i]-shear[1]*orthoUp[i]);
    const perspective = c.projection === 'perspective', ratio = vp.width_mm/vp.height_mm;
    const tolerance = 64*Number.EPSILON*Math.max(1, radius);
    const distance = perspective ? (c.distance == null ? radius/Math.sin(Math.atan(Math.tan(c.fov*rad/2)*Math.min(1, ratio)))+tolerance : c.distance) : 2*radius+1;
    const base = (perspective ? 1/Math.tan(c.fov*rad/2) : Math.min(1, ratio)/radius)*c.zoom*framing.zoom_scale;
    return {scale, radius, right, up, forward, ray, orthoRight, orthoUp, distance, perspective, base,
      near: c.near == null ? Math.max(tolerance, distance-radius-tolerance) : c.near,
      far: c.far == null ? distance+radius+tolerance : c.far};
  }

  // Shaders. Every kind projects with the static backends' camera model and
  // writes either its colour or, with PICK defined, its primitive id.
  const PROJECT = `
    uniform vec3 uTarget, uScale, uRight, uUp, uForward;
    uniform float uDistance, uNear, uFar, uBase, uPerspective, uPpm;
    uniform vec2 uSize, uOrigin, uInner;
    const vec4 OFFSCREEN = vec4(2.0, 2.0, 2.0, 1.0);
    vec4 project(vec3 p, out float d) {
      vec3 delta = (p-uTarget)*uScale;
      d = uDistance+dot(delta, uForward);
      float w = mix(1.0, d, uPerspective);
      vec2 xy = vec2(dot(delta, uRight), dot(delta, uUp))*uBase/w;
      xy *= vec2(uInner.y/uInner.x, 1.0)*0.5;
      float z = mix(2.0*(d-uNear)/(uFar-uNear)-1.0,
        (uFar+uNear)/(uFar-uNear)-2.0*uFar*uNear/((uFar-uNear)*d), uPerspective);
      return vec4(uOrigin+(xy+0.5)*uInner, z, w);
    }
    vec4 clipPosition(vec2 px, float z, float w) { return vec4((2.0*px/uSize-1.0)*w, z*w, w); }`;
  const COLOUR_SPACE = `
    vec3 toLinear(vec3 c) { return mix(c/12.92, pow((c+0.055)/1.055, vec3(2.4)), step(vec3(0.04045), c)); }
    vec3 toSrgb(vec3 l) { return mix(12.92*l, 1.055*pow(l, vec3(1.0/2.4))-0.055, step(vec3(0.0031308), l)); }`;
  const FRAGMENT = `
    uniform float uClipData; uniform int uOitPass;
    in vec3 vModel; in vec4 vColour; flat in uint vId; flat in float vClip;
    out vec4 outColour;
    ${COLOUR_SPACE}
    void clipData() {
      if (uClipData > 0.5 && vClip > 0.5 && (any(lessThan(vModel, vec3(0.0))) || any(greaterThan(vModel, vec3(1.0))))) discard;
    }
    void emit(vec4 c) {
    #ifdef PICK
      outColour = vec4(float(vId & 255u), float((vId >> 8) & 255u), float((vId >> 16) & 255u), float((vId >> 24) & 255u))/255.0;
    #else
      if (c.a <= 0.0) discard;
      if (uOitPass == 1) {
        float weight = clamp(pow(c.a+0.01, 3.0)*1000.0*pow(1.0-gl_FragCoord.z*0.9, 3.0), 0.01, 30.0);
        outColour = vec4(toLinear(c.rgb)*c.a, c.a)*weight;
      } else if (uOitPass == 2) outColour = vec4(c.a);
      else outColour = c;
    #endif
    }`;
  const VARYINGS = 'out vec3 vModel; out vec4 vColour; flat out uint vId; flat out float vClip;';

  // Vertex layouts: one interleaved buffer per kind. A per-vertex state
  // stream (opacity factor of selection dimming) and, for strokes, the dash
  // phase at each segment start follow as separate streams. Discs carry
  // their selection state inside the instance record.
  const LAYOUTS = {
    [FACE]: [['aPos', 3, 'f32'], ['aNormal', 3, 'f32'], ['aColour', 4, 'u8n'], ['aId', 1, 'u32'], ['aFlags', 4, 'u8']],
    [STROKE]: [['aA', 3, 'f32'], ['aB', 3, 'f32'], ['aCorner', 4, 'f32'], ['aWidth', 1, 'f32'],
      ['aColour', 4, 'u8n'], ['aId', 1, 'u32'], ['aFlags', 4, 'u8']],
    [MARKER]: [['aPos', 3, 'f32'], ['aOffset', 2, 'f32'], ['aLocal', 2, 'f32'], ['aShape', 3, 'f32'],
      ['aColour', 4, 'u8n'], ['aOutline', 4, 'u8n'], ['aId', 1, 'u32'], ['aFlags', 4, 'u8']],
    [DISC]: [['aPos', 3, 'f32'], ['aAngle', 1, 'f32'], ['aShape', 3, 'f32'],
      ['aColour', 4, 'u8n'], ['aOutline', 4, 'u8n'], ['aId', 1, 'u32'], ['aFlags', 4, 'u8'], ['aState', 1, 'f32']],
    [TEXT]: [['aPos', 3, 'f32'], ['aCorner', 2, 'f32'], ['aBox', 4, 'f32'], ['aAngle', 1, 'f32'], ['aId', 1, 'u32']]
  };
  const STREAMS = {[FACE]: ['aState'], [STROKE]: ['aState', 'aDashStart'], [MARKER]: ['aState'], [TEXT]: ['aState'], [DISC]: []};

  const SHADERS = {
    [FACE]: [`
      in vec3 aPos; in vec3 aNormal; in vec4 aColour; in uint aId; in vec4 aFlags; in float aState;
      uniform float uLight, uAmbient, uDiffuse, uMultiply; uniform vec3 uLightDir, uRay;
      ${VARYINGS}
      ${COLOUR_SPACE}
      void main() {
        float d; vec4 p = project(aPos, d);
        gl_Position = clipPosition(p.xy, p.z, p.w);
        vec4 c = aColour;
        vec3 n = aNormal/uScale;
        if (aFlags.x > 0.5 && uLight > 0.5 && dot(n, n) > 0.0) {
          n = normalize(n);
          if (dot(n, uRay) > 0.0) n = -n;
          float b = uAmbient+uDiffuse*max(0.0, dot(n, uLightDir));
          vec3 l = toLinear(c.rgb);
          l = uMultiply > 0.5 ? l*b : 1.0-(1.0-l)*b;
          c.rgb = clamp(toSrgb(clamp(l, 0.0, 1.0)), 0.0, 1.0);
        }
        c.a *= aState;
        vColour = c; vModel = aPos; vId = aId; vClip = aFlags.y;
      }`, `
      void main() { clipData(); emit(vColour); }`],
    [STROKE]: [`
      in vec3 aA; in vec3 aB; in vec4 aCorner; in float aWidth; in vec4 aColour; in uint aId; in vec4 aFlags;
      in float aState; in float aDashStart;
      ${VARYINGS}
      flat out vec4 vSegment; flat out vec4 vCaps; flat out float vDashStart;
      void main() {
        float da, db; vec4 pa = project(aA, da), pb = project(aB, db);
        vec2 dir = pb.xy-pa.xy; float len = length(dir);
        if ((uPerspective > 0.5 && (da <= 0.0 || db <= 0.0)) || len < 1e-6) { gl_Position = OFFSCREEN; return; }
        dir /= len;
        vec2 n = vec2(-dir.y, dir.x);
        float hw = max(0.5, aWidth*uPpm*0.5);
        bool atEnd = aCorner.y > 0.5;
        float cap = atEnd ? aCorner.w : aCorner.z;
        vec4 q = atEnd ? pb : pa;
        vec2 px = q.xy+(atEnd ? dir : -dir)*(cap > 0.5 ? hw : 0.0)+n*aCorner.x*hw;
        gl_Position = clipPosition(px, q.z, q.w);
        vColour = vec4(aColour.rgb, aColour.a*aState);
        vModel = atEnd ? aB : aA; vId = aId; vClip = aFlags.y;
        vSegment = vec4(pa.xy, pb.xy); vCaps = vec4(aCorner.zw, hw, aFlags.z); vDashStart = aDashStart;
      }`, `
      uniform float uRatio, uPpm; uniform highp sampler2D uDash;
      flat in vec4 vSegment; flat in vec4 vCaps; flat in float vDashStart;
      bool dashOn(int row, float along) {
        int n = int(texelFetch(uDash, ivec2(0, row), 0).r);
        float period = 0.0;
        for (int k = 1; k <= 8; k++) { if (k > n) break; period += texelFetch(uDash, ivec2(k, row), 0).r; }
        if (period <= 0.0) return true;
        float phase = mod(along, period);
        for (int k = 1; k <= 8; k++) {
          if (k > n) break;
          float v = texelFetch(uDash, ivec2(k, row), 0).r;
          if (phase < v) return k % 2 == 1;
          phase -= v;
        }
        return true;
      }
      void main() {
        clipData();
        vec2 f = gl_FragCoord.xy/uRatio, a = vSegment.xy, b = vSegment.zw, ab = b-a;
        float l2 = max(dot(ab, ab), 1e-12), t = dot(f-a, ab)/l2;
        if (t < 0.0 && vCaps.x > 1.5 && length(f-a) > vCaps.z) discard;
        if (t > 1.0 && vCaps.y > 1.5 && length(f-b) > vCaps.z) discard;
        if (vCaps.w > 0.5 && !dashOn(int(vCaps.w)-1, vDashStart+clamp(t, 0.0, 1.0)*sqrt(l2)/uPpm)) discard;
        emit(vColour);
      }`],
    [MARKER]: [`
      in vec3 aPos; in vec3 aShape; in vec4 aColour; in vec4 aOutline;
      #ifdef INSTANCED
      in float aAngle;
      #else
      in vec2 aOffset; in vec2 aLocal;
      #endif
      in uint aId; in vec4 aFlags; in float aState;
      ${VARYINGS}
      out vec2 vLocal; flat out vec3 vShape; flat out vec4 vOutline; flat out vec3 vMode;
      void main() {
        float d; vec4 p = project(aPos, d);
        #ifdef INSTANCED
        vec2 local = vec2((gl_VertexID & 1) == 0 ? -1.0 : 1.0, (gl_VertexID & 2) == 0 ? -1.0 : 1.0);
        vec2 offset = local*aShape.xy;
        offset = mat2(cos(aAngle), sin(aAngle), -sin(aAngle), cos(aAngle))*offset;
        #else
        vec2 local = aLocal, offset = aOffset;
        #endif
        gl_Position = clipPosition(p.xy+offset*uPpm, p.z, p.w);
        vColour = vec4(aColour.rgb, aColour.a*aState); vOutline = vec4(aOutline.rgb, aOutline.a*aState);
        vModel = aPos; vId = aId; vClip = aFlags.y;
        vLocal = local; vShape = aShape; vMode = aFlags.xzw;
      }`, `
      in vec2 vLocal; flat in vec3 vShape; flat in vec4 vOutline; flat in vec3 vMode;
      void main() {
        clipData();
        if (vMode.x < 0.5) { emit(vColour); return; }
        float r = length(vLocal), aa = max(fwidth(r), 1e-6);
        float cover = clamp((1.0-r)/aa+0.5, 0.0, 1.0);
        if (cover <= 0.0) discard;
        vec4 c = vColour;
        if (vMode.y > 0.5 || vMode.z > 0.5) {
          float rim = length(vLocal*vShape.xy)/max(r, 1e-6);
          float inner = max(0.0, 1.0-2.0*vShape.z/max(rim, 1e-6));
          if (r > inner) c = vOutline; else if (vMode.y > 0.5) discard;
        }
        emit(vec4(c.rgb, c.a*cover));
      }`],
    [TEXT]: [`
      in vec3 aPos; in vec2 aCorner; in vec4 aBox; in float aAngle; in uint aId; in float aState;
      ${VARYINGS}
      void main() {
        float d; vec4 p = project(aPos, d);
        vec2 o = vec2((aCorner.x-aBox.z)*aBox.x, (aCorner.y-aBox.w)*aBox.y);
        float a = radians(aAngle);
        o = vec2(cos(a)*o.x-sin(a)*o.y, sin(a)*o.x+cos(a)*o.y);
        gl_Position = clipPosition(p.xy+o*uPpm, p.z, p.w);
        vColour = vec4(1.0); vModel = aPos; vId = aId; vClip = 0.0;
      }`, `
      void main() { emit(vColour); }`]
  };
  SHADERS[DISC] = SHADERS[MARKER];

  function compile(gl, kind, pick) {
    const header = '#version 300 es\nprecision highp float;\nprecision highp int;\n' + (pick ? '#define PICK\n' : '') + (INSTANCED.has(kind) ? '#define INSTANCED\n' : '');
    return shaderProgram(gl, header+PROJECT+SHADERS[kind][0], header+FRAGMENT+SHADERS[kind][1],
      LAYOUTS[kind].concat(STREAMS[kind].map(n => [n])).map(a => a[0]));
  }
  function shaderProgram(gl, vertex, fragment, attributes = []) {
    const shader = (type, source) => {
      const s = gl.createShader(type); gl.shaderSource(s, source); gl.compileShader(s);
      if (!gl.getShaderParameter(s, gl.COMPILE_STATUS)) throw Error(gl.getShaderInfoLog(s));
      return s;
    };
    const p = gl.createProgram();
    const shaders = [shader(gl.VERTEX_SHADER, vertex), shader(gl.FRAGMENT_SHADER, fragment)];
    shaders.forEach(s => gl.attachShader(p, s));
    attributes.forEach((a, loc) => gl.bindAttribLocation(p, loc, a));
    gl.linkProgram(p);
    if (!gl.getProgramParameter(p, gl.LINK_STATUS)) throw Error(gl.getProgramInfoLog(p));
    shaders.forEach(s => gl.deleteShader(s));
    const cache = {};
    p.uniform = name => name in cache ? cache[name] : (cache[name] = gl.getUniformLocation(p, name));
    return p;
  }

  // Weighted blended transparency (McGuire and Bavoil 2013) accumulates
  // linear-light colour and alpha weights separately from the product of
  // fragment transmittances.
  //
  // Faces of an alpha_mode = "surface" sheet count once per pixel: only the
  // nearest fragment of each sheet enters the accumulation, as in the static
  // backends. A sheet pass copies the opaque depth into a second depth buffer,
  // draws the sheet's faces depth-only with a LESS test, which leaves the
  // nearest sheet depth where the sheet is in front of the opaque scene, and
  // accumulates the same faces with an EQUAL test against it: a one-layer
  // depth peel per sheet (Everitt 2001; Bavoil and Myers 2008). The same
  // program and vertices write and test the depth, so the nearest fragment
  // passes exactly, and the rasteriser's fill rule keeps shared edges from
  // passing twice. Sheets whose screen boxes do not overlap share one pass.
  class Transparency {
    constructor(gl) {
      this.gl = gl;
      if (!gl.getExtension('EXT_color_buffer_float')) throw Error('Transparent scenes need WebGL 2 with EXT_color_buffer_float.');
      this.depth = gl.createRenderbuffer();
      this.targets = [gl.RGBA8, gl.RGBA8, gl.RGBA16F, gl.RGBA8].map(format =>
        ({format, framebuffer: gl.createFramebuffer(), texture: gl.createTexture()}));
      this.sheetDepth = gl.createRenderbuffer();
      this.sheetTargets = [null, 2, 3].map(texture => ({texture, framebuffer: gl.createFramebuffer()}));
      this.vao = gl.createVertexArray();
      this.program = shaderProgram(gl, `#version 300 es
        void main() { vec2 p = vec2((gl_VertexID << 1) & 2, gl_VertexID & 2); gl_Position = vec4(p*2.0-1.0, 0.0, 1.0); }`,
        `#version 300 es
        precision highp float;
        uniform sampler2D uScene, uAccum, uReveal; uniform vec2 uSize; uniform bool uCopy;
        out vec4 colour;
        ${COLOUR_SPACE}
        void main() {
          vec2 uv = gl_FragCoord.xy/uSize;
          vec4 base = texture(uScene, uv);
          if (uCopy) { colour = base; return; }
          vec4 accum = texture(uAccum, uv);
          float alpha = 1.0-texture(uReveal, uv).r;
          vec3 average = accum.rgb/max(accum.a, 1e-8);
          float total = alpha+base.a*(1.0-alpha);
          vec3 rgb = average*alpha+toLinear(base.rgb/max(base.a, 1e-8))*base.a*(1.0-alpha);
          colour = vec4(toSrgb(clamp(rgb/max(total, 1e-8), 0.0, 1.0))*total, total);
        }`);
    }
    resize(width, height) {
      if (width === this.width && height === this.height) return;
      const gl = this.gl; this.width = width; this.height = height;
      gl.bindRenderbuffer(gl.RENDERBUFFER, this.depth); gl.renderbufferStorage(gl.RENDERBUFFER, gl.DEPTH_COMPONENT24, width, height);
      this.targets.forEach(t => {
        gl.bindTexture(gl.TEXTURE_2D, t.texture);
        gl.texImage2D(gl.TEXTURE_2D, 0, t.format, width, height, 0, gl.RGBA, t.format === gl.RGBA16F ? gl.HALF_FLOAT : gl.UNSIGNED_BYTE, null);
        [gl.TEXTURE_MIN_FILTER, gl.TEXTURE_MAG_FILTER].forEach(f => gl.texParameteri(gl.TEXTURE_2D, f, gl.NEAREST));
        [gl.TEXTURE_WRAP_S, gl.TEXTURE_WRAP_T].forEach(f => gl.texParameteri(gl.TEXTURE_2D, f, gl.CLAMP_TO_EDGE));
        gl.bindFramebuffer(gl.FRAMEBUFFER, t.framebuffer);
        gl.framebufferTexture2D(gl.FRAMEBUFFER, gl.COLOR_ATTACHMENT0, gl.TEXTURE_2D, t.texture, 0);
        gl.framebufferRenderbuffer(gl.FRAMEBUFFER, gl.DEPTH_ATTACHMENT, gl.RENDERBUFFER, this.depth);
        if (gl.checkFramebufferStatus(gl.FRAMEBUFFER) !== gl.FRAMEBUFFER_COMPLETE) throw Error('The transparency framebuffer is unavailable.');
      });
      gl.bindRenderbuffer(gl.RENDERBUFFER, this.sheetDepth); gl.renderbufferStorage(gl.RENDERBUFFER, gl.DEPTH_COMPONENT24, width, height);
      this.sheetTargets.forEach(t => {
        gl.bindFramebuffer(gl.FRAMEBUFFER, t.framebuffer);
        if (t.texture != null) gl.framebufferTexture2D(gl.FRAMEBUFFER, gl.COLOR_ATTACHMENT0, gl.TEXTURE_2D, this.targets[t.texture].texture, 0);
        else gl.drawBuffers([gl.NONE]);
        gl.framebufferRenderbuffer(gl.FRAMEBUFFER, gl.DEPTH_ATTACHMENT, gl.RENDERBUFFER, this.sheetDepth);
        if (gl.checkFramebufferStatus(gl.FRAMEBUFFER) !== gl.FRAMEBUFFER_COMPLETE) throw Error('The sheet framebuffer is unavailable.');
      });
    }
    // Starts a sheet pass: the opaque depth becomes the sheet depth, bound for
    // the depth-only draw. bindSheet(1) and bindSheet(2) then select the
    // accumulation and transmittance targets tested against it.
    startSheet() {
      const gl = this.gl;
      gl.bindFramebuffer(gl.READ_FRAMEBUFFER, this.targets[3].framebuffer);
      gl.bindFramebuffer(gl.DRAW_FRAMEBUFFER, this.sheetTargets[0].framebuffer);
      gl.blitFramebuffer(0, 0, this.width, this.height, 0, 0, this.width, this.height, gl.DEPTH_BUFFER_BIT, gl.NEAREST);
      gl.bindFramebuffer(gl.FRAMEBUFFER, this.sheetTargets[0].framebuffer);
    }
    bindSheet(pass) { this.gl.bindFramebuffer(this.gl.FRAMEBUFFER, this.sheetTargets[pass].framebuffer); }
    bind(index, clear) {
      const gl = this.gl;
      gl.bindFramebuffer(gl.FRAMEBUFFER, this.targets[index].framebuffer);
      if (clear != null) { gl.clearColor(clear, clear, clear, clear); gl.clear(gl.COLOR_BUFFER_BIT); }
    }
    resolve(source, copy = false) {
      const gl = this.gl, p = this.program;
      if (copy) gl.bindFramebuffer(gl.FRAMEBUFFER, null); else this.bind(1-source);
      gl.disable(gl.BLEND); gl.disable(gl.DEPTH_TEST); gl.depthMask(false);
      gl.useProgram(p); gl.bindVertexArray(this.vao); gl.uniform2f(p.uniform('uSize'), this.width, this.height);
      gl.uniform1i(p.uniform('uCopy'), copy ? 1 : 0);
      [source, 2, 3].forEach((index, unit) => {
        gl.activeTexture(gl.TEXTURE0+unit); gl.bindTexture(gl.TEXTURE_2D, this.targets[index].texture);
        gl.uniform1i(p.uniform(['uScene', 'uAccum', 'uReveal'][unit]), unit);
      });
      gl.drawArrays(gl.TRIANGLES, 0, 3);
      return 1-source;
    }
    release() {
      const gl = this.gl;
      this.targets.forEach(t => { gl.deleteFramebuffer(t.framebuffer); gl.deleteTexture(t.texture); });
      this.sheetTargets.forEach(t => gl.deleteFramebuffer(t.framebuffer));
      gl.deleteRenderbuffer(this.sheetDepth);
      gl.deleteRenderbuffer(this.depth); gl.deleteVertexArray(this.vao); gl.deleteProgram(this.program);
    }
  }

  // An interleaved vertex array that grows as primitives are added.
  class Vertices {
    constructor(layout) {
      let offset = 0;
      this.attrs = layout.map(([name, size, type]) => {
        const a = {name, size, type, offset};
        offset += type === 'f32' || type === 'u32' ? 4*size : size;
        offset = Math.ceil(offset/4)*4;
        return a;
      });
      this.byName = {}; this.attrs.forEach(a => { this.byName[a.name] = a; });
      this.stride = offset; this.count = 0; this.allocate(256);
    }
    allocate(capacity) {
      const old = this.u8, buffer = new ArrayBuffer(capacity*this.stride);
      this.capacity = capacity; this.f32 = new Float32Array(buffer); this.u32 = new Uint32Array(buffer); this.u8 = new Uint8Array(buffer);
      if (old) this.u8.set(old);
    }
    add(values) {
      if (this.count === this.capacity) this.allocate(this.capacity*2);
      const v = this.count++;
      return this.set(v, values);
    }
    set(v, values) {
      for (const name in values) {
        const a = this.byName[name], x = values[name], base = v*this.stride+a.offset;
        if (a.type === 'f32') for (let k = 0; k < a.size; k++) this.f32[base/4+k] = x[k];
        else if (a.type === 'u32') this.u32[base/4] = x;
        else if (a.type === 'u8n') for (let k = 0; k < a.size; k++) this.u8[base+k] = Math.round(Math.max(0, Math.min(1, x[k]))*255);
        else for (let k = 0; k < a.size; k++) this.u8[base+k] = x[k];
      }
      return v;
    }
    data() { return this.u8.subarray(0, this.count*this.stride); }
  }

  function bindAttributes(gl, vertices, start = 0, divisor = 0) {
    vertices.attrs.forEach((a, loc) => {
      const offset = a.offset+start*vertices.stride;
      gl.enableVertexAttribArray(loc);
      if (a.type === 'u32') gl.vertexAttribIPointer(loc, a.size, gl.UNSIGNED_INT, vertices.stride, offset);
      else gl.vertexAttribPointer(loc, a.size, a.type === 'f32' ? gl.FLOAT : gl.UNSIGNED_BYTE, a.type === 'u8n', vertices.stride, offset);
      gl.vertexAttribDivisor(loc, divisor);
    });
  }

  function packetPrimitives(packet, furniture, glyphs = new Map()) {
    ['vertices','corners','faces','polylines','markers','text','footprints','footprint_vertices'].forEach(name => {
      Object.keys(packet[name]).forEach(key => { packet[name][key] = column(packet[name][key]); });
    });
    const sheet = packet.face_sheet = column(packet.face_sheet);
    const vx = packet.vertices.x, vy = packet.vertices.y, vz = packet.vertices.z, corner = packet.corners.vertex;
    const result = [];
    ['faces', 'polylines', 'markers', 'text'].forEach((name, kind) => {
      const t = packet[name];
      for (let i = 0; i < t.offset.length; i++) {
        const material = packet.materials[t.style_id[i]-1];
        if (material.anchor) continue;
        const points = [], glyph = [];
        for (let j = 0; j < t.count[i]; j++) {
          const v = corner[t.offset[i]+j-1]-1;
          points.push([vx[v], vy[v], vz[v]]); glyph.push(glyphs.get(v+1));
        }
        const p = {kind, points, material, opacity: t.opacity[i], partition: t.partition[i], furniture, row: i, table: t, packet,
          primitiveId: t.primitive_id[i], layer: t.layer_id[i], source: t.source_row[i], sheet: kind === FACE ? sheet[i] || 0 : 0};
        if (glyph.some(Boolean)) p.glyph = glyph;
        if (kind === FACE) p.normal = material.normal || cross(sub(points[1], points[0]), sub(points[2], points[0]));
        result.push(p);
      }
    });
    return result;
  }

  // Paint colour of a primitive before lighting and selection.
  function baseColour(p) {
    const m = p.material;
    const c = (p.kind === FACE ? m.fill : (p.kind === MARKER || p.kind === DISC) && Number(m.shape) >= 21 ? m.fill : m.colour).slice();
    c[3] = c[3] > 0 ? p.opacity : 0;
    return c;
  }

  // GPU geometry of one panel. Primitive i (0-based) carries pick id i+1;
  // furniture carries 0 and is never picked.
  function buildScene(gl, panel) {
    const glyphs = new Map();
    arr(panel.glyphs).forEach(g => {
      const rows = column(g.rows), angle = column(g.angle);
      const vectors = {};
      ['centre', 'local', 'u', 'v', 'up'].forEach(key => { vectors[key] = g[key] == null ? null : column(g[key]); });
      const at = (key, i) => vectors[key] == null ? null : Array.from(vectors[key].slice(3*i, 3*i+3));
      rows.forEach((row, i) => glyphs.set(row, {facing: g.facing, angle: angle[i]*rad,
        centre: at('centre', i), local: at('local', i), u: at('u', i), v: at('v', i), up: at('up', i)}));
    });
    const prims = packetPrimitives(panel.scene, false, glyphs).concat(packetPrimitives(panel.furniture, true));
    prims.forEach(p => {
      if (p.kind === MARKER && p.packet.footprints.kind[p.packet.markers.footprint_id[p.row]-1] === 1) p.kind = DISC;
    });
    const vertices = {}, elements = {};
    KINDS.forEach(k => { vertices[k] = new Vertices(LAYOUTS[k]); elements[k] = []; });
    const dashes = [], dashRows = new Map(), dashed = [];
    const dashRow = pattern => {
      const key = pattern.join(',');
      if (!dashRows.has(key)) { dashRows.set(key, dashes.length); dashes.push(pattern.slice(0, 8)); }
      return dashRows.get(key);
    };
    prims.forEach((p, i) => {
      p.index = i; p.colour = baseColour(p);
      const id = p.furniture ? 0 : i+1, clip = p.furniture ? 0 : 1, V = vertices[p.kind], E = elements[p.kind];
      p.vertexStart = V.count; p.elementStart = E.length;
      if (p.colour[3] <= 0 && p.kind !== TEXT) { p.skip = true; }
      else if (p.kind === FACE && p.points.length >= 3) {
        const shade = p.material.shade ? 1 : 0;
        p.points.forEach(v => V.add({aPos: v, aNormal: p.normal, aColour: p.colour, aId: id, aFlags: [shade, clip, 0, 0]}));
        for (let j = 1; j < p.points.length-1; j++) E.push(p.vertexStart, p.vertexStart+j, p.vertexStart+j+1);
      } else if (p.kind === STROKE && !['blank', '0'].includes(String(p.material.linetype))) {
        const t = p.table, width = t.width_mm[p.row] || p.material.width_mm;
        const lineCap = [0, 2, 1][t.cap[p.row]] || 0, join = t.join[p.row] === 0 ? 2 : 0;
        const pattern = arr(p.material.dash).filter(Number.isFinite), row = pattern.length ? dashRow(pattern)+1 : 0;
        const segments = [];
        for (let j = 1; j < p.points.length; j++) {
          const a = p.points[j-1], b = p.points[j];
          const caps = [j === 1 ? lineCap : join, j === p.points.length-1 ? lineCap : join];
          const start = V.count;
          [[-1, 0], [1, 0], [-1, 1], [1, 1]].forEach(([side, end]) =>
            V.add({aA: a, aB: b, aCorner: [side, end, caps[0], caps[1]], aWidth: [width], aColour: p.colour, aId: id, aFlags: [0, clip, row, 0]}));
          E.push(start, start+1, start+2, start+2, start+1, start+3);
          segments.push(start);
        }
        if (row) dashed.push({p, segments, period: pattern.reduce((s, v) => s+v, 0)});
      } else if (p.kind === MARKER || p.kind === DISC) {
        markerGeometry(p, V, E, id, clip);
      } else if (p.kind === TEXT) {
        const t = p.table, r = p.row, box = [t.width_mm[r], t.height_mm[r], t.hjust[r], t.vjust[r]];
        [[0, 0], [1, 0], [0, 1], [1, 1]].forEach(c => V.add({aPos: p.points[0], aCorner: c, aBox: box, aAngle: [t.angle[r]], aId: id}));
        E.push(p.vertexStart, p.vertexStart+1, p.vertexStart+2, p.vertexStart+2, p.vertexStart+1, p.vertexStart+3);
      }
      p.vertexCount = V.count-p.vertexStart; p.elementCount = E.length-p.elementStart;
      if (!p.elementCount) p.skip = true;
    });
    const scene = {prims, dashed, kinds: {}, glyphs, glyphFaces: prims.filter(p => p.glyph && p.kind === FACE && !p.skip)};
    KINDS.forEach(k => {
      const V = vertices[k], vao = gl.createVertexArray(), buffer = gl.createBuffer();
      gl.bindVertexArray(vao);
      gl.bindBuffer(gl.ARRAY_BUFFER, buffer); gl.bufferData(gl.ARRAY_BUFFER, INSTANCED.has(k) ? 0 : V.data(), gl.STATIC_DRAW);
      bindAttributes(gl, V);
      const streams = STREAMS[k].map((name, j) => {
        const b = gl.createBuffer(), values = new Float32Array(Math.max(1, V.count)).fill(name === 'aState' ? 1 : 0), loc = V.attrs.length+j;
        gl.bindBuffer(gl.ARRAY_BUFFER, b); gl.bufferData(gl.ARRAY_BUFFER, values, gl.DYNAMIC_DRAW);
        gl.enableVertexAttribArray(loc); gl.vertexAttribPointer(loc, 1, gl.FLOAT, false, 0, 0);
        return {name, buffer: b, values};
      });
      gl.bindVertexArray(null);
      const all = new Uint32Array(elements[k]);
      scene.kinds[k] = {vao, buffer, streams, elements: all, count: V.count, vertices: V, instanced: INSTANCED.has(k),
        states: streams.length ? streams[0].values : new Float32Array(V.count).fill(1),
        fixed: gl.createBuffer(), frame: gl.createBuffer(), fixedRanges: [[0, 0], [0, 0], [0, 0]],
        fixedData: new Uint32Array(all.length), frameData: new Uint32Array(all.length)};
    });
    const dashTexture = gl.createTexture(), rows = Math.max(1, dashes.length), texels = new Float32Array(9*rows);
    dashes.forEach((d, r) => { texels[9*r] = d.length; d.forEach((v, j) => { texels[9*r+1+j] = v; }); });
    gl.bindTexture(gl.TEXTURE_2D, dashTexture);
    gl.texImage2D(gl.TEXTURE_2D, 0, gl.R32F, 9, rows, 0, gl.RED, gl.FLOAT, texels);
    [gl.TEXTURE_MIN_FILTER, gl.TEXTURE_MAG_FILTER].forEach(f => gl.texParameteri(gl.TEXTURE_2D, f, gl.NEAREST));
    scene.dashTexture = dashTexture;
    scene.release = () => {
      KINDS.forEach(k => { const g = scene.kinds[k]; gl.deleteVertexArray(g.vao); [g.buffer, g.fixed, g.frame, ...g.streams.map(s => s.buffer)].forEach(b => gl.deleteBuffer(b)); });
      gl.deleteTexture(dashTexture);
    };
    return scene;
  }

  // Outline vertices are positioned in model space before the colour and id
  // passes. The same updated positions also supply face normals and labels.
  function updateGlyphs(gl, V) {
    if (!V.scene.glyphFaces.length) return;
    const b = V.basis, c = V.camera, step = 1e-3;
    const depth = b.perspective ? b.distance+step*dot(b.right, b.forward) : 1;
    const factor = step*b.base*V.data.viewport.height_mm/(2*depth);
    const k = step/Math.hypot(dot(b.right, b.right)*factor, dot(b.right, b.up)*factor);
    function planeUp(n, hint) {
      let v = sub(hint, n.map(x => x*dot(hint, n)));
      if (dot(v, v) < 1e-20) {
        const index = n.reduce((best, x, i) => Math.abs(x) < Math.abs(n[best]) ? i : best, 0);
        const axis = [0, 0, 0]; axis[index] = 1;
        v = sub(axis, n.map(x => x*dot(axis, n)));
      }
      return norm(v);
    }
    V.scene.glyphs.forEach(g => {
      const delta = g.centre.map((x, i) => (x-c.target[i])*b.scale[i]);
      const toward = b.perspective ? delta.map((x, i) => -x-b.forward[i]*b.distance) : b.ray.map(x => -x);
      let u, v, n;
      if (g.facing === 'fixed') {
        u = g.u; v = g.v; n = cross(u, v);
        if (dot(n, toward) < 0) u = u.map(x => -x);
      } else {
        n = norm(g.facing === 'camera_point' ? toward : (b.perspective ? b.forward : b.ray).map(x => -x));
        v = planeUp(n, g.up || b.up); u = norm(cross(v, n));
      }
      const x = g.local[0]*Math.cos(g.angle)-g.local[1]*Math.sin(g.angle);
      const y = g.local[0]*Math.sin(g.angle)+g.local[1]*Math.cos(g.angle);
      g.point = g.centre.map((a, i) => a+(x*u[i]+y*v[i])*k/b.scale[i]);
    });
    const geometry = V.scene.kinds[FACE], vertices = geometry.vertices, ranges = [];
    V.scene.glyphFaces.forEach(p => {
      p.points = p.glyph.map((g, i) => g ? g.point : p.points[i]);
      p.normal = cross(sub(p.points[1], p.points[0]), sub(p.points[2], p.points[0]));
      p.points.forEach((point, i) => vertices.set(p.vertexStart+i, {aPos: point, aNormal: p.normal}));
      const end = p.vertexStart+p.vertexCount, last = ranges[ranges.length-1];
      if (last && last[1] === p.vertexStart) last[1] = end;
      else ranges.push([p.vertexStart, end]);
    });
    gl.bindBuffer(gl.ARRAY_BUFFER, geometry.buffer);
    ranges.forEach(([start, end]) => gl.bufferSubData(gl.ARRAY_BUFFER, start*vertices.stride,
      vertices.u8.subarray(start*vertices.stride, end*vertices.stride)));
  }

  // Marker footprints in mm around the anchor: ellipses as one quad whose
  // fragment shader cuts the outline and border, other shapes as triangles.
  function markerGeometry(p, V, E, id, clip) {
    const fp = p.packet.footprints, i = p.packet.markers.footprint_id[p.row]-1;
    const shape = Number(p.material.shape), border = p.material.border_mm;
    const hollow = [0, 1, 2, 5, 6].includes(shape), separate = shape >= 21 && shape <= 25;
    const outline = p.material.colour.slice(); outline[3] *= p.opacity;
    const angle = fp.angle[i]*rad, rotate = v => [Math.cos(angle)*v[0]-Math.sin(angle)*v[1], Math.sin(angle)*v[0]+Math.cos(angle)*v[1]];
    const start = V.count;
    if (fp.kind[i] === 1) {
      const rx = fp.radius_x_mm[i], ry = fp.radius_y_mm[i];
      V.add({aPos: p.points[0], aAngle: [angle], aState: [1], aShape: [rx, ry, border],
        aColour: p.colour, aOutline: outline, aId: id, aFlags: [1, clip, hollow ? 1 : 0, separate ? 1 : 0]});
      E.push(start);
      return;
    }
    const fv = p.packet.footprint_vertices, rings = []; let ring, last;
    for (let j = fp.offset[i]-1; j < fp.offset[i]+fp.count[i]-1; j++) {
      if (fv.ring[j] !== last) { ring = []; rings.push(ring); last = fv.ring[j]; }
      ring.push([fv.x_mm[j], fv.y_mm[j]]);
    }
    const outer = rings[0];
    if (!outer) return;
    let inner = rings[1];
    if (!inner && (hollow || separate)) inner = outer.map(v => v.map(x => x*Math.max(0, 1-2*border/Math.max(1e-9, Math.hypot(...v)))));
    const vertex = (v, c) => V.add({aPos: p.points[0], aOffset: rotate(v), aLocal: [0, 0], aShape: [0, 0, 0], aColour: c, aOutline: c, aId: id, aFlags: [0, clip, 0, 0]});
    if (inner && inner.length === outer.length) {
      outer.forEach((v, j) => {
        const k = (j+1)%outer.length, a = vertex(v, outline), b = vertex(outer[k], outline), c = vertex(inner[j], outline), d = vertex(inner[k], outline);
        E.push(a, b, c, c, b, d);
      });
    }
    if (!hollow) {
      const ring = inner || outer, centre = ring.reduce((a, b) => a.map((v, j) => v+b[j]/ring.length), [0, 0]);
      const c = vertex(centre, p.colour), first = V.count;
      ring.forEach(v => vertex(v, p.colour));
      ring.forEach((_, j) => E.push(c, first+j, first+(j+1)%ring.length));
    }
  }

  HTMLWidgets.widget({
    name: 'ggplot3d_view', type: 'output',
    factory: function (el, width, height) {
      el.classList.add('ggplot3d_view');
      const chrome = document.createElement('div'); chrome.className = 'gg3d-chrome';
      const canvas = document.createElement('canvas'), labels = document.createElement('canvas');
      canvas.className = 'gg3d-surface'; labels.className = 'gg3d-labels';
      canvas.tabIndex = 0;
      canvas.setAttribute('aria-label', '3D scene. Drag or use arrow keys to orbit. Wheel, pinch, plus or minus to zoom. Click to select, Escape to clear.');
      const tip = document.createElement('div'); tip.className = 'gg3d-tooltip'; tip.hidden = true; tip.setAttribute('role', 'tooltip');
      const controls = document.createElement('div'); controls.className = 'gg3d-controls';
      const play = document.createElement('button'), reset = document.createElement('button'), report = document.createElement('button');
      play.textContent = 'Play'; reset.textContent = 'Reset'; report.textContent = 'Camera code';
      play.type = reset.type = report.type = 'button'; play.hidden = true; controls.append(play, reset, report);
      const code = document.createElement('textarea'); code.className = 'gg3d-code'; code.hidden = true;
      code.readOnly = true; code.rows = 3; code.setAttribute('aria-label', 'Current coord_3d camera call');
      el.append(chrome, canvas, labels, tip, controls, code);
      const ctx = labels.getContext('2d');
      const gl = canvas.getContext('webgl2', {alpha: true, antialias: true, preserveDrawingBuffer: true});
      if (!gl) {
        el.replaceChildren(document.createTextNode('WebGL 2 is unavailable. Enable WebGL to view this scene.'));
        return {renderValue: function () {}, resize: function () {}};
      }
      const programs = {}, pickPrograms = {};
      KINDS.forEach(k => { programs[k] = compile(gl, k, false); pickPrograms[k] = compile(gl, k, true); });
      const pickTarget = {framebuffer: gl.createFramebuffer(), colour: gl.createRenderbuffer(), depth: gl.createRenderbuffer(), panel: -1};
      let transparency = null;
      let data, views = [], frame, active = 0, frameRequest, pickRequest, hovered = null;
      let selected = new Set(), filter = null, selectionHandle = null, filterHandle = null;
      let playback = null, pathIndex = 0, group = null;

      function frameLayout() {
        const ppm = Math.min(width/frame.width_mm, height/frame.height_mm);
        return {ppm, x: (width-frame.width_mm*ppm)/2, y: (height-frame.height_mm*ppm)/2};
      }
      function project(V, p) {
        const b = V.basis, L = V.layout, c = V.camera;
        const delta = p.map((v, i) => (v-c.target[i])*b.scale[i]);
        const depth = b.distance+dot(delta, b.forward);
        const m = b.base/(b.perspective ? depth : 1)/2;
        return [L.ox+(0.5+dot(delta, b.right)*m*V.data.viewport.height_mm/V.data.viewport.width_mm)*L.iw,
          L.oy+(0.5+dot(delta, b.up)*m)*L.ih, depth];
      }
      function visiblePoint(V, p) {
        const q = project(V, p);
        return q[2] >= V.basis.near && q[2] <= V.basis.far && (!V.camera.clip_data || p.every(v => v >= 0 && v <= 1));
      }
      // A pane faces away when its outward normal points away from the eye.
      function rearFacing(V, normal, points) {
        const b = V.basis, centre = [0, 1, 2].map(i => points.reduce((s, v) => s+v[i], 0)/points.length);
        const delta = centre.map((v, i) => (v-V.camera.target[i])*b.scale[i]);
        const toward = b.perspective ? b.forward.map((v, i) => -v*b.distance-delta[i]) : b.ray.map(v => -v);
        const facing = dot(normal, toward);
        return facing <= 64*Number.EPSILON*Math.max(1, Math.abs(facing));
      }
      function furnitureVisible(V, p) {
        const f = p.material.frame;
        if (!f) return true;
        const names = arr(f.panes), normals = f.normals || [], framing = V.data.framing;
        const rear = normals.every(n => rearFacing(V, n, p.points));
        p.partition = V.camera.clip_data ? (rear ? 0 : 2) : 1;
        if (f.type === 'edge') return framing.box === 'all' || (framing.box === 'back' && rear);
        const name = names[0], panes = arr(framing.panes);
        return panes.includes(name) || (panes.includes('floor') && arr(framing.floor).includes(name)) ||
          (panes.includes('back') && arr(framing.back).includes(name) && rear);
      }

      // Selection state: dimmed primitives move to the translucent pass,
      // filtered primitives leave the scene.
      function uploadOrder(g, buffer, indices, count, usage) {
        if (g.instanced) {
          const source = g.vertices, bytes = new Uint8Array(count*source.stride);
          for (let i = 0; i < count; i++) {
            const j = indices[i], begin = j*source.stride;
            source.f32[(begin+source.byName.aState.offset)/4] = g.states[j];
            bytes.set(source.u8.subarray(begin, begin+source.stride), i*source.stride);
          }
          gl.bindBuffer(gl.ARRAY_BUFFER, g[buffer]); gl.bufferData(gl.ARRAY_BUFFER, bytes, usage);
        } else {
          gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER, g[buffer]); gl.bufferData(gl.ELEMENT_ARRAY_BUFFER, indices.subarray(0, count), usage);
        }
      }
      function applyState(V) {
        const S = V.scene, dim = data.selection.dim;
        KINDS.forEach(k => S.kinds[k].states.fill(1));
        S.prims.forEach(p => {
          p.hidden = !p.furniture && filter && p.crosstalk && !filter.has(p.key);
          p.dimmed = !p.furniture && selected.size > 0 && !selected.has(p.key);
          if (p.dimmed && !p.skip) S.kinds[p.kind].states.fill(dim, p.vertexStart, p.vertexStart+p.vertexCount);
          const outline = (p.kind === MARKER || p.kind === DISC) && p.material.colour[3]*p.opacity < 1;
          p.translucent = p.colour[3] < 1 || outline || p.dimmed;
        });
        KINDS.forEach(k => {
          const g = S.kinds[k], s = g.streams[0];
          if (s) { gl.bindBuffer(gl.ARRAY_BUFFER, s.buffer); gl.bufferSubData(gl.ARRAY_BUFFER, 0, s.values); }
          let n = 0;
          [0, 1, 2].forEach(part => {
            const start = n;
            S.prims.forEach(p => {
              if (p.kind !== k || p.furniture || p.skip || p.hidden || p.translucent || p.partition !== part || k === TEXT) return;
              g.fixedData.set(g.elements.subarray(p.elementStart, p.elementStart+p.elementCount), n); n += p.elementCount;
            });
            g.fixedRanges[part] = [start, n-start];
          });
          uploadOrder(g, 'fixed', g.fixedData, n, gl.DYNAMIC_DRAW);
        });
        V.translucent = S.prims.filter(p => !p.furniture && !p.skip && !p.hidden && p.translucent && p.kind !== TEXT);
        V.pickDirty = true;
      }

      // Per-frame lists: furniture (camera-dependent panes and edges),
      // translucent primitives batched by kind, and the
      // labels that are visible for picking.
      function prepare(V) {
        const S = V.scene;
        const furniture = S.prims.filter(p => p.furniture && !p.skip && p.kind !== TEXT && furnitureVisible(V, p));
        const fill = {}; KINDS.forEach(k => { fill[k] = 0; });
        const push = p => { const g = S.kinds[p.kind]; g.frameData.set(g.elements.subarray(p.elementStart, p.elementStart+p.elementCount), fill[p.kind]); fill[p.kind] += p.elementCount; };
        V.furnitureRanges = [0, 1, 2].map(part => {
          const out = {};
          KINDS.forEach(k => {
            const start = fill[k];
            furniture.forEach(p => { if (p.kind === k && p.partition === part && !p.translucent) push(p); });
            out[k] = [start, fill[k]-start];
          });
          return out;
        });
        const translucent = V.translucent.concat(furniture.filter(p => p.translucent));
        const batches = sheetBatches(V, translucent), isolated = new Set(batches.flat());
        V.runs = [0, 1, 2].map(part => {
          const runs = [];
          DRAW_KINDS.forEach(kind => {
            const start = fill[kind];
            translucent.forEach(p => { if (p.partition === part && p.kind === kind && !isolated.has(p)) push(p); });
            if (fill[kind] > start) runs.push({kind, start, count: fill[kind]-start});
          });
          return runs;
        });
        V.sheetRuns = batches.map(batch => {
          const start = fill[FACE];
          batch.forEach(push);
          return {kind: FACE, start, count: fill[FACE]-start};
        });
        V.labels = S.prims.filter(p => p.kind === TEXT && !p.furniture && !p.hidden && !p.skip && visiblePoint(V, p.points[0]) &&
          (p.partition !== 1 || !anchorHidden(V, p.points[0])));
        const textStart = fill[TEXT];
        V.labels.forEach(push);
        V.textRange = [textStart, fill[TEXT]-textStart];
        KINDS.forEach(k => {
          const g = S.kinds[k];
          uploadOrder(g, 'frame', g.frameData, fill[k], gl.STREAM_DRAW);
        });
        if (S.dashed.length) dashPhases(V);
      }
      // Sheet passes of the translucent alpha_mode = "surface" faces of the
      // data partition. Sheets are placed first fit, largest first, into the
      // first pass whose sheets' screen boxes they do not overlap; a sheet
      // that finds no room once MAX_SHEET_PASSES passes exist is composited
      // face by face.
      function sheetBatches(V, translucent) {
        const sheets = new Map();
        translucent.forEach(p => {
          if (p.kind !== FACE || p.partition !== 1 || !(p.sheet > 0)) return;
          if (!sheets.has(p.sheet)) sheets.set(p.sheet, []);
          sheets.get(p.sheet).push(p);
        });
        if (sheets.size <= 1) return Array.from(sheets.values());
        const items = Array.from(sheets.values()).sort((a, b) => b.length-a.length).map(faces => {
          const box = [Infinity, Infinity, -Infinity, -Infinity];
          faces.forEach(p => p.points.forEach(v => {
            const q = project(V, v);
            box[0] = Math.min(box[0], q[0]); box[1] = Math.min(box[1], q[1]);
            box[2] = Math.max(box[2], q[0]); box[3] = Math.max(box[3], q[1]);
          }));
          return {faces, box};
        });
        const overlap = (a, b) => a[0] <= b[2]+1 && b[0] <= a[2]+1 && a[1] <= b[3]+1 && b[1] <= a[3]+1;
        const passes = [];
        items.forEach(item => {
          let pass = passes.find(q => q.boxes.every(b => !overlap(b, item.box)));
          if (!pass && passes.length < MAX_SHEET_PASSES) passes.push(pass = {boxes: [], faces: []});
          if (pass) { pass.boxes.push(item.box); pass.faces.push(...item.faces); }
        });
        return passes.map(q => q.faces);
      }
      // Dashes continue along a polyline in projected millimetres.
      function dashPhases(V) {
        const g = V.scene.kinds[STROKE], s = g.streams[1];
        V.scene.dashed.forEach(({p, segments, period}) => {
          let travel = 0;
          for (let j = 0; j < segments.length; j++) {
            const pa = project(V, p.points[j]), pb = project(V, p.points[j+1]);
            s.values.fill(period > 0 ? travel%period : 0, segments[j], segments[j]+4);
            travel += Math.hypot(pb[0]-pa[0], pb[1]-pa[1])/V.layout.ppm;
          }
        });
        gl.bindBuffer(gl.ARRAY_BUFFER, s.buffer); gl.bufferSubData(gl.ARRAY_BUFFER, 0, s.values);
      }
      function anchorHidden(V, anchor) {
        const p = project(V, anchor);
        return V.opaqueFaces.some(face => {
          for (let i = 1; i < face.points.length-1; i++) {
            const q = [face.points[0], face.points[i], face.points[i+1]].map(v => project(V, v));
            const a = sub(q[1], q[0]), b = sub(q[2], q[0]), d = sub(p, q[0]), den = a[0]*b[1]-a[1]*b[0];
            if (Math.abs(den) < 1e-10) continue;
            const u = (d[0]*b[1]-d[1]*b[0])/den, v = (a[0]*d[1]-a[1]*d[0])/den;
            if (u < 0 || v < 0 || u+v > 1) continue;
            const z = V.basis.perspective ? 1/((1-u-v)/q[0][2]+u/q[1][2]+v/q[2][2]) : (1-u-v)*q[0][2]+u*q[1][2]+v*q[2][2];
            if (z < p[2]-1e-9) return true;
          }
          return false;
        });
      }

      function uniforms(prog, V) {
        const b = V.basis, L = V.layout, set3 = (n, v) => gl.uniform3fv(prog.uniform(n), v), set1 = (n, v) => gl.uniform1f(prog.uniform(n), v);
        gl.useProgram(prog);
        set3('uTarget', V.camera.target); set3('uScale', b.scale); set3('uRight', b.right); set3('uUp', b.up); set3('uForward', b.forward);
        set1('uDistance', b.distance); set1('uNear', b.near); set1('uFar', b.far); set1('uBase', b.base);
        set1('uPerspective', b.perspective ? 1 : 0); set1('uPpm', L.ppm); set1('uRatio', canvas.width/width);
        set1('uClipData', V.camera.clip_data ? 1 : 0);
        gl.uniform2f(prog.uniform('uSize'), width, height); gl.uniform2f(prog.uniform('uOrigin'), L.ox, L.oy);
        gl.uniform2f(prog.uniform('uInner'), L.iw, L.ih);
        const light = V.data.light;
        set1('uLight', light ? 1 : 0);
        if (light) {
          let direction = light.direction;
          if (light.space === 'camera') direction = [0, 1, 2].map(i => b.orthoRight[i]*light.direction[0]+b.orthoUp[i]*light.direction[1]-b.forward[i]*light.direction[2]);
          else if (light.space === 'data') direction = direction.map((v, i) => v*b.scale[i]);
          set3('uLightDir', norm(direction)); set3('uRay', b.ray);
          set1('uAmbient', light.ambient); set1('uDiffuse', light.diffuse); set1('uMultiply', light.mode === 'multiply' ? 1 : 0);
        }
        gl.activeTexture(gl.TEXTURE0); gl.bindTexture(gl.TEXTURE_2D, V.scene.dashTexture);
        gl.uniform1i(prog.uniform('uDash'), 0);
      }
      function drawRange(V, kind, buffer, range, set, oitPass = 0) {
        if (!range || range[1] <= 0) return;
        uniforms(set[kind], V);
        gl.uniform1i(set[kind].uniform('uOitPass'), oitPass);
        const g = V.scene.kinds[kind];
        if (g.instanced) {
          gl.bindVertexArray(g.vao); gl.bindBuffer(gl.ARRAY_BUFFER, g[buffer]);
          bindAttributes(gl, g.vertices, range[0], 1);
          gl.drawArraysInstanced(gl.TRIANGLE_STRIP, 0, 4, range[1]);
          return;
        }
        gl.bindVertexArray(g.vao); gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER, g[buffer]);
        gl.drawElements(gl.TRIANGLES, range[1], gl.UNSIGNED_INT, range[0]*4);
      }
      function rect(r) {
        const ratio = canvas.width/width;
        return r.map(v => Math.round(v*ratio));
      }
      function drawPanel(V, pick) {
        const set = pick ? pickPrograms : programs, L = V.layout;
        const oit = !pick && (V.runs.some(runs => runs.length) || V.sheetRuns.length > 0);
        let source = 0;
        if (oit) {
          if (!transparency) transparency = new Transparency(gl);
          transparency.resize(canvas.width, canvas.height); transparency.bind(source);
        }
        gl.enable(gl.SCISSOR_TEST);
        gl.scissor(...rect([L.px, L.py, L.pw, L.ph]));
        const bg = pick ? [0, 0, 0, 0] : V.data.background;
        gl.clearColor(...(oit ? bg.map((v, i) => i < 3 ? v*bg[3] : v) : bg)); gl.clearDepth(1); gl.depthMask(true);
        gl.clear(gl.COLOR_BUFFER_BIT | gl.DEPTH_BUFFER_BIT);
        if (V.camera.clip) gl.scissor(...rect([L.ox, L.oy, L.iw, L.ih]));
        else gl.disable(gl.SCISSOR_TEST);
        if (pick) gl.disable(gl.BLEND);
        else { gl.enable(gl.BLEND); gl.blendFuncSeparate(gl.SRC_ALPHA, gl.ONE_MINUS_SRC_ALPHA, gl.ONE, gl.ONE_MINUS_SRC_ALPHA); }
        gl.depthFunc(gl.LEQUAL);
        [0, 1, 2].forEach(part => {
          if (oit) {
            transparency.bind(source);
            gl.enable(gl.BLEND); gl.blendFuncSeparate(gl.SRC_ALPHA, gl.ONE_MINUS_SRC_ALPHA, gl.ONE, gl.ONE_MINUS_SRC_ALPHA);
          }
          if (part === 1) gl.enable(gl.DEPTH_TEST); else gl.disable(gl.DEPTH_TEST);
          gl.depthMask(part === 1);
          DRAW_KINDS.forEach(k => {
            drawRange(V, k, 'fixed', V.scene.kinds[k].fixedRanges[part], set);
            drawRange(V, k, 'frame', V.furnitureRanges[part][k], set);
          });
          gl.depthMask(pick && part === 1);
          const sheetRuns = part === 1 ? V.sheetRuns : [];
          const accumulate = pass => {
            gl.enable(gl.BLEND);
            if (pass === 1) gl.blendFunc(gl.ONE, gl.ONE);
            else gl.blendFunc(gl.ZERO, gl.ONE_MINUS_SRC_COLOR);
          };
          if (oit && (V.runs[part].length || sheetRuns.length)) {
            [1, 2].forEach(pass => {
              transparency.bind(pass+1, pass === 1 ? 0 : 1);
              accumulate(pass);
              V.runs[part].forEach(r => drawRange(V, r.kind, 'frame', [r.start, r.count], set, pass));
            });
            sheetRuns.forEach(r => {
              transparency.startSheet();
              gl.disable(gl.BLEND); gl.colorMask(false, false, false, false);
              gl.depthMask(true); gl.depthFunc(gl.LESS);
              drawRange(V, r.kind, 'frame', [r.start, r.count], set);
              gl.colorMask(true, true, true, true); gl.depthMask(false); gl.depthFunc(gl.EQUAL);
              [1, 2].forEach(pass => {
                transparency.bindSheet(pass);
                accumulate(pass);
                drawRange(V, r.kind, 'frame', [r.start, r.count], set, pass);
              });
              gl.depthFunc(gl.LEQUAL);
            });
            source = transparency.resolve(source);
          } else V.runs[part].concat(sheetRuns).forEach(r => drawRange(V, r.kind, 'frame', [r.start, r.count], set));
        });
        if (pick) { gl.disable(gl.DEPTH_TEST); drawRange(V, TEXT, 'frame', V.textRange, set); }
        gl.depthMask(true); gl.disable(gl.SCISSOR_TEST); gl.bindVertexArray(null);
        if (oit) {
          gl.enable(gl.SCISSOR_TEST); gl.scissor(...rect([L.px, L.py, L.pw, L.ph]));
          transparency.resolve(source, true);
          gl.depthMask(true); gl.disable(gl.SCISSOR_TEST); gl.bindVertexArray(null);
        }
      }

      function text(label, xy, style, angle, hjust, vjust, ppm) {
        if (!label || !style.visible) return;
        ctx.save(); ctx.translate(xy[0], height-xy[1]); ctx.rotate(-angle*rad);
        const face = style.face, bold = face === 'bold' || face === 2 || face === 4, italic = face === 'italic' || face === 3 || face === 4;
        ctx.font = (italic ? 'italic ' : '')+(bold ? 'bold ' : '')+Math.max(11, style.size*25.4/72.27*ppm)+'px '+(style.family || 'sans-serif');
        ctx.fillStyle = css(style.colour); ctx.textAlign = 'left'; ctx.textBaseline = 'middle';
        const w = ctx.measureText(label).width;
        ctx.fillText(label, -hjust*w, (vjust-0.5)*style.size*25.4/72.27*ppm); ctx.restore();
      }
      function axes(V) {
        const L = V.layout, P = p => project(V, p), ppm = L.ppm;
        const pts = arr(V.data.bounds).map(P), centre = [0, 1].map(a => pts.reduce((s, p) => s+p[a], 0)/pts.length);
        const occupied = [];
        function line(a, b, s) {
          if (!s.visible) return;
          ctx.beginPath(); ctx.strokeStyle = css(s.colour); ctx.lineWidth = s.width*ppm;
          ctx.moveTo(a[0], height-a[1]); ctx.lineTo(b[0], height-b[1]); ctx.stroke();
        }
        function polyline(points, s) {
          if (!s.visible || points.length < 2) return;
          ctx.beginPath(); ctx.strokeStyle = css(s.colour); ctx.lineWidth = s.width*ppm;
          points.forEach((p, i) => i ? ctx.lineTo(p[0], height-p[1]) : ctx.moveTo(p[0], height-p[1])); ctx.stroke();
        }
        arr(V.data.axes).forEach(g => {
          if (!g.visible) return;
          const options = arr(g.candidates.options), choices = options.flatMap((c, i) => {
            const path = arr(c.path).map(P), a = path[0], b = path[path.length-1], d = sub(b, a), len = Math.hypot(d[0], d[1]);
            const depth = path.reduce((s, p) => s+p[2], 0)/path.length;
            if (len < 1e-7) {
              if (path.length <= 2) return [];
              const low = Math.min(...path.map(p => p[1])), xs = path.map(p => p[0]);
              const mid = [(Math.min(...xs)+Math.max(...xs))/2, low];
              return [{c, i, path, closed: true, n: [0, -1], a, b, mid, exterior: false, distance: centre[1]-low, depth}];
            }
            let n = [-d[1]/len, d[0]/len]; const mid = [(a[0]+b[0])/2, (a[1]+b[1])/2];
            if (dot(n, sub(mid, centre)) < 0) n = n.map(v => -v);
            const exterior = Math.max(...pts.map(p => dot(sub(p.slice(0, 2), a.slice(0, 2)), n))) <= 1e-6;
            return [{c, i, path, closed: false, n, a, b, mid, exterior, distance: dot(n, sub(mid, centre)), depth: (a[2]+b[2])/2}];
          });
          const auto = q => g.candidates.auto === 'lower' ? -q.mid[1] : q.distance;
          const score = q => g.position === 'top' ? q.mid[1] : g.position === 'bottom' ? -q.mid[1] : g.position === 'left' ? -q.mid[0] : g.position === 'right' ? q.mid[0] : auto(q);
          const rounded = (v, n) => Math.round(v*Math.pow(10, n))/Math.pow(10, n);
          choices.sort((a, b) => Number(b.exterior)-Number(a.exterior) || rounded(score(b)/ppm, 8)-rounded(score(a)/ppm, 8) || rounded(a.depth, 12)-rounded(b.depth, 12) || a.i-b.i);
          let q = choices[0];
          if (typeof g.position === 'number' || /^[1-4]$/.test(g.position)) q = choices.find(q => q.i === Number(g.position)-1);
          if (g.secondary && q) {
            const primary = q;
            q = choices.slice().sort((a, b) => Number(b.exterior)-Number(a.exterior) || Math.hypot(...sub(b.mid, primary.mid))-Math.hypot(...sub(a.mid, primary.mid)))[0];
          }
          if (!q) return;
          const straight = q.path.length === 2, values = arr(g.values), labelText = arr(g.labels), d = sub(q.b, q.a);
          const ticks = arr(q.c.ticks).map(P), before = arr(q.c.before).map(P), after = arr(q.c.after).map(P);
          // A curved axis turns its ticks with the local direction, away from the frame centre.
          const normalAt = i => {
            if (straight || !before[i]) return q.n;
            const t = sub(after[i], before[i]), len = Math.hypot(t[0], t[1]) || 1;
            let n = [-t[1]/len, t[0]/len];
            if (dot(n, sub(ticks[i].slice(0, 2), centre)) < 0) n = n.map(v => -v);
            return n;
          };
          const lo = (g.cap === 'lower' || g.cap === 'both') && values.length ? Math.min(...values) : 0;
          const hi = (g.cap === 'upper' || g.cap === 'both') && values.length ? Math.max(...values) : 1;
          if (straight) line(q.a.map((v, i) => v+d[i]*lo), q.a.map((v, i) => v+d[i]*hi), g.line);
          else polyline(q.path, g.line);
          const titleSize = arr(g.title_size).flat(), sizes = g.label_size || [];
          const sizeAt = i => Array.isArray(sizes[0]) ? sizes[i] : [sizes[i], sizes[i+values.length]];
          const tangent = straight ? norm(d.slice(0, 2)) : null;
          // Labels stand in dodge rows as deep as the deepest label along the normal.
          const depths = values.map((value, i) => { const size = sizeAt(i), n = normalAt(i); return Math.abs(n[0])*size[0]+Math.abs(n[1])*size[1]; });
          const step = (depths.length ? Math.max(...depths) : 0)+g.gap/2, band = depths.length ? g.dodge*step-g.gap/2 : 0;
          const candidates = values.map((value, i) => {
            const a = ticks[i], size = sizeAt(i), n = normalAt(i);
            const offset = g.tick_length+g.gap+depths[i]/2+(i%g.dodge)*step;
            const xy = a.slice(0, 2).map((v, j) => v+n[j]*offset*ppm);
            line(a, a.slice(0, 2).map((v, j) => v+n[j]*g.tick_length*ppm), g.ticks);
            const w = size[0]*ppm, h = size[1]*ppm;
            return {i, xy, box: [xy[0]-w/2, xy[1]-h/2, xy[0]+w/2, xy[1]+h/2], end: tangent ? dot(xy, tangent)+(Math.abs(tangent[0])*w+Math.abs(tangent[1])*h)/2 : i};
          });
          if (g.minor_ticks) arr(q.c.minor).map(P).forEach(a => line(a, a.slice(0, 2).map((v, j) => v+q.n[j]*g.tick_length/2*ppm), g.ticks));
          candidates.sort((a, b) => a.end-b.end || a.i-b.i).forEach(c => {
            const b = c.box, gap = (g.check_overlap ? 1 : 0.5)*ppm;
            if (b.some(v => !Number.isFinite(v))) return;
            if (g.thin && (b[0] < 0 || b[1] < 0 || b[2] > width || b[3] > height || occupied.some(a => a[0] < b[2]+gap && a[2] > b[0]-gap && a[1] < b[3]+gap && a[3] > b[1]-gap))) return;
            occupied.push(b); text(labelText[c.i], c.xy, g.text, g.angle, 0.5, 0.5, ppm);
          });
          const title = arr(g.title)[0];
          if (title) {
            const th = Array.isArray(titleSize[0]) ? titleSize[0][1] : titleSize[1];
            const offset = (g.tick_length+g.gap+(g.text.visible ? band : 0)+g.title_gap+th/2)*ppm;
            let anchor = q.mid, n = q.n, angle = 0;
            if (q.c.title_at) {
              anchor = P(q.c.title_at); n = norm(sub(anchor.slice(0, 2), centre));
            } else if (q.closed) {
              anchor = [q.mid[0], Math.min(q.mid[1], ...pts.map(p => p[1]))]; n = [0, -1];
            } else {
              if (!straight) {
                const offsets = q.path.map(p => dot(sub(p.slice(0, 2), q.a.slice(0, 2)), n)), side = Math.abs(Math.max(...offsets)) >= Math.abs(Math.min(...offsets)) ? 1 : -1;
                const bulge = Math.max(0, ...offsets.map(v => side*v)); n = n.map(v => side*v); anchor = q.mid.map((v, j) => v+n[j]*bulge);
              }
              angle = Math.atan2(d[1], d[0])/rad; if (angle > 90) angle -= 180; if (angle < -90) angle += 180;
            }
            text(title, anchor.slice(0, 2).map((v, j) => v+n[j]*offset), g.title_style, angle, 0.5, 0.5, ppm);
          }
        });
      }
      function sceneLabels(V) {
        V.labels.forEach(p => {
          const m = p.material, t = p.table, i = p.row, c = p.colour.slice();
          if (p.dimmed) c[3] *= data.selection.dim;
          text(m.label, project(V, p.points[0]), {visible: true, colour: c, size: m.fontsize, family: m.family, face: m.fontface}, t.angle[i], t.hjust[i], t.vjust[i], V.layout.ppm);
        });
      }

      function cameraCode(camera, framing) {
        const n = x => Number(x.toFixed(6));
        const vector = v => 'c('+v.map(n).join(', ')+')';
        const shear = arr(camera.shear), oblique = camera.projection === 'oblique' ? ', oblique = c(angle = '+n(Math.atan2(shear[1], shear[0])/rad)+', ratio = '+n(Math.hypot(shear[0], shear[1]))+')' : '';
        const vertical = (camera.up || 3) !== 3 ? ', up = "'+'xyz'[camera.up-1]+'"' : '';
        return 'coord_3d(azimuth = '+n(camera.azimuth)+', elevation = '+n(camera.elevation)+',\n  roll = '+n(camera.roll)+', zoom = '+n(camera.zoom)+', projection = "'+camera.projection+'"'+oblique+vertical+',\n  frame = "'+framing.frame+'", aspect = '+vector(camera.aspect)+',\n  target = '+vector(camera.target)+', fov = '+n(camera.fov)+
          (camera.distance == null ? '' : ', distance = '+n(camera.distance))+
          (camera.near == null ? '' : ', near = '+n(camera.near))+(camera.far == null ? '' : ', far = '+n(camera.far))+')';
      }
      function publish() {
        const V = views[active], snapshot = clone(V.camera);
        el.ggplot3dCamera = snapshot;
        el.dispatchEvent(new CustomEvent('ggplot3d:camera', {detail: snapshot}));
        if (window.Shiny && el.id) window.Shiny.setInputValue(el.id+'_camera', snapshot, {priority: 'event'});
        code.value = cameraCode(V.camera, V.data.framing);
      }
      function draw() {
        frameRequest = null; if (!data) return;
        const F = frameLayout();
        gl.viewport(0, 0, canvas.width, canvas.height);
        gl.disable(gl.SCISSOR_TEST); gl.clearColor(0, 0, 0, 0); gl.clear(gl.COLOR_BUFFER_BIT);
        ctx.setTransform(labels.width/width, 0, 0, labels.height/height, 0, 0); ctx.clearRect(0, 0, width, height);
        views.forEach(V => {
          const vp = V.data.viewport, r = V.data.rect_mm, ppm = F.ppm;
          V.basis = cameraBasis(V.camera, vp, V.framing);
          updateGlyphs(gl, V);
          const px = F.x+r[0]*ppm, py = F.y+r[1]*ppm;
          V.layout = {ppm, px, py, pw: r[2]*ppm, ph: r[3]*ppm,
            ox: px+(r[2]-vp.outer_width_mm)*ppm/2+V.data.framing.origin[0]*ppm,
            oy: py+(r[3]-vp.outer_height_mm)*ppm/2+V.data.framing.origin[1]*ppm,
            iw: vp.width_mm*ppm, ih: vp.height_mm*ppm};
          V.opaqueFaces = V.scene.prims.filter(p => p.kind === FACE && !p.furniture && !p.skip && !p.hidden && p.partition === 1 && !p.translucent);
          prepare(V);
          drawPanel(V, false);
          V.pickDirty = true;
          sceneLabels(V); axes(V);
        });
        publish(); el.dataset.rendered = 'true'; el.dataset.renderer = 'webgl2';
      }
      function schedule() { if (!frameRequest) frameRequest = requestAnimationFrame(draw); }

      // Picking renders the panel's ids offscreen and reads a small window
      // around the pointer, nearest hit first.
      function pickAt(x, y, radius) {
        const i = panelAt(x, y);
        if (i < 0 || !data) return null;
        const V = views[i], w = canvas.width, h = canvas.height, ratio = w/width;
        if (pickTarget.width !== w || pickTarget.height !== h) {
          gl.bindRenderbuffer(gl.RENDERBUFFER, pickTarget.colour); gl.renderbufferStorage(gl.RENDERBUFFER, gl.RGBA8, w, h);
          gl.bindRenderbuffer(gl.RENDERBUFFER, pickTarget.depth); gl.renderbufferStorage(gl.RENDERBUFFER, gl.DEPTH_COMPONENT24, w, h);
          gl.bindFramebuffer(gl.FRAMEBUFFER, pickTarget.framebuffer);
          gl.framebufferRenderbuffer(gl.FRAMEBUFFER, gl.COLOR_ATTACHMENT0, gl.RENDERBUFFER, pickTarget.colour);
          gl.framebufferRenderbuffer(gl.FRAMEBUFFER, gl.DEPTH_ATTACHMENT, gl.RENDERBUFFER, pickTarget.depth);
          pickTarget.width = w; pickTarget.height = h; pickTarget.panel = -1;
        }
        gl.bindFramebuffer(gl.FRAMEBUFFER, pickTarget.framebuffer);
        if (pickTarget.panel !== i || V.pickDirty) {
          gl.viewport(0, 0, w, h);
          drawPanel(V, true);
          pickTarget.panel = i; V.pickDirty = false;
        }
        const r = Math.round((radius == null ? 4 : radius)*ratio), cx = Math.round(x*ratio), cy = Math.round((height-y)*ratio);
        const x0 = Math.max(0, cx-r), y0 = Math.max(0, cy-r), x1 = Math.min(w-1, cx+r), y1 = Math.min(h-1, cy+r);
        let best = null;
        if (x1 >= x0 && y1 >= y0) {
          const pixels = new Uint8Array(4*(x1-x0+1)*(y1-y0+1));
          gl.readPixels(x0, y0, x1-x0+1, y1-y0+1, gl.RGBA, gl.UNSIGNED_BYTE, pixels);
          for (let yy = y0; yy <= y1; yy++) for (let xx = x0; xx <= x1; xx++) {
            const k = 4*((yy-y0)*(x1-x0+1)+(xx-x0)), id = pixels[k]+256*pixels[k+1]+65536*pixels[k+2]+16777216*pixels[k+3];
            const d = Math.hypot(xx-cx, yy-cy);
            if (id && d <= r && (!best || d < best.d)) best = {id, d};
          }
        }
        gl.bindFramebuffer(gl.FRAMEBUFFER, null);
        if (!best) return null;
        const p = V.scene.prims[best.id-1];
        return p ? {panel: i+1, panel_id: V.data.id, primitive_id: p.primitiveId, layer: p.layer, row: p.source,
          key: p.key == null ? null : p.key, tooltip: p.tooltip == null ? null : p.tooltip} : null;
      }
      function panelAt(x, y) {
        const yy = height-y;
        return views.findIndex(V => V.layout && x >= V.layout.px && x <= V.layout.px+V.layout.pw && yy >= V.layout.py && yy <= V.layout.py+V.layout.ph);
      }

      function setSelection(keys, notify) {
        selected = new Set(arr(keys).map(String));
        views.forEach(applyState); schedule();
        if (!notify) return;
        const value = Array.from(selected);
        el.dispatchEvent(new CustomEvent('ggplot3d:select', {detail: {keys: value}}));
        if (window.Shiny && el.id) window.Shiny.setInputValue(el.id+'_selected', value, {priority: 'event'});
        if (selectionHandle) {
          const linked = value.filter(k => views.some(V => V.crosstalkKeys.has(k)));
          if (linked.length) selectionHandle.set(linked); else selectionHandle.clear();
        }
      }
      function click(x, y, extend) {
        const mode = data.selection.mode;
        if (mode === 'none') return;
        const hit = pickAt(x, y);
        if (!hit || hit.key == null) { if (!extend && selected.size) setSelection([], true); return; }
        let keys;
        if (mode === 'multiple' && extend) {
          keys = new Set(selected);
          if (keys.has(hit.key)) keys.delete(hit.key); else keys.add(hit.key);
        } else keys = selected.size === 1 && selected.has(hit.key) ? [] : [hit.key];
        setSelection(Array.from(keys), true);
      }
      function hover(x, y) {
        pickRequest = null;
        const hit = data ? pickAt(x, y) : null, same = hit && hovered && hit.panel === hovered.panel && hit.primitive_id === hovered.primitive_id;
        if (!same) {
          hovered = hit;
          el.dispatchEvent(new CustomEvent('ggplot3d:hover', {detail: hit}));
        }
        canvas.classList.toggle('gg3d-pickable', !!(hit && (hit.key != null || hit.tooltip)));
        if (!hit || !hit.tooltip) { tip.hidden = true; return; }
        tip.textContent = hit.tooltip; tip.hidden = false;
        const w = tip.offsetWidth, h = tip.offsetHeight;
        tip.style.left = Math.max(0, Math.min(width-w, x+12))+'px';
        tip.style.top = Math.max(0, Math.min(height-h, y+12 + (y+12+h > height ? -h-24 : 0)))+'px';
      }

      function resize(w, h) {
        width = w; height = h; const ratio = window.devicePixelRatio || 1;
        [canvas, labels].forEach(c => { c.width = Math.round(w*ratio); c.height = Math.round(h*ratio); });
        schedule();
      }
      const observer = new ResizeObserver(entries => {
        const box = entries[0].contentRect;
        if (box.width > 0 && box.height > 0 && (Math.abs(box.width-width) > 0.5 || Math.abs(box.height-height) > 0.5)) resize(box.width, box.height);
      });
      observer.observe(el);

      // Orbit and zoom apply to the panel under the pointer, or to every
      // panel by the same amount when cameras are linked. A turn or reset,
      // from this viewer or one of its link group, stops path playback.
      function turnViews(change) {
        if (!data) return;
        stop();
        (data.link ? views : [views[active]]).forEach(V => {
          const c = V.camera;
          if (change.azimuth) c.azimuth += change.azimuth;
          if (change.elevation) c.elevation = Math.max(-90, Math.min(90, c.elevation+change.elevation));
          if (change.roll) c.roll += change.roll;
          if (change.zoom) c.zoom *= change.zoom;
        });
        schedule();
      }
      function resetViews() {
        if (!data) return;
        stop(); pathIndex = 0;
        views.forEach(V => { V.camera = clone(V.initial); V.framing = V.data.framing; });
        schedule();
      }
      const self = {turn: turnViews, reset: resetViews};
      function peers() { return group ? Array.from(linkGroups.get(group) || []) : [self]; }
      function move(change) { if (data) peers().forEach(v => v.turn(change)); }
      function joinLink(name) {
        if (group) linkGroups.get(group).delete(self);
        group = name || null;
        if (group) { if (!linkGroups.has(group)) linkGroups.set(group, new Set()); linkGroups.get(group).add(self); }
      }

      // Path playback sets every panel's camera to the sampled view of the
      // current frame, with the magnification of the path's framing.
      const pathOf = () => (data && views.length ? views[0].data.path : null);
      function pathFrame(k) {
        pathIndex = k;
        views.forEach(V => {
          const p = V.data.path; if (!p) return;
          const at = key => arr(p[key])[k];
          Object.assign(V.camera, {azimuth: at('azimuth'), elevation: at('elevation'), roll: at('roll'), zoom: at('zoom'),
            fov: at('fov'), projection: at('projection'), distance: at('distance') > 0 ? at('distance') : null,
            target: [at('target_x'), at('target_y'), at('target_z')], shear: [at('shear_x'), at('shear_y')]});
          V.framing = Object.assign({}, V.data.framing, {zoom_scale: p.zoom_scale});
        });
        schedule();
      }
      function stop() {
        if (playback) cancelAnimationFrame(playback.request);
        playback = null; play.textContent = 'Play';
      }
      function tick(now) {
        const P = pathOf(), n = arr(P.azimuth).length;
        let k = Math.floor((now-playback.start)/1000/P.step);
        if (P.loop) k %= n;
        else if (k >= n-1) { pathFrame(n-1); stop(); pathIndex = 0; return; }
        pathFrame(k); playback.request = requestAnimationFrame(tick);
      }
      function start() {
        const P = pathOf(); if (!P) return;
        stop(); playback = {start: performance.now()-pathIndex*P.step*1000};
        play.textContent = 'Pause'; playback.request = requestAnimationFrame(tick);
      }
      play.addEventListener('click', () => (playback ? stop() : start()));
      const local = e => { const box = canvas.getBoundingClientRect(); return [e.clientX-box.left, e.clientY-box.top]; };
      const pointers = new Map(); let pinch, press;
      canvas.addEventListener('pointerdown', e => {
        stop(); canvas.focus(); canvas.setPointerCapture(e.pointerId); pointers.set(e.pointerId, [e.clientX, e.clientY]);
        const xy = local(e), i = panelAt(...xy);
        if (i >= 0) active = i;
        press = pointers.size === 1 ? {xy, client: [e.clientX, e.clientY], extend: e.shiftKey || e.ctrlKey || e.metaKey} : null;
        const ps = Array.from(pointers.values()); pinch = ps.length === 2 ? Math.hypot(...sub(ps[0], ps[1])) : null;
      });
      canvas.addEventListener('pointermove', e => {
        if (!data) return;
        if (!pointers.has(e.pointerId)) {
          const xy = local(e);
          if (!pickRequest) pickRequest = requestAnimationFrame(() => hover(...xy));
          return;
        }
        tip.hidden = true;
        const old = pointers.get(e.pointerId); pointers.set(e.pointerId, [e.clientX, e.clientY]);
        if (pointers.size === 1) {
          if (press && Math.hypot(e.clientX-press.client[0], e.clientY-press.client[1]) > 3) press = null;
          if (!press) move({azimuth: -(e.clientX-old[0])*0.5, elevation: (e.clientY-old[1])*0.5});
        } else {
          press = null;
          const ps = Array.from(pointers.values()), d = Math.hypot(...sub(ps[0], ps[1]));
          if (pinch) move({zoom: d/pinch});
          pinch = d;
        }
      });
      function release(e) {
        if (press && e.type === 'pointerup' && pointers.size === 1) click(...press.xy, press.extend);
        pointers.delete(e.pointerId); pinch = null; press = null;
      }
      canvas.addEventListener('pointerup', release); canvas.addEventListener('pointercancel', release);
      canvas.addEventListener('pointerleave', () => { tip.hidden = true; });
      canvas.addEventListener('wheel', e => {
        if (!data) return; e.preventDefault();
        const i = panelAt(...local(e)); if (i >= 0) active = i;
        move({zoom: Math.exp(-Math.max(-100, Math.min(100, e.deltaY))*0.001)});
      }, {passive: false});
      canvas.addEventListener('keydown', e => {
        if (!data) return;
        const actions = {ArrowLeft: {azimuth: -5}, ArrowRight: {azimuth: 5}, ArrowUp: {elevation: 5}, ArrowDown: {elevation: -5},
          '+': {zoom: 1.1}, '=': {zoom: 1.1}, '-': {zoom: 1/1.1}};
        if (actions[e.key]) { e.preventDefault(); move(actions[e.key]); }
        if (e.key === 'Escape' && selected.size) { e.preventDefault(); setSelection([], true); }
      });
      reset.addEventListener('click', () => { if (data) peers().forEach(v => v.reset()); });
      report.addEventListener('click', () => { publish(); code.hidden = !code.hidden; if (!code.hidden) { code.focus(); code.select(); } });

      function connectCrosstalk(group) {
        if (selectionHandle) { selectionHandle.close(); filterHandle.close(); selectionHandle = filterHandle = null; }
        if (!group || !window.crosstalk) return;
        selectionHandle = new window.crosstalk.SelectionHandle(group);
        filterHandle = new window.crosstalk.FilterHandle(group);
        selectionHandle.on('change', e => { if (e.sender !== selectionHandle) setSelection(e.value || [], false); });
        filterHandle.on('change', e => { filter = e.value ? new Set(e.value.map(String)) : null; views.forEach(applyState); schedule(); });
        filter = filterHandle.filteredKeys ? new Set(filterHandle.filteredKeys.map(String)) : null;
        if (selectionHandle.value && selectionHandle.value.length) selected = new Set(selectionHandle.value.map(String));
      }

      resize(width, height);
      return {
        renderValue: function (x) {
          if (x.schema_version !== 4) throw Error('Unsupported ggplot3d viewer schema');
          stop(); pathIndex = 0;
          views.forEach(V => V.scene.release());
          if (transparency) { transparency.release(); transparency = null; }
          data = x; frame = x.frame; active = 0; selected = new Set(); filter = null; hovered = null;
          chrome.innerHTML = x.chrome || '';
          const svg = chrome.querySelector('svg');
          if (svg) { svg.setAttribute('width', '100%'); svg.setAttribute('height', '100%'); svg.setAttribute('preserveAspectRatio', 'xMidYMid meet'); }
          views = arr(x.panels).map(panel => {
            if (panel.scene.schema_version !== 2) throw Error('Unsupported ggplot3d scene schema');
            const scene = buildScene(gl, panel), entries = new Map(), crosstalkKeys = new Set();
            arr(panel.layers).forEach(l => {
              const rows = arr(l.rows), keys = arr(l.key), tips = arr(l.tooltip), map = new Map();
              rows.forEach((r, j) => map.set(r, j));
              entries.set(l.layer, {map, keys, tips, crosstalk: !!l.crosstalk});
              if (l.crosstalk) keys.forEach(k => crosstalkKeys.add(String(k)));
            });
            scene.prims.forEach(p => {
              if (p.furniture) return;
              const entry = entries.get(p.layer), j = entry ? entry.map.get(p.source) : undefined;
              if (j === undefined) return;
              p.key = entry.keys[j] == null ? null : String(entry.keys[j]);
              p.tooltip = entry.tips[j] == null ? null : String(entry.tips[j]);
              p.crosstalk = entry.crosstalk;
            });
            const camera = clone(panel.camera);
            return {data: panel, scene, camera, initial: clone(camera), framing: panel.framing, crosstalkKeys};
          });
          play.hidden = !pathOf();
          joinLink(x.link_group); el.dataset.link = group || '';
          connectCrosstalk(x.selection.group);
          views.forEach(applyState);
          el.ggplot3d = {
            getCamera: panel => clone(views[(panel || active+1)-1].camera),
            setCamera: (values, panel) => { (panel ? [views[panel-1]] : views).forEach(V => Object.assign(V.camera, values)); schedule(); },
            getProjectedBox: panel => { const V = views[(panel || 1)-1]; return arr(V.data.bounds).map(p => project(V, p)); },
            pick: (x, y, radius) => pickAt(x, y, radius),
            select: keys => setSelection(keys, true),
            getSelection: () => Array.from(selected),
            redraw: draw,
            turn: move, play: start, pause: stop, setPathFrame: pathFrame, playing: () => playback !== null
          };
          draw();
        }, resize
      };
    }
  });
})();
