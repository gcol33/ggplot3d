(function () {
  'use strict';
  const rad = Math.PI / 180;
  const dot = (a, b) => a.reduce((s, v, i) => s + v * b[i], 0);
  const sub = (a, b) => a.map((v, i) => v - b[i]);
  const norm = a => { const n = Math.hypot(...a); return a.map(v => v / (n || 1)); };
  const cross = (a, b) => [a[1]*b[2]-a[2]*b[1], a[2]*b[0]-a[0]*b[2], a[0]*b[1]-a[1]*b[0]];
  const arr = x => x == null ? [] : Array.isArray(x) ? x : [x];
  const css = c => 'rgba(' + c.slice(0, 3).map(v => Math.round(v*255)).join(',') + ',' + c[3] + ')';
  // Viewers in one link group share every orbit, zoom and reset: angle
  // changes add and zoom factors multiply, so each keeps its offset.
  const linkGroups = new Map();

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

  function packetPrimitives(packet, furniture) {
    ['vertices','corners','faces','polylines','markers','text','footprints','footprint_vertices'].forEach(name=>{
      Object.keys(packet[name]).forEach(key=>packet[name][key]=arr(packet[name][key]));
    });
    const verts = packet.vertices.x.map((_, i) => [packet.vertices.x[i], packet.vertices.y[i], packet.vertices.z[i]]);
    const result = [];
    ['faces', 'polylines', 'markers', 'text'].forEach((name, kind) => {
      const t = packet[name];
      arr(t.offset).forEach((offset, i) => {
        const points = Array.from({length: t.count[i]}, (_, j) => verts[packet.corners.vertex[offset+j-1]-1]);
        const material = packet.materials[t.style_id[i]-1];
        if (material.anchor) return;
        const p = {kind, points, material, opacity: t.opacity[i], partition: t.partition[i],
          furniture, source: i, table: t, packet};
        if (kind === 0) {
          p.normal = material.normal || cross(sub(points[1], points[0]), sub(points[2], points[0]));
          p.triangles = points.slice(1, -1).flatMap((v, j) => [points[0], v, points[j+2]]);
        }
        result.push(p);
      });
    });
    return result;
  }

  HTMLWidgets.widget({
    name: 'ggplot3d_view', type: 'output',
    factory: function (el, width, height) {
      el.classList.add('ggplot3d_view');
      const canvas = document.createElement('canvas'), labels = document.createElement('canvas');
      canvas.className = 'gg3d-surface'; labels.className = 'gg3d-labels';
      canvas.tabIndex = 0;
      canvas.setAttribute('aria-label', '3D scene. Drag or use arrow keys to orbit. Wheel, pinch, plus or minus to zoom.');
      const controls = document.createElement('div'); controls.className = 'gg3d-controls';
      const play = document.createElement('button'), reset = document.createElement('button'), report = document.createElement('button');
      play.textContent = 'Play'; reset.textContent = 'Reset'; report.textContent = 'Camera code';
      play.type = reset.type = report.type = 'button'; play.hidden = true; controls.append(play, reset, report);
      const code = document.createElement('textarea'); code.className = 'gg3d-code'; code.hidden = true;
      code.readOnly = true; code.rows = 3; code.setAttribute('aria-label', 'Current coord_3d camera call');
      el.append(canvas, labels, controls, code);
      const ctx = labels.getContext('2d');
      const gl = canvas.getContext('webgl', {alpha: true, antialias: true, preserveDrawingBuffer: true});
      if (!gl) {
        el.replaceChildren(document.createTextNode('WebGL is unavailable. Enable WebGL to view this scene.'));
        return {renderValue: function () {}, resize: function () {}};
      }
      const vertexSource = `
        attribute vec3 aPoint; attribute vec2 aOffset;
        uniform vec3 uTarget, uScale, uRight, uUp, uForward;
        uniform float uDistance, uNear, uFar, uBase, uPerspective;
        uniform vec2 uSize, uOrigin, uPixels, uInner;
        varying vec3 vModel;
        void main() {
          vec3 delta=(aPoint-uTarget)*uScale;
          float d=uDistance+dot(delta,uForward);
          float w=mix(1.0,d,uPerspective);
          vec2 xy=vec2(dot(delta,uRight),dot(delta,uUp))*uBase/w;
          xy*=vec2(uInner.y/uInner.x,1.0)*0.5;
          vec2 px=uOrigin+(xy+0.5)*uInner+aOffset*uPixels;
          float z=mix(2.0*(d-uNear)/(uFar-uNear)-1.0,
            (uFar+uNear)/(uFar-uNear)-2.0*uFar*uNear/((uFar-uNear)*d),uPerspective);
          gl_Position=vec4((2.0*px/uSize-1.0)*w,z*w,w);
          vModel=aPoint;
        }`;
      const fragmentSource = `
        precision highp float;
        uniform vec4 uColour; uniform float uClip;
        varying vec3 vModel;
        void main() {
          if(uClip>0.5 && (any(lessThan(vModel,vec3(0.0))) || any(greaterThan(vModel,vec3(1.0))))) discard;
          gl_FragColor=uColour;
        }`;
      function shader(type, source) {
        const s = gl.createShader(type); gl.shaderSource(s, source); gl.compileShader(s);
        if (!gl.getShaderParameter(s, gl.COMPILE_STATUS)) throw Error(gl.getShaderInfoLog(s));
        return s;
      }
      const program = gl.createProgram();
      gl.attachShader(program, shader(gl.VERTEX_SHADER, vertexSource));
      gl.attachShader(program, shader(gl.FRAGMENT_SHADER, fragmentSource)); gl.linkProgram(program);
      if (!gl.getProgramParameter(program, gl.LINK_STATUS)) throw Error(gl.getProgramInfoLog(program));
      gl.useProgram(program);
      const uniforms = {};
      ['Target','Scale','Right','Up','Forward','Distance','Near','Far','Base','Perspective','Size','Origin','Pixels','Inner','Colour','Clip'].forEach(k => uniforms[k] = gl.getUniformLocation(program, 'u'+k));
      const pointLoc = gl.getAttribLocation(program, 'aPoint'), offsetLoc = gl.getAttribLocation(program, 'aOffset');
      const dynamic = gl.createBuffer();
      let data, camera, initial, primitives = [], buffers = [], basis, layout, frameRequest, framing, playback = null, pathIndex = 0, link = null;
      // A turn or reset, from this viewer or a linked one, stops path playback.
      const self = {
        turn(change) {
          stop();
          if(change.azimuth) camera.azimuth+=change.azimuth;
          if(change.elevation) camera.elevation=Math.max(-90,Math.min(90,camera.elevation+change.elevation));
          if(change.roll) camera.roll+=change.roll;
          if(change.zoom) camera.zoom*=change.zoom;
          schedule();
        },
        reset() {stop();pathIndex=0;framing=data.framing;camera=JSON.parse(JSON.stringify(initial));schedule();}
      };
      function peers() {return link ? Array.from(linkGroups.get(link) || []) : [self];}
      function move(change) {if(data) peers().forEach(v=>v.turn(change));}
      function joinLink(group) {
        if(link) linkGroups.get(link).delete(self);
        link=group || null;
        if(link) {if(!linkGroups.has(link)) linkGroups.set(link,new Set());linkGroups.get(link).add(self);}
      }
      let opaqueFaces = [];

      function project(p) {
        const delta = p.map((v, i) => (v-camera.target[i])*basis.scale[i]);
        const depth = basis.distance+dot(delta, basis.forward);
        const m = basis.base/(basis.perspective ? depth : 1)/2;
        return [layout.ox+(0.5+dot(delta,basis.right)*m*data.viewport.height_mm/data.viewport.width_mm)*layout.iw,
          layout.oy+(0.5+dot(delta,basis.up)*m)*layout.ih, depth];
      }
      function visiblePoint(p) {
        const q = project(p);
        return q[2]>=basis.near && q[2]<=basis.far && (!camera.clip_data || p.every(v => v>=0 && v<=1));
      }
      // A pane faces away when its outward normal points away from the eye.
      function rearFacing(normal, points) {
        const centre = [0,1,2].map(i => points.reduce((s,v) => s+v[i], 0)/points.length);
        const delta = centre.map((v,i) => (v-camera.target[i])*basis.scale[i]);
        const toward = basis.perspective ? basis.forward.map((v,i) => -v*basis.distance-delta[i]) : basis.ray.map(v => -v);
        const facing = dot(normal, toward);
        return facing <= 64*Number.EPSILON*Math.max(1, Math.abs(facing));
      }
      function furnitureVisible(p) {
        const f = p.material.frame;
        if (!f) return true;
        const names = arr(f.panes), normals = f.normals || [];
        const rear = normals.every(n => rearFacing(n, p.points));
        p.partition = camera.clip_data ? (rear ? 0 : 2) : 1;
        if (f.type === 'edge') return data.framing.box==='all' || (data.framing.box==='back' && rear);
        const name = names[0], panes = arr(data.framing.panes);
        return panes.includes(name) || (panes.includes('floor') && arr(data.framing.floor).includes(name)) ||
          (panes.includes('back') && arr(data.framing.back).includes(name) && rear);
      }
      function colour(p) {
        const m = p.material;
        let c = (p.kind===0 ? m.fill : p.kind===2 && Number(m.shape)>=21 ? m.fill : m.colour).slice();
        if (p.kind===0 && m.shade && data.light) {
          let n = norm(p.normal.map((v,i) => m.normal ? v/basis.scale[i] : v*basis.scale[(i+1)%3]*basis.scale[(i+2)%3]));
          if (dot(n,basis.ray)>0) n = n.map(v => -v);
          const light = data.light;
          let direction = light.direction;
          if (light.space==='camera') direction = n.map((_,i) => basis.orthoRight[i]*light.direction[0]+basis.orthoUp[i]*light.direction[1]-basis.forward[i]*light.direction[2]);
          else direction = direction.map((v,i) => v*basis.scale[i]);
          const brightness = light.ambient+light.diffuse*Math.max(0,dot(n,norm(direction)));
          c = c.map((v,i) => {
            if(i===3) return v;
            let linear = v<=0.04045 ? v/12.92 : Math.pow((v+0.055)/1.055,2.4);
            linear = light.mode==='multiply' ? linear*brightness : 1-(1-linear)*brightness;
            return linear<=0.0031308 ? 12.92*linear : 1.055*Math.pow(linear,1/2.4)-0.055;
          });
        }
        c[3] = c[3]>0 ? p.opacity : 0;
        return c;
      }
      function drawBuffer(buffer, count, c) {
        if (!count || c[3]<=0) return;
        gl.uniform4fv(uniforms.Colour,c); gl.bindBuffer(gl.ARRAY_BUFFER,buffer);
        gl.enableVertexAttribArray(pointLoc); gl.enableVertexAttribArray(offsetLoc);
        gl.vertexAttribPointer(pointLoc,3,gl.FLOAT,false,20,0); gl.vertexAttribPointer(offsetLoc,2,gl.FLOAT,false,20,12);
        gl.drawArrays(gl.TRIANGLES,0,count);
      }
      function drawDynamic(vertices,c) {
        gl.bindBuffer(gl.ARRAY_BUFFER,dynamic);
        gl.bufferData(gl.ARRAY_BUFFER,new Float32Array(vertices.flat()),gl.DYNAMIC_DRAW);
        drawBuffer(dynamic,vertices.length,c);
      }
      function stroke(points,width,c,dash) {
        if(width<=0 || c[3]<=0) return;
        let travel = 0;
        for(let i=1;i<points.length;i++) {
          const a=points[i-1], b=points[i], pa=project(a), pb=project(b);
          const d=[(pb[0]-pa[0])/layout.ppm,(pb[1]-pa[1])/layout.ppm], length=Math.hypot(...d);
          if(length<1e-9 || pa[2]<=0 || pb[2]<=0) continue;
          const offset=[-d[1]/length*width/2,d[0]/length*width/2];
          const vertex=(p,s) => [...p,...offset.map(v=>v*s)];
          const segment=(lo,hi) => {
            const interpolate=t => {
              const w = basis.perspective ? (t/ pb[2])/((1-t)/pa[2]+t/pb[2]) : t;
              return a.map((v,j)=>v+(b[j]-v)*w);
            };
            const x=interpolate(lo/length),y=interpolate(hi/length);
            drawDynamic([vertex(x,1),vertex(x,-1),vertex(y,1),vertex(y,1),vertex(x,-1),vertex(y,-1)],c);
          };
          const pattern=arr(dash).filter(Number.isFinite), period=pattern.reduce((s,v)=>s+v,0);
          if(!period) segment(0,length);
          else {
            let t=0;
            while(t<length) {
              let phase=(travel+t)%period,k=0;
              while(k<pattern.length-1 && phase>=pattern[k]) {phase-=pattern[k];k++;}
              const end=Math.min(length,t+pattern[k]-phase);
              if(end<=t+1e-10) {t+=1e-9;continue;}
              if(k%2===0) segment(t,end);
              t=end;
            }
          }
          travel+=length;
        }
      }
      function marker(p,c) {
        if(!visiblePoint(p.points[0])) return;
        const fp=p.packet.footprints, i=p.packet.markers.footprint_id[p.source]-1;
        const angle=fp.angle[i]*rad, rotate=v => [Math.cos(angle)*v[0]-Math.sin(angle)*v[1],Math.sin(angle)*v[0]+Math.cos(angle)*v[1]];
        let rings=[];
        if(fp.kind[i]===1) rings=[Array.from({length:48},(_,j)=>rotate([fp.radius_x_mm[i]*Math.cos(j*2*Math.PI/48),fp.radius_y_mm[i]*Math.sin(j*2*Math.PI/48)]))];
        else {
          const fv=p.packet.footprint_vertices; let ring, last;
          for(let j=fp.offset[i]-1;j<fp.offset[i]+fp.count[i]-1;j++) {
            if(fv.ring[j]!==last) {ring=[];rings.push(ring);last=fv.ring[j];}
            ring.push([fv.x_mm[j],fv.y_mm[j]]);
          }
        }
        const shape=Number(p.material.shape), border=p.material.border_mm;
        const outer=rings[0], vertex=v=>[...p.points[0],...v];
        if(!outer) return;
        let inner=rings[1];
        const hollow=[0,1,2,5,6].includes(shape), separate=shape>=21 && shape<=25;
        if(!inner && (hollow || separate)) {
          inner=outer.map(v=>v.map(x=>x*Math.max(0,1-2*border/Math.max(1e-9,Math.hypot(...v)))));
        }
        if(inner && inner.length===outer.length) {
          const outline=p.material.colour.slice();outline[3]*=p.opacity;
          drawDynamic(outer.flatMap((v,j)=>{
            const next=(j+1)%outer.length;
            return [vertex(v),vertex(outer[next]),vertex(inner[j]),vertex(inner[j]),vertex(outer[next]),vertex(inner[next])];
          }),outline);
        }
        if(!hollow) {
          const ring=inner || outer,center=ring.reduce((a,b)=>a.map((v,j)=>v+b[j]/ring.length),[0,0]);
          drawDynamic(ring.flatMap((v,j)=>[vertex(center),vertex(v),vertex(ring[(j+1)%ring.length])]),c);
        }
      }
      function paint(p) {
        gl.uniform1f(uniforms.Clip,camera.clip_data && !p.furniture ? 1 : 0);
        const c=colour(p);
        if(p.kind===0) drawBuffer(p.buffer,p.triangles.length,c);
        if(p.kind===1 && !['blank','0'].includes(String(p.material.linetype))) stroke(p.points,p.table.width_mm[p.source] || p.material.width_mm,c,p.material.dash);
        if(p.kind===2) marker(p,c);
      }
      function anchorVisible(anchor) {
        const p=project(anchor);
        return !opaqueFaces.some(face=>{
          for(let i=1;i<face.points.length-1;i++) {
            const t=[face.points[0],face.points[i],face.points[i+1]], q=t.map(project);
            const a=sub(q[1],q[0]),b=sub(q[2],q[0]),d=sub(p,q[0]),den=a[0]*b[1]-a[1]*b[0];
            if(Math.abs(den)<1e-10) continue;
            const u=(d[0]*b[1]-d[1]*b[0])/den,v=(a[0]*d[1]-a[1]*d[0])/den;
            if(u<0 || v<0 || u+v>1) continue;
            const z=basis.perspective ? 1/((1-u-v)/q[0][2]+u/q[1][2]+v/q[2][2]) : (1-u-v)*q[0][2]+u*q[1][2]+v*q[2][2];
            if(z<p[2]-1e-9) return true;
          }
          return false;
        });
      }
      function text(label,xy,style,angle,hjust,vjust) {
        if(!label || !style.visible) return;
        ctx.save(); ctx.translate(xy[0],height-xy[1]);ctx.rotate(-angle*rad);
        const face=style.face, bold=face==='bold' || face===2 || face===4, italic=face==='italic' || face===3 || face===4;
        ctx.font=(italic?'italic ':'')+(bold?'bold ':'')+Math.max(11,style.size*25.4/72.27*layout.ppm)+'px '+(style.family || 'sans-serif');
        ctx.fillStyle=css(style.colour);ctx.textAlign='left';ctx.textBaseline='middle';
        const w=ctx.measureText(label).width;
        ctx.fillText(label,-hjust*w,(vjust-0.5)*style.size*25.4/72.27*layout.ppm);ctx.restore();
      }
      function axes() {
        const pts=arr(data.bounds).map(project),centre=[0,1].map(a=>pts.reduce((s,p)=>s+p[a],0)/pts.length);
        const occupied=[];
        function line(a,b,s) {
          if(!s.visible) return;
          ctx.beginPath();ctx.strokeStyle=css(s.colour);ctx.lineWidth=s.width*layout.ppm;
          ctx.moveTo(a[0],height-a[1]);ctx.lineTo(b[0],height-b[1]);ctx.stroke();
        }
        function polyline(points,s) {
          if(!s.visible || points.length<2) return;
          ctx.beginPath();ctx.strokeStyle=css(s.colour);ctx.lineWidth=s.width*layout.ppm;
          points.forEach((p,i)=>i?ctx.lineTo(p[0],height-p[1]):ctx.moveTo(p[0],height-p[1]));ctx.stroke();
        }
        data.axes.forEach(g=>{
          if(!g.visible) return;
          const options=arr(g.candidates.options), choices=options.flatMap((c,i)=>{
            const path=arr(c.path).map(project), a=path[0], b=path[path.length-1], d=sub(b,a), len=Math.hypot(d[0],d[1]);
            const depth=path.reduce((s,p)=>s+p[2],0)/path.length;
            if(len<1e-7) {
              if(path.length<=2) return [];
              const low=Math.min(...path.map(p=>p[1])), xs=path.map(p=>p[0]);
              const mid=[(Math.min(...xs)+Math.max(...xs))/2,low];
              return [{c,i,path,closed:true,n:[0,-1],a,b,mid,exterior:false,distance:centre[1]-low,depth}];
            }
            let n=[-d[1]/len,d[0]/len];const mid=[(a[0]+b[0])/2,(a[1]+b[1])/2];
            if(dot(n,sub(mid,centre))<0) n=n.map(v=>-v);
            const exterior=Math.max(...pts.map(p=>dot(sub(p.slice(0,2),a.slice(0,2)),n)))<=1e-6;
            return [{c,i,path,closed:false,n,a,b,mid,exterior,distance:dot(n,sub(mid,centre)),depth:(a[2]+b[2])/2}];
          });
          const auto=q=>g.candidates.auto==='lower'?-q.mid[1]:q.distance;
          const score=q=>g.position==='top'?q.mid[1]:g.position==='bottom'?-q.mid[1]:g.position==='left'?-q.mid[0]:g.position==='right'?q.mid[0]:auto(q);
          const rounded=(v,n)=>Math.round(v*Math.pow(10,n))/Math.pow(10,n);
          choices.sort((a,b)=>Number(b.exterior)-Number(a.exterior) || rounded(score(b)/layout.ppm,8)-rounded(score(a)/layout.ppm,8) || rounded(a.depth,12)-rounded(b.depth,12) || a.i-b.i);
          let q=choices[0];
          if(typeof g.position==='number' || /^[1-4]$/.test(g.position)) q=choices.find(q=>q.i===Number(g.position)-1);
          if(g.secondary && q) {
            const primary=q;
            q=choices.slice().sort((a,b)=>Number(b.exterior)-Number(a.exterior) || Math.hypot(...sub(b.mid,primary.mid))-Math.hypot(...sub(a.mid,primary.mid)))[0];
          }
          if(!q) return;
          const straight=q.path.length===2, values=arr(g.values), labels=arr(g.labels), d=sub(q.b,q.a);
          const ticks=arr(q.c.ticks).map(project), before=arr(q.c.before).map(project), after=arr(q.c.after).map(project);
          // A curved axis turns its ticks with the local direction, away from the frame centre.
          const normalAt=i=>{
            if(straight || !before[i]) return q.n;
            const t=sub(after[i],before[i]), len=Math.hypot(t[0],t[1]) || 1;
            let n=[-t[1]/len,t[0]/len];
            if(dot(n,sub(ticks[i].slice(0,2),centre))<0) n=n.map(v=>-v);
            return n;
          };
          const lo=(g.cap==='lower' || g.cap==='both') && values.length ? Math.min(...values) : 0;
          const hi=(g.cap==='upper' || g.cap==='both') && values.length ? Math.max(...values) : 1;
          if(straight) line(q.a.map((v,i)=>v+d[i]*lo),q.a.map((v,i)=>v+d[i]*hi),g.line);
          else polyline(q.path,g.line);
          const titleSize=arr(g.title_size).flat(), sizes=g.label_size || [];
          const sizeAt=i => Array.isArray(sizes[0]) ? sizes[i] : [sizes[i],sizes[i+values.length]];
          const tangent=straight ? norm(d.slice(0,2)) : null;
          // Labels stand in dodge rows as deep as the deepest label along the normal.
          const depths=values.map((value,i)=>{const size=sizeAt(i), n=normalAt(i);return Math.abs(n[0])*size[0]+Math.abs(n[1])*size[1];});
          const step=(depths.length ? Math.max(...depths) : 0)+g.gap/2, band=depths.length ? g.dodge*step-g.gap/2 : 0;
          const candidates=values.map((value,i)=>{
            const a=ticks[i], size=sizeAt(i), n=normalAt(i);
            const offset=g.tick_length+g.gap+depths[i]/2+(i%g.dodge)*step;
            const xy=a.slice(0,2).map((v,j)=>v+n[j]*offset*layout.ppm);
            line(a,a.slice(0,2).map((v,j)=>v+n[j]*g.tick_length*layout.ppm),g.ticks);
            const w=size[0]*layout.ppm,h=size[1]*layout.ppm;
            return {i,xy,box:[xy[0]-w/2,xy[1]-h/2,xy[0]+w/2,xy[1]+h/2],end:tangent?dot(xy,tangent)+(Math.abs(tangent[0])*w+Math.abs(tangent[1])*h)/2:i};
          });
          if(g.minor_ticks) arr(q.c.minor).map(project).forEach(a=>line(a,a.slice(0,2).map((v,j)=>v+q.n[j]*g.tick_length/2*layout.ppm),g.ticks));
          candidates.sort((a,b)=>a.end-b.end || a.i-b.i).forEach(c=>{
            const b=c.box,gap=(g.check_overlap ? 1 : 0.5)*layout.ppm;
            if(b.some(v=>!Number.isFinite(v))) return;
            if(g.thin && (b[0]<0 || b[1]<0 || b[2]>width || b[3]>height || occupied.some(a=>a[0]<b[2]+gap && a[2]>b[0]-gap && a[1]<b[3]+gap && a[3]>b[1]-gap))) return;
            occupied.push(b);text(labels[c.i],c.xy,g.text,g.angle,0.5,0.5);
          });
          const title=arr(g.title)[0];
          if(title) {
            const th=Array.isArray(titleSize[0]) ? titleSize[0][1] : titleSize[1];
            const offset=(g.tick_length+g.gap+(g.text.visible ? band : 0)+g.title_gap+th/2)*layout.ppm;
            let anchor=q.mid, n=q.n, angle=0;
            if(q.c.title_at) {
              anchor=project(q.c.title_at);n=norm(sub(anchor.slice(0,2),centre));
            } else if(q.closed) {
              anchor=[q.mid[0],Math.min(q.mid[1],...pts.map(p=>p[1]))];n=[0,-1];
            } else {
              if(!straight) {
                const offsets=q.path.map(p=>dot(sub(p.slice(0,2),q.a.slice(0,2)),n)), side=Math.abs(Math.max(...offsets))>=Math.abs(Math.min(...offsets))?1:-1;
                const bulge=Math.max(0,...offsets.map(v=>side*v));n=n.map(v=>side*v);anchor=q.mid.map((v,j)=>v+n[j]*bulge);
              }
              angle=Math.atan2(d[1],d[0])/rad;if(angle>90)angle-=180;if(angle< -90)angle+=180;
            }
            text(title,anchor.slice(0,2).map((v,j)=>v+n[j]*offset),g.title_style,angle,0.5,0.5);
          }
        });
      }
      function publish() {
        const snapshot=JSON.parse(JSON.stringify(camera));
        el.ggplot3dCamera=snapshot;
        el.dispatchEvent(new CustomEvent('ggplot3d:camera',{detail:snapshot}));
        if(window.Shiny && el.id) window.Shiny.setInputValue(el.id+'_camera',snapshot,{priority:'event'});
        const n=x=>Number(x.toFixed(6));
        const vector=v=>'c('+v.map(n).join(', ')+')';
        const shear=arr(camera.shear), oblique=camera.projection==='oblique' ? ', oblique = c(angle = '+n(Math.atan2(shear[1],shear[0])/rad)+', ratio = '+n(Math.hypot(shear[0],shear[1]))+')' : '';
        const vertical=(camera.up || 3)!==3 ? ', up = "'+'xyz'[camera.up-1]+'"' : '';
        code.value='coord_3d(azimuth = '+n(camera.azimuth)+', elevation = '+n(camera.elevation)+',\n  roll = '+n(camera.roll)+', zoom = '+n(camera.zoom)+', projection = "'+camera.projection+'"'+oblique+vertical+',\n  frame = "'+data.framing.frame+'", aspect = '+vector(camera.aspect)+',\n  target = '+vector(camera.target)+', fov = '+n(camera.fov)+
          (camera.distance==null?'':', distance = '+n(camera.distance))+
          (camera.near==null?'':', near = '+n(camera.near))+(camera.far==null?'':', far = '+n(camera.far))+')';
      }
      function draw() {
        frameRequest=null;if(!data) return;
        basis=cameraBasis(camera,data.viewport,framing);
        const vp=data.viewport,ppm=Math.min(width/vp.outer_width_mm,height/vp.outer_height_mm);
        layout={ppm,ox:(width-vp.outer_width_mm*ppm)/2+data.framing.origin[0]*ppm,
          oy:(height-vp.outer_height_mm*ppm)/2+data.framing.origin[1]*ppm,iw:vp.width_mm*ppm,ih:vp.height_mm*ppm};
        gl.disable(gl.SCISSOR_TEST);
        gl.viewport(0,0,canvas.width,canvas.height);gl.clearColor(...data.background);gl.clearDepth(1);
        gl.clear(gl.COLOR_BUFFER_BIT|gl.DEPTH_BUFFER_BIT);
        ctx.setTransform(labels.width/width,0,0,labels.height/height,0,0);ctx.clearRect(0,0,width,height);
        ['target','scale','right','up','forward'].forEach(k=>gl.uniform3fv(uniforms[k[0].toUpperCase()+k.slice(1)],k==='target'?camera.target:basis[k]));
        ['distance','near','far','base'].forEach(k=>gl.uniform1f(uniforms[k[0].toUpperCase()+k.slice(1)],basis[k]));
        gl.uniform1f(uniforms.Perspective,basis.perspective?1:0);
        gl.uniform2f(uniforms.Size,width,height);gl.uniform2f(uniforms.Origin,layout.ox,layout.oy);
        gl.uniform2f(uniforms.Pixels,ppm,ppm);gl.uniform2f(uniforms.Inner,layout.iw,layout.ih);
        if(camera.clip) {
          gl.enable(gl.SCISSOR_TEST);const ratio=canvas.width/width;
          gl.scissor(Math.round(layout.ox*ratio),Math.round(layout.oy*ratio),Math.round(layout.iw*ratio),Math.round(layout.ih*ratio));
        }
        gl.enable(gl.BLEND);gl.blendFuncSeparate(gl.SRC_ALPHA,gl.ONE_MINUS_SRC_ALPHA,gl.ONE,gl.ONE_MINUS_SRC_ALPHA);
        gl.depthFunc(gl.LEQUAL);
        const visible=primitives.filter(p=>!p.furniture || furnitureVisible(p));
        const depth=p=>p.points.reduce((s,v)=>s+project(v)[2]/p.points.length,0);
        opaqueFaces=visible.filter(p=>p.kind===0 && p.partition===1 && colour(p)[3]>=1);
        [0,1,2].forEach(part=>{
          const group=visible.filter(p=>p.partition===part && p.kind!==3);
          if(part===1) gl.enable(gl.DEPTH_TEST);else gl.disable(gl.DEPTH_TEST);
          gl.depthMask(part===1);
          group.filter(p=>colour(p)[3]>=1).forEach(paint);
          gl.depthMask(false);
          group.filter(p=>colour(p)[3]<1).sort((a,b)=>depth(b)-depth(a)).forEach(paint);
        });
        gl.depthMask(true);gl.finish();
        visible.filter(p=>p.kind===3 && visiblePoint(p.points[0])).forEach(p=>{
          if(p.partition===1 && !anchorVisible(p.points[0])) return;
          const m=p.material,t=p.table,i=p.source;
          text(m.label,project(p.points[0]),{visible:true,colour:colour(p),size:m.fontsize,family:m.family,face:m.fontface},t.angle[i],t.hjust[i],t.vjust[i]);
        });
        axes();publish();el.dataset.rendered='true';el.dataset.renderer='webgl';
      }
      function schedule() {if(!frameRequest) frameRequest=requestAnimationFrame(draw);}
      function pathFrame(k) {
        const p=data.path,at=key=>arr(p[key])[k];
        pathIndex=k;
        Object.assign(camera,{azimuth:at('azimuth'),elevation:at('elevation'),roll:at('roll'),zoom:at('zoom'),
          fov:at('fov'),projection:at('projection'),distance:at('distance')>0?at('distance'):null,
          target:[at('target_x'),at('target_y'),at('target_z')],shear:[at('shear_x'),at('shear_y')]});
        framing=Object.assign({},data.framing,{zoom_scale:p.zoom_scale});
        schedule();
      }
      function stop() {
        if(playback) cancelAnimationFrame(playback.request);
        playback=null;play.textContent='Play';
      }
      function tick(now) {
        const n=arr(data.path.azimuth).length;
        let k=Math.floor((now-playback.start)/1000/data.path.step);
        if(data.path.loop) k%=n;
        else if(k>=n-1) {pathFrame(n-1);stop();pathIndex=0;return;}
        pathFrame(k);playback.request=requestAnimationFrame(tick);
      }
      function start() {
        if(!data || !data.path) return;
        stop();playback={start:performance.now()-pathIndex*data.path.step*1000};
        play.textContent='Pause';playback.request=requestAnimationFrame(tick);
      }
      play.addEventListener('click',()=>playback?stop():start());
      function resize(w,h) {
        width=w;height=h;const ratio=window.devicePixelRatio || 1;
        [canvas,labels].forEach(c=>{c.width=Math.round(w*ratio);c.height=Math.round(h*ratio);});schedule();
      }
      const observer=new ResizeObserver(entries=>{
        const box=entries[0].contentRect;
        if(box.width>0 && box.height>0 && (Math.abs(box.width-width)>0.5 || Math.abs(box.height-height)>0.5)) resize(box.width,box.height);
      });
      observer.observe(el);
      const pointers=new Map();let pinch;
      canvas.addEventListener('pointerdown',e=>{
        stop();canvas.focus();canvas.setPointerCapture(e.pointerId);pointers.set(e.pointerId,[e.clientX,e.clientY]);
        const ps=Array.from(pointers.values());pinch=ps.length===2?Math.hypot(...sub(ps[0],ps[1])):null;
      });
      canvas.addEventListener('pointermove',e=>{
        if(!data || !pointers.has(e.pointerId))return;
        const old=pointers.get(e.pointerId);pointers.set(e.pointerId,[e.clientX,e.clientY]);
        if(pointers.size===1) move({azimuth:-(e.clientX-old[0])*0.5,elevation:(e.clientY-old[1])*0.5});
        else {const ps=Array.from(pointers.values()),d=Math.hypot(...sub(ps[0],ps[1]));if(pinch)move({zoom:d/pinch});pinch=d;}
      });
      function release(e){pointers.delete(e.pointerId);pinch=null;}
      canvas.addEventListener('pointerup',release);canvas.addEventListener('pointercancel',release);
      canvas.addEventListener('wheel',e=>{if(!data)return;e.preventDefault();move({zoom:Math.exp(-Math.max(-100,Math.min(100,e.deltaY))*0.001)});},{passive:false});
      canvas.addEventListener('keydown',e=>{
        if(!data)return;
        const actions={ArrowLeft:{azimuth:-5},ArrowRight:{azimuth:5},ArrowUp:{elevation:5},ArrowDown:{elevation:-5},'+':{zoom:1.1},'=':{zoom:1.1},'-':{zoom:1/1.1}};
        if(actions[e.key]){e.preventDefault();move(actions[e.key]);}
      });
      reset.addEventListener('click',()=>{if(data) peers().forEach(v=>v.reset());});
      report.addEventListener('click',()=>{publish();code.hidden=!code.hidden;if(!code.hidden){code.focus();code.select();}});
      resize(width,height);
      return {
        renderValue: function(x) {
          if(x.schema_version!==2 || x.scene.schema_version!==1) throw Error('Unsupported ggplot3d scene schema');
          buffers.forEach(b=>gl.deleteBuffer(b));buffers=[];
          stop();pathIndex=0;data=x;framing=x.framing;play.hidden=!x.path;
          camera=JSON.parse(JSON.stringify(x.camera));initial=JSON.parse(JSON.stringify(camera));
          joinLink(x.link);el.dataset.link=link || '';
          primitives=packetPrimitives(x.scene,false).concat(packetPrimitives(x.furniture,true));
          primitives.filter(p=>p.kind===0).forEach(p=>{
            p.buffer=gl.createBuffer();buffers.push(p.buffer);gl.bindBuffer(gl.ARRAY_BUFFER,p.buffer);
            gl.bufferData(gl.ARRAY_BUFFER,new Float32Array(p.triangles.flatMap(v=>[...v,0,0])),gl.STATIC_DRAW);
          });
          el.ggplot3d={getCamera:()=>JSON.parse(JSON.stringify(camera)),
            getProjectedBox:()=>arr(data.bounds).map(project),
            setCamera:values=>{Object.assign(camera,values);schedule();},
            turn:move,play:start,pause:stop,setPathFrame:pathFrame,playing:()=>playback!==null};
          draw();
        }, resize
      };
    }
  });
})();
