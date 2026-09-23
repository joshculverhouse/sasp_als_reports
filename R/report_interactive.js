(function(){
'use strict';
const themes={
 activity:['#74B9A5','#419F83','#00A984','#00765C','#005341'],
 purple:['#B87DB0','#AD65A4','#A8589E','#873B7E','#652C5E'],
 orange:['#E99C6F','#E08049','#EC6725','#C2511C','#963A14'],
 attitudes:['#DA8495','#D95B77','#E62249','#B31C3A','#83152C'],
 types:['#00A984','#A8589E','#EC6725','#E62249','#547B38','#85532E','#783C62']
};
const blues=['#7CA8CA','#6494BA','#4B80AA','#316D99','#1C5A85','#0C4974','#003E69'];
const extended=['#007F65','#A8589E','#C45B20','#CD2348','#657D32','#85532E','#753D65','#397969','#A26B19'];
const good=r=>r&&r.status==='Available'&&Number.isFinite(r.percent);
const ci=r=>false; // Confidence intervals are intentionally not presented.
const pct=v=>Number.isFinite(v)?v.toFixed(1)+'%':'Unavailable';
const esc=s=>String(s).replace(/&/g,'&amp;').replace(/</g,'&lt;').replace(/>/g,'&gt;');
const wrap=s=>esc(s).replace(/(.{1,70})(?:\s|$)/g,'$1<br>').replace(/<br>$/,'');
function colour(spec,area,index,count){
 const england=area==='England';
 if(!england&&(spec.theme==='types'||count>5))return extended[index%extended.length];
 const palette=england?blues:(themes[spec.theme]||themes.activity);
 return palette[count===1?(england?palette.length-1:2):Math.round(index*(palette.length-1)/(count-1))];
}
function bounds(rows,trend){
 const vals=rows.filter(good).map(r=>r.percent);
 if(!vals.length)return [0,100];
 const lo=Math.min(...vals),hi=Math.max(...vals),pad=Math.max(3,Math.max(10,hi-lo)*.1);
 return [trend?Math.max(0,Math.floor((lo-pad)/5)*5):0,Math.min(100,Math.max(10,Math.ceil((hi+pad)/5)*5))];
}
function hover(r){
 if(!r)return 'Unavailable';
 return esc(`${r.year} · ${r.area}`)+'<br>'+esc(`${r.group}: ${r.response}`)+'<br>'+esc(good(r)?pct(r.percent):r.status);
}

function series(spec,areas,groups,trend,response){
 const cats=spec.categories;
 if(!areas.length||!groups.length)return [];
 return areas.flatMap(area=>{
  const rows=spec.rows.filter(r=>r.area===area),england=area==='England';
  if(!trend&&spec.activity_types){
   const rr=groups.map(g=>rows.find(r=>r.year===spec.latest&&r.group===g));
   return [{type:'bar',orientation:'h',name:area,legendgroup:area,x:rr.map(r=>good(r)?r.percent:null),y:groups,
    text:rr.map(r=>good(r)?pct(r.percent):''),textposition:'outside',cliponaxis:false,
    marker:{color:groups.map(g=>colour(spec,area,spec.groups.indexOf(g),spec.groups.length)),pattern:{shape:england?'/':''}},
    customdata:rr.map(r=>hover(r)+(spec.denominator?'<br>Denominator: valid respondents within '+esc(spec.denominator):'')),hovertemplate:'%{customdata}<extra></extra>'}];
  }
  if(!trend){
   return cats.map((cat,i)=>{
    const rr=groups.map(g=>rows.find(r=>r.year===spec.latest&&r.group===g&&r.response===cat));
    return {type:'bar',name:cat+' — '+area,x:groups[0]==='All'?[cat]:groups,y:rr.map(r=>good(r)?r.percent:null),
     text:rr.map(r=>good(r)?pct(r.percent):''),textposition:'outside',cliponaxis:false,
     marker:{color:colour(spec,area,i,cats.length),pattern:{shape:england?'/':''}},
     customdata:rr.map(r=>hover(r)+(spec.denominator?'<br>Denominator: valid respondents within '+esc(spec.denominator):'')),hovertemplate:'%{customdata}<extra></extra>'};
   });
  }
  const overall=spec.groups.length===1&&spec.groups[0]==='All';
  const names=overall?cats:groups;
  return names.map(name=>{
   const index=overall?cats.indexOf(name):spec.groups.indexOf(name),count=overall?cats.length:spec.groups.length;
   const rr=spec.years.map(y=>rows.find(r=>r.year===y&&r.group===(overall?'All':name)&&r.response===(overall?name:response)));
   let last=-1;rr.forEach((r,j)=>{if(good(r))last=j});
   return {type:'scatter',mode:'lines+markers+text',name:name+' — '+area,x:spec.years,y:rr.map(r=>good(r)?r.percent:null),connectgaps:false,
    line:{color:colour(spec,area,index,count),width:2,dash:england?'dash':'solid'},
    marker:{size:7,symbol:england?'square':'circle'},text:rr.map((r,j)=>j===last?pct(r.percent):''),
    textposition:england?'bottom right':'top right',cliponaxis:false,
    customdata:rr.map(r=>hover(r)+(spec.denominator?'<br>Denominator: valid respondents within '+esc(spec.denominator):'')),hovertemplate:'%{customdata}<extra></extra>'};
  });
 });
}
function summary(spec,areas,groups,trend,response){
 if(!areas.length||!groups.length)return 'Select at least one area and one group to display results.';
 return areas.map(area=>{
  const eligible=spec.rows.filter(r=>r.area===area&&groups.includes(r.group)&&(!trend||spec.groups[0]==='All'||r.response===response));
  const latest=eligible.filter(r=>r.year===spec.latest&&good(r)),previous=spec.years[spec.years.length-2];
  if(!latest.length)return `No publishable estimates are available for ${area} in ${spec.latest}.`;
  const lines=latest.map(r=>{
   let s=(r.group==='All'?r.response:r.group+' — '+r.response)+': '+pct(r.percent);
   if(trend&&previous){
    const p=eligible.find(z=>z.year===previous&&z.group===r.group&&z.response===r.response&&good(z));
    if(p){const change=r.percent-p.percent;s+=`; ${change>=0?'+':''}${change.toFixed(1)} percentage points from ${previous} (${pct(p.percent)})`;}
    else s+=`; comparison with ${previous} unavailable`;
   }
   return s+'.';
  });
  return `${area}, ${spec.latest}. `+(spec.denominator?'Percentages within each '+spec.denominator+'; eligible respondents with valid answers to both questions. ':'')+lines.join(' ');
 }).join('\n\n')+(trend?'\n\nChanges are descriptive; no significance test has been applied.':'');
}
function activate(panel){
 if(panel._als)return;
 const spec=JSON.parse(panel.querySelector('script.als-spec').textContent),controls=panel.querySelector('.als-controls');
 let active=0,ready=Promise.resolve(),draw=()=>{},queue=Promise.resolve();
 function checkboxes(title,values,initial){
  const field=document.createElement('fieldset'),legend=document.createElement('legend');legend.textContent=title;field.appendChild(legend);
  const inputs=values.map(value=>{
   const label=document.createElement('label'),input=document.createElement('input');input.type='checkbox';input.value=value;input.checked=initial.includes(value);
   label.appendChild(input);label.appendChild(document.createTextNode(' '+value));field.appendChild(label);input.onchange=()=>draw();return input;
  });
  controls.appendChild(field);return {field,inputs,selected:()=>inputs.filter(i=>i.checked).map(i=>i.value)};
 }
 const areas=checkboxes('Area',spec.areas,[spec.areas[0]]);
 const groups=checkboxes(spec.activity_types?'Activity type':'Group',spec.groups,spec.groups);
 if(spec.groups.length===1&&spec.groups[0]==='All')groups.field.hidden=true;
 for(const [caption,value] of [['Select all',true],['Clear all',false]]){
  const b=document.createElement('button');b.type='button';b.textContent=caption;groups.field.appendChild(b);
  b.onclick=()=>{groups.inputs.forEach(i=>i.checked=value);draw()};
 }
 const response=document.createElement('select');response.setAttribute('aria-label','Trend response category');
 spec.categories.forEach(c=>response.add(new Option(c,c)));if(spec.categories.includes('Active'))response.value='Active';controls.appendChild(response);response.onchange=()=>draw();
 const download=document.createElement('button');download.type='button';download.textContent='Download PNG';controls.appendChild(download);
 const graph=panel.querySelector('.als-interactive-chart'),text=panel.querySelector('.als-summary'),notice=panel.querySelector('.als-chart-error');
 function selectionName(){return areas.selected().join('_and_')+'_'+(active===1?'trend':spec.latest.replace('/','_'))+(spec.denominator?'_within_'+spec.denominator.replace(/ /g,'_'):'');}
 draw=()=>{
  if(active===2)return;
  const trend=active===1,ar=areas.selected(),gr=groups.selected(),range=bounds(spec.rows,trend);
  response.hidden=!trend||spec.groups[0]==='All'||spec.activity_types;
  const data=series(spec,ar,gr,trend,response.value);
  const heading=spec.label+' — '+(ar.join(' and ')||'No area selected')+(trend?' · Trend':' · '+spec.latest)+(spec.denominator?' · Within '+spec.denominator:'');
  const layout={title:{text:wrap(heading),font:{size:16}},barmode:'group',height:560,autosize:true,
   margin:{l:65,r:90,t:130,b:125},paper_bgcolor:'white',plot_bgcolor:'white',font:{family:'Arial, sans-serif',color:'#283344'},
   xaxis:{type:'category',title:{text:trend?'Survey year':''},automargin:true},
   yaxis:{title:{text:spec.denominator?'% within '+spec.denominator:'Percent (%)'},range,ticksuffix:'%',automargin:true,gridcolor:'#e5e9ef'},
   legend:{orientation:'h',y:-.24,x:0},hovermode:'closest',uniformtext:{minsize:10,mode:'hide'}};
  if(!trend&&spec.activity_types){
   layout.xaxis={title:{text:spec.denominator?'% within '+spec.denominator:'Percent (%)'},range,ticksuffix:'%',gridcolor:'#e5e9ef',automargin:true};
   layout.yaxis={type:'category',autorange:'reversed',automargin:true,categoryorder:'array',categoryarray:gr};
   layout.margin.l=245;layout.height=Math.max(560,200+gr.length*48);
  }
  text.textContent=summary(spec,ar,gr,trend,response.value);
  download.disabled=!ar.length||!gr.length||!data.some(t=>(trend||!spec.activity_types?t.y:t.x).some(v=>v!==null));
  if(!ar.length||!gr.length)layout.annotations=[{text:'Select at least one area and one group',xref:'paper',yref:'paper',x:.5,y:.5,showarrow:false}];
  if(typeof Plotly==='undefined'){notice.hidden=false;notice.textContent='Plotly could not load. The data table remains available.';download.disabled=true;return;}
  // Queue renders so rapid checkbox changes cannot race an unfinished Plotly draw.
  queue=queue.catch(()=>{}).then(()=>Plotly.react(graph,data,layout,{responsive:true,displaylogo:false,displayModeBar:true,
   toImageButtonOptions:{format:'png',filename:'active_lives_'+ar.join('_and_')+'_'+(trend?'trend':spec.latest.replace('/','_')),width:1400,height:900,scale:2}}));
  ready=queue;
 };
 download.onclick=async()=>{await ready;await Plotly.downloadImage(graph,{format:'png',filename:'active_lives_'+selectionName(),width:1400,height:900,scale:2})};
 panel.querySelectorAll('.als-tabs button').forEach((b,i)=>b.onclick=()=>{
  active=i;panel.querySelectorAll('.als-tabs button').forEach((z,j)=>{z.classList.toggle('selected',j===i);z.setAttribute('aria-pressed',j===i)});
  panel.querySelector('.als-interactive-body').hidden=i===2;panel.querySelector('.als-data-body').hidden=i!==2;draw();
 });
 panel._als={draw};draw();
}
function init(){document.querySelectorAll('details.als-plotly-panel').forEach(p=>{
 p.addEventListener('toggle',()=>{if(p.open){if(p._als)p._als.draw();else activate(p)}});if(p.open)activate(p);
});}
if(typeof module!=='undefined')module.exports={bounds,series,summary,good,ci,colour};
if(typeof document!=='undefined'){if(document.readyState==='loading')document.addEventListener('DOMContentLoaded',init);else init();}
})();

function alsDenominator(select){
 const root=select.closest('.als-dual'),views=Array.from(root.children).filter(x=>x.classList.contains('als-view'));
 const old=views.find(v=>!v.hidden),tab=old?Array.from(old.querySelectorAll('.als-tabs button')).findIndex(b=>b.classList.contains('selected')):0,selected=old?Array.from(old.querySelectorAll('.als-controls fieldset:first-child input:checked')).map(x=>x.value):[];
 views.forEach((v,i)=>{v.hidden=i!==Number(select.value);const p=v.querySelector('.als-plotly-panel');p.open=!v.hidden;});
 // Let the panel toggle handler initialise the selected view before syncing areas.
 setTimeout(()=>{const p=views[Number(select.value)].querySelector('.als-plotly-panel');if(p._als){p.querySelectorAll('.als-controls fieldset:first-child input').forEach(x=>x.checked=selected.includes(x.value));const b=p.querySelectorAll('.als-tabs button')[Math.max(0,tab)];if(b)b.click();p._als.draw();}},0);
}
if(typeof document!=='undefined')document.addEventListener('click',function(event){
 const a=event.target.closest('a[href^="#"]');if(!a)return;
 const target=document.getElementById(a.getAttribute('href').slice(1));if(!target)return;
 let p=target.parentElement;while(p){if(p.tagName==='DETAILS')p.open=true;p=p.parentElement;}
});
if(typeof document!=='undefined'){
 const redrawDual=()=>document.querySelectorAll('.als-dual').forEach(root=>root.addEventListener('toggle',()=>{
  if(root.open)root.querySelectorAll('.als-view:not([hidden]) .als-plotly-panel').forEach(p=>{if(p._als)p._als.draw();});
 }));
 if(document.readyState==='loading')document.addEventListener('DOMContentLoaded',redrawDual);else redrawDual();
}
