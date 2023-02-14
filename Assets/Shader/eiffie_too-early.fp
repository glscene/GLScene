// Too early? by eiffie
// License Creative Commons Attribution-NonCommercial-ShareAlike 3.0 Unported License.

uniform vec3      iResolution;           // viewport resolution (in pixels)
uniform float     iGlobalTime;           // shader playback time (in seconds)

#define time iGlobalTime*0.75
#define size iResolution

float focalDistance=2.5,aperature=0.03,fudgeFactor=1.0;
#define TAO 6.283
vec2 rotate(vec2 v, float angle) {return cos(angle)*v+sin(angle)*vec2(v.y,-v.x);}
vec2 Kaleido(vec2 v){float w=floor(.5+3.5015*atan(v.x,-v.y))*0.28559;return rotate(v,w);}
float Cone(in vec4 z, float radius, float height){return (max(abs(z.y)-height,length(z.xz))-radius*clamp(height-abs(z.y),0.0,height))/z.w;}

float DE(in vec3 z0)
{	
	vec4 z=vec4(z0,1.0);
	z.xy=rotate(z.xy,z.z*0.4);
	z.xy=clamp(z.xy,-1.25,1.25)*2.0-z.xy;
	float r=length(z.xy);
	z.xy=rotate(z.xy,r*0.4);
	z.z=abs(mod(z.z,0.5)-0.25)-0.125;
	z.xy=Kaleido(z.xy);
	z.yz=rotate(z.yz,(0.5-r)*0.15);
	float f=max(0.8-abs(-z0.z-2.0),0.0);
	float d=min(r-0.025,Cone(z,0.025,1.0-f*f));
	float dB=length(z0+vec3(-1.0,-1.0,-0.47))-0.6;
	dB=min(dB,length(z0+vec3(1.18,-1.24,0.3))-0.6+cos(z0.y*31.5)*0.02);
	vec3 p=z0+vec3(-1.08,0.72,0.46);
	dB=min(dB,length(p)-0.5);
	p.xy=mat2(0.913,-0.4078,0.4078,0.913)*p.xy;
	dB=min(dB,max(abs(p.y-0.55)-0.05,length(p.xz)-0.1+sin(atan(p.x,p.z)*8.0)*0.01));
	p.y-=0.69;
	dB=min(dB,max(abs(p.y)-0.25,length(p.xz+vec2(p.y*p.y))-0.0025));
	return max(min(d,dB),-z0.z-2.0);
}
vec3 mcol;
float CE(vec3 z0){//same for coloring
	vec4 z=vec4(z0,1.0);
	z.xy=rotate(z.xy,z.z*0.4);
	z.xy=clamp(z.xy,-1.25,1.25)*2.0-z.xy;
	float r=length(z.xy);
	z.xy=rotate(z.xy,r*0.4);
	z.z=abs(mod(z.z,0.5)-0.25)-0.125;
	z.xy=Kaleido(z.xy);
	z.yz=rotate(z.yz,(0.5-r)*0.15);
	float d=min(r-0.025,Cone(z,0.025,1.0));
	float dB1=length(z0+vec3(-1.0,-1.0,-0.47))-0.6;
	float dB2=length(z0+vec3(1.18,-1.24,0.3))-0.6+cos(z0.y*31.5)*0.02;
	vec3 p=z0+vec3(-1.08,0.72,0.46);
	float dB3=length(p)-0.5;
	p.xy=mat2(0.913,-0.4078,0.4078,0.913)*p.xy;
	float dB4=max(abs(p.y-0.55)-0.05,length(p.xz)-0.1+sin(atan(p.x,p.z)*8.0)*0.01);
	p.y-=0.69;
	dB4=min(dB4,max(abs(p.y)-0.25,length(p.xz+vec2(p.y*p.y))-0.0025));
	float dB=min(dB1,min(dB2,min(dB3,dB4)));
	if(dB<d){
		if(abs(dB-dB1)<0.001)mcol+=vec3(1.0,0.0,0.0);
		else if(abs(dB-dB2)<0.001)mcol+=vec3(1.0,0.5,0.0);
		else if(abs(dB-dB3)<0.001)mcol+=vec3(1.0,0.0,1.0);
		else mcol+=vec3(1.0);
		d=dB;
	}else{
		mcol+=vec3(0.1,0.3,0.15)*r;
	}
	return d;
}

float pixelSize;
float CircleOfConfusion(float t){//calculates the radius of the circle of confusion at length t
	return max(abs(focalDistance-t)*aperature,pixelSize*(1.0+t));
}
mat3 lookat(vec3 fw,vec3 up){
	fw=normalize(fw);vec3 rt=normalize(cross(fw,normalize(up)));return mat3(rt,cross(rt,fw),fw);
}
float linstep(float a, float b, float t){return clamp((t-a)/(b-a),0.,1.);}// i got this from knighty and/or darkbeam
float rand(vec2 co){// implementation found at: lumina.sourceforge.net/Tutorials/Noise.html
	return fract(sin(dot(co*0.123,vec2(12.9898,78.233))) * 43758.5453);
}

void main() {
	pixelSize=1.0/size.y;
	vec3 ro=vec3(4.18,0.32,-6.43)+vec3(sin(time),sin(time*2.3),cos(time*0.7));
	if(time>60.0)ro=vec3(sin(time*0.2),sin(time*0.3),cos(time*0.2))*6.0;
	vec3 p=vec3((2.0*gl_FragCoord.xy-size.xy)/size.y,5.0);
	vec3 fw=normalize(-ro);
	vec3 rt=normalize(cross(fw,vec3(0.0,1.0,0.0)));
	vec3 up=cross(rt,fw);
	vec3 rd=normalize(p.x*rt+p.y*up+p.z*fw);//normalize(dir);//
	vec3 L=normalize(vec3(0.36,1.0,0.0));
	vec4 col=vec4(0.0);//color accumulator
	ro+=rd*3.7;
	float t=0.0;//distance traveled
	for(int i=1;i<78;i++){//march loop
		if(col.w>0.95 || t>20.0)continue;//bail if we hit a surface or go out of bounds
		float rCoC=CircleOfConfusion(t);//calc the radius of CoC
		float d=DE(ro)+0.25*rCoC;
		if(d<rCoC){//if we are inside add its contribution
			vec3 p=ro-rd*abs(d-rCoC);//back up to border of CoC
			mcol=vec3(0.0);//clear the color trap, collecting color samples with normal deltas
			vec2 v=vec2(rCoC*0.5,0.0);//use normal deltas based on CoC radius
			vec3 N=normalize(vec3(-CE(p-v.xyy)+CE(p+v.xyy),-CE(p-v.yxy)+CE(p+v.yxy),-CE(p-v.yyx)+CE(p+v.yyx)));
			mcol*=0.1666;
			vec3 scol=mcol*(0.7+0.3*dot(N,L));
			scol+=pow(max(0.0,dot(reflect(rd,N),L)),32.0)*vec3(0.7,0.5,0.25);
			if(d<rCoC*0.5 && mcol.r>0.5){//reflect the ray if we hit a bulb "directly enough"
				rd=reflect(rd,N);d=rCoC*0.5;ro=p;t+=1.0;
			}
			float alpha=fudgeFactor*(1.0-col.w)*linstep(-rCoC,rCoC,-d);//calculate the mix like cloud density
			col+=vec4(scol*alpha,alpha);//blend in the new color
		}
		d=abs(fudgeFactor*d*(0.7+0.2*rand(gl_FragCoord.xy*vec2(i))));//add in noise to reduce banding and create fuzz
		ro+=d*rd;//march
		t+=d;
	}//mix in background color
	vec3 scol=vec3(0.7,0.8,0.9);//mix(vec3(0.05,0.2,0.1)+rd*0.025,vec3(0.3,0.4,0.5)+rd*0.1,smoothstep(-0.1,1.0,rd.y+noyz(rd.xy*vec2(25.0,2.0))*0.25));
	col.rgb+=scol*(1.0-clamp(col.w,0.0,1.0));

	gl_FragColor = vec4(clamp(col.rgb,0.0,1.0),1.0);
}