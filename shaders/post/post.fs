#version 330

in vec2 uv;
out vec4 colour;

uniform sampler2D screen_tex;

void main() { 
  if(uv.x > 1 || uv.y > 1 || uv.x < 0 || uv.y < 0) discard;
  colour = texture(screen_tex, uv);
}
