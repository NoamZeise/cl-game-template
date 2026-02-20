#version 330

layout (location = 0) in vec2 pos;

uniform mat4 model;
uniform mat4 view;
uniform mat4 projection;

out vec2 fuv;

void main() {
  vec4 world = model * vec4(pos, 0, 1);
  fuv = pos;
  gl_Position = projection * view * world;
}
