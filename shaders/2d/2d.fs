#version 330

in vec2 fuv;

layout(location = 0) out vec4 colour;

uniform vec4 obj_colour;
uniform sampler2D tex;

void main() {
  vec4 c_obj = obj_colour * texture(tex, fuv);

  colour = c_obj;
}
