#version 450 core

layout (location = 0) in vec3 aPos;
layout (location = 1) in vec3 aColor;
layout (location = 2) in vec2 aUVCoord;

out vec2 texCoord;
out vec3 color;

uniform mat4 model;

void main()
{
    gl_Position = model * vec4(aPos, 1.0);
    texCoord    = vec2(aUVCoord.x, 1.0 - aUVCoord.y);
    color = aColor;
}