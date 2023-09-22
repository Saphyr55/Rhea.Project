#version 450 core

layout (location = 0) in vec3 aPos;
layout (location = 1) in vec4 aColor;
layout (location = 2) in vec2 aUVCoord;

out vec2 texCoord;
out vec4 color;

void main()
{
    gl_Position = vec4(aPos, 1.0);
    texCoord    = vec2(aUVCoord.x, 1.0 - aUVCoord.y);
    color = aColor;
}