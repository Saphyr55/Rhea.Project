#version 450 core

out vec4 FragColor;
in vec2 TexCoord;

uniform vec3 uColor;
uniform sampler2D uTexture;

void main()
{
    FragColor = texture(uTexture, TexCoord);
} 
