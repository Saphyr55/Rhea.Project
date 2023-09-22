#version 450 core

out vec4 FragColor;

in vec2 texCoord;
in vec4 color;

uniform sampler2D uTexture;

void main()
{
    FragColor = texture(uTexture, texCoord) * color;
} 
