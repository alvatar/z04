#version 100

attribute vec2 position;
attribute vec2 texCoord;
uniform mat4 perspectiveMatrix;

varying vec2 colorCoord;

void main()
{
    colorCoord = texCoord;
    gl_Position = perspectiveMatrix * vec4(position, 0.0, 1.0);
}