attribute vec4 position;
uniform mat4 perspectiveMatrix;

void main()
{
    gl_Position = perspectiveMatrix * position;
    //gl_Position = vec4(position.xyz, 1.0);
}