precision mediump float;

uniform sampler2D colorTexture;

varying vec2 colorCoord;

void main()
{
    gl_FragColor = texture2D(colorTexture, colorCoord);
}