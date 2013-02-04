#version 330

struct light {
    float intensity[20];
    float position;
};


uniform light jam[10];

smooth in vec4 interpColor;

out vec4 outputColor;

void main()
{
	outputColor = interpColor + vec4(jam[4].position, jam[1].intensity[5], 1.0, jam[1].intensity[5]);
}
