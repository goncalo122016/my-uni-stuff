#include <stdio.h>

#ifdef __APPLE__
#include <GLUT/glut.h>
#else
#include <GL/glut.h>
#endif

#define _USE_MATH_DEFINES
#include <math.h>
#include <time.h>

float alfa = 0.0f, beta = 0.5f, radius = 100.0f;
float camX, camY, camZ;


void spherical2Cartesian() {

	camX = radius * cos(beta) * sin(alfa);
	camY = radius * sin(beta);
	camZ = radius * cos(beta) * cos(alfa);
}


void changeSize(int w, int h) {

	// Prevent a divide by zero, when window is too short
	// (you cant make a window with zero width).
	if(h == 0)
		h = 1;

	// compute window's aspect ratio 
	float ratio = w * 1.0 / h;

	// Set the projection matrix as current
	glMatrixMode(GL_PROJECTION);
	// Load Identity Matrix
	glLoadIdentity();
	
	// Set the viewport to be the entire window
    glViewport(0, 0, w, h);

	// Set perspective
	gluPerspective(45.0f ,ratio, 1.0f ,1000.0f);

	// return to the model view matrix mode
	glMatrixMode(GL_MODELVIEW);
}

float gerarAleatorio() {
    return ((float)rand() / RAND_MAX) * 200.0f - 100.0f;
}

void renderScene(void) {
	srand(10);

	// clear buffers
	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

	// set the camera
	glLoadIdentity();
	gluLookAt(camX, camY, camZ,
		0.0, 0.0, 0.0,
		0.0f, 1.0f, 0.0f);

	glColor3f(0.2f, 0.8f, 0.2f);
	glBegin(GL_TRIANGLES);
		glVertex3f(100.0f, 0, -100.0f);
		glVertex3f(-100.0f, 0, -100.0f);
		glVertex3f(-100.0f, 0, 100.0f);

		glVertex3f(100.0f, 0, -100.0f);
		glVertex3f(-100.0f, 0, 100.0f);
		glVertex3f(100.0f, 0, 100.0f);
	glEnd();
	
	// put code to draw scene in here
	int nTrees = 100;
	for (int i = 0; i < nTrees; i++) {
		// Gerar coordenadas aleatórias para a posição da árvore
		float x = gerarAleatorio();
		float z = gerarAleatorio();
		while(x * x + z * z <= 2500) {
			x = gerarAleatorio();
			z = gerarAleatorio();
		}

		// Desenhar a árvore
		glPushMatrix();
		
		glTranslatef(x, 0, z);
		glScalef(0.1f, 0.1f, 0.1f);
		glColor3f(0.55f, 0.27f, 0.07f); // Castanho
		// TRONCO
		glRotatef(-90, 1, 0, 0);
		GLUquadric* quadric = gluNewQuadric();
		gluCylinder(quadric, 5.0f, 5.0f, 50.0f, 10, 10);
		gluDeleteQuadric(quadric);
		glRotatef(90, 1, 0, 0); // Rotacionar de volta para a posição original

		// FOLHAS
		glTranslatef(0, 50.0f, 0); // Mover para o topo do tronco
		glRotatef(-90, 1, 0, 0);
		glColor3f(0.0f, 1.0f, 0.0f); // Verde
		glutSolidCone(50.0f, 100.0f, 10, 10);
		glTranslatef(0.0f, 0, 50.0f); // Mover um pouco para cima para o segundo cone
		glutSolidCone(40.0f, 80.0f, 10, 10);
		glPopMatrix();

		glColor3f(1.0f, 0.0f, 0.0f); // Vermelho
		glutSolidTorus(5.0f, 10.0f, 10, 10);
	}

	// 8 teapots num círculo
	for(int j = 0; j < 8; j++) {
		glPushMatrix();
		float angle = j * (360.0f / 8);
		float x = 35.0f * cos(angle * M_PI / 180.0f);
		float z = 35.0f * sin(angle * M_PI / 180.0f);
		glTranslatef(x, 10.0f, z);
		glRotatef(-angle, 0, 1, 0);
		glutSolidTeapot(5.0f);
		glPopMatrix();
	}
	
	glutSwapBuffers();
}


void processKeys(unsigned char c, int xx, int yy) {

// put code to process regular keys in here

}


void processSpecialKeys(int key, int xx, int yy) {

	switch (key) {

	case GLUT_KEY_RIGHT:
		alfa -= 0.1; break;

	case GLUT_KEY_LEFT:
		alfa += 0.1; break;

	case GLUT_KEY_UP:
		beta += 0.1f;
		if (beta > 1.5f)
			beta = 1.5f;
		break;

	case GLUT_KEY_DOWN:
		beta -= 0.1f;
		if (beta < -1.5f)
			beta = -1.5f;
		break;

	case GLUT_KEY_PAGE_DOWN: radius -= 1.0f;
		if (radius < 1.0f)
			radius = 1.0f;
		break;

	case GLUT_KEY_PAGE_UP: radius += 1.0f; break;
	}
	spherical2Cartesian();
	glutPostRedisplay();

}


void printInfo() {

	printf("Vendor: %s\n", glGetString(GL_VENDOR));
	printf("Renderer: %s\n", glGetString(GL_RENDERER));
	printf("Version: %s\n", glGetString(GL_VERSION));

	printf("\nUse Arrows to move the camera up/down and left/right\n");
	printf("Home and End control the distance from the camera to the origin");
}

int main(int argc, char **argv) {	
// init GLUT and the window
	glutInit(&argc, argv);
	glutInitDisplayMode(GLUT_DEPTH|GLUT_DOUBLE|GLUT_RGBA);
	glutInitWindowPosition(100,100);
	glutInitWindowSize(800,800);
	glutCreateWindow("CG@DI-UM");
		
// Required callback registry 
	glutDisplayFunc(renderScene);
	glutReshapeFunc(changeSize);
	
// Callback registration for keyboard processing
	glutKeyboardFunc(processKeys);
	glutSpecialFunc(processSpecialKeys);

//  OpenGL settings
	glEnable(GL_DEPTH_TEST);
	glEnable(GL_CULL_FACE);

	spherical2Cartesian();

	printInfo();

// enter GLUT's main cycle
	glutMainLoop();
	
	return 1;
}
