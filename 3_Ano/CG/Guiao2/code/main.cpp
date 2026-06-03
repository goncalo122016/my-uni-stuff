#ifdef __APPLE__
#include <GLUT/glut.h>
#else
#include <GL/glut.h>
#endif

#include <math.h>

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

float tx, ty, tz = 0.0f;
float rx, ry, rz = 0.0f;
float angle = 5.0f;

/*
void renderScene() {

    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

    glLoadIdentity();

    // CÂMARA REAL
    gluLookAt(20,10,20,
              0,0,0,
              0,1,0);


    drawAxis();


    // ESFERA 1
    glTranslatef(0,2,0);

    glColor3f(1,0,0);
    glutWireSphere(1,16,16);

	glTranslatef(0,-2,0);

    // OBJETO QUE REPRESENTA A POSIÇÃO DA CÂMARA DO EXERCÍCIO
    // posição da câmara do exercício:

	glTranslatef(-sqrt(18),2,0);
	glRotatef(45,0,1,0);

    glColor3f(0,0,1);
    glutWireCube(0.5);

    // ESFERA 2
    glTranslatef(3,0,3);

    glColor3f(1,1,0);
    glutWireSphere(1,16,16);

	glutSwapBuffers();
}
*/

void renderScene(void) {

	// clear buffers
	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

	// set the camera
	glLoadIdentity();
	gluLookAt(5.0,5.0,5.0, 
		      0.0,0.0,0.0,
			  0.0f,1.0f,0.0f);

// put axis drawing in here
	glBegin(GL_LINES);
		// X axis in red
		glColor3f(1.0f, 0.0f, 0.0f);
		glVertex3f(0.0f, 0.0f, 0.0f);
		glVertex3f( 100.0f, 0.0f, 0.0f);
		// Y Axis in Green
		glColor3f(0.0f, 1.0f, 0.0f);
		glVertex3f(0.0f, 0.0f, 0.0f);
		glVertex3f(0.0f, 100.0f, 0.0f);
		// Z Axis in Blue
		glColor3f(0.0f, 0.0f, 1.0f);
		glVertex3f(0.0f, 0.0f, 0.0f);
		glVertex3f(0.0f, 0.0f, 100.0f);
	glEnd();


// put the geometric transformations here
	glRotatef(angle, rx, ry, rz);
	glTranslatef(tx, ty, tz);

// put pyramid drawing instructions here
	glBegin(GL_TRIANGLES);
		// Base
		glVertex3f(-1.0f, -1.0f,  1.0f);
		glVertex3f(-1.0f, -1.0f, -1.0f);
		glVertex3f( 1.0f, -1.0f,  1.0f);

		glVertex3f(-1.0f, -1.0f, -1.0f);
		glVertex3f( 1.0f, -1.0f, -1.0f);
		glVertex3f( 1.0f, -1.0f,  1.0f);

		// Lados
		glColor3f(1.0f, 0.0f, 0.0f);
		glVertex3f(-1.0f, -1.0f,  1.0f);
		glVertex3f( 1.0f, -1.0f,  1.0f);
		glVertex3f( 0.0f,  1.0f,  0.0f);

		glColor3f(0.0f, 1.0f, 0.0f);
		glVertex3f( 1.0f, -1.0f,  1.0f);
		glVertex3f( 1.0f, -1.0f, -1.0f);
		glVertex3f( 0.0f,  1.0f,  0.0f);

		glColor3f(0.0f, 0.0f, 1.0f);
		glVertex3f( 1.0f, -1.0f, -1.0f);
		glVertex3f(-1.0f, -1.0f, -1.0f);
		glVertex3f( 0.0f,  1.0f,  0.0f);

		glColor3f(1.0f, 1.0f, 0.0f);
		glVertex3f(-1.0f, -1.0f, -1.0f);
		glVertex3f(-1.0f, -1.0f,  1.0f);
		glVertex3f( 0.0f,  1.0f,  0.0f);
	glEnd();

	// End of frame
	glutSwapBuffers();
}

// write function to process keyboard events
void processKeys(int key, int x, int y) {
	switch (key) {
		case GLUT_KEY_UP:
			tx+=1;
			break;
		case GLUT_KEY_DOWN:
			tz+=1;
			break;
		case GLUT_KEY_RIGHT:
			rx = 0;
			ry = 1;
			rz = 0;
			angle+=5;
			break;
		case GLUT_KEY_LEFT:
			rx = 0;
			ry = 1;
			rz = 0;
			angle-=5;
			break;
	}
	glutPostRedisplay();
}

void processNormalKeys(unsigned char key, int x, int y) {
	switch (key) {
		case 'w':
		case 'W':
			ty+=1;
			break;
		case 's':
		case 'S':
			ty-=1;
			break;
	}
	glutPostRedisplay();
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

	
// put here the registration of the keyboard callbacks
	glutSpecialFunc(processKeys);
	glutKeyboardFunc(processNormalKeys);

//  OpenGL settings
	glEnable(GL_DEPTH_TEST);
	glEnable(GL_CULL_FACE);
	
// enter GLUT's main cycle
	glutMainLoop();
	
	return 1;
}
