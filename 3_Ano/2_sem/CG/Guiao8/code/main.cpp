#include <stdlib.h>
#define _USE_MATH_DEFINES
#include <math.h>

#ifdef __APPLE__
#include <GLUT/glut.h>
#else
#include <GL/glut.h>
#endif


float camX = 0, camY, camZ = 5;
int startX, startY, tracking = 0;

int alpha = 0, beta = 0, r = 5;

#define POINT_COUNT 5
float p[POINT_COUNT][3] = {{-1,-1,0},{-1,1,0},{1,1,0},{0,0,0},{1,-1,0}};

static float prevY[3] = { 0.0f, 1.0f, 0.0f };  // "up" inicial

void buildRotMatrix(float *x, float *y, float *z, float *m) {
	m[0] = x[0]; m[1] = x[1]; m[2] = x[2]; m[3] = 0;
	m[4] = y[0]; m[5] = y[1]; m[6] = y[2]; m[7] = 0;
	m[8] = z[0]; m[9] = z[1]; m[10] = z[2]; m[11] = 0;
	m[12] = 0; m[13] = 0; m[14] = 0; m[15] = 1;
}

void cross(float *a, float *b, float *res) {
	res[0] = a[1]*b[2] - a[2]*b[1];
	res[1] = a[2]*b[0] - a[0]*b[2];
	res[2] = a[0]*b[1] - a[1]*b[0];
}

void normalize(float *a) {
	float l = sqrt(a[0]*a[0] + a[1]*a[1] + a[2]*a[2]);
	a[0] = a[0]/l;
	a[1] = a[1]/l;
	a[2] = a[2]/l;
}

float length(float *v) {
	return sqrt(v[0]*v[0] + v[1]*v[1] + v[2]*v[2]);
}

void multMatrixVector(float *m, float *v, float *res) {
	for (int j = 0; j < 4; ++j) {
		res[j] = 0;
		for (int k = 0; k < 4; ++k) {
			res[j] += v[k] * m[j * 4 + k];
		}
	}
}



void getCatmullRomPoint(float t, float *p0, float *p1, float *p2, float *p3,
                        float *pos, float *deriv) {

	float m[4][4] = { {-0.5f,  1.5f, -1.5f,  0.5f},
	                  { 1.0f, -2.5f,  2.0f, -0.5f},
	                  {-0.5f,  0.0f,  0.5f,  0.0f},
	                  { 0.0f,  1.0f,  0.0f,  0.0f} };

	// T and T' (derivative) row vectors
	float T[4]  = { t*t*t, t*t, t, 1.0f };
	float Td[4] = { 3*t*t, 2*t, 1.0f, 0.0f };

	// Process each component x, y, z independently
	for (int i = 0; i < 3; i++) {

		// Build the P vector for this component
		float pv[4] = { p0[i], p1[i], p2[i], p3[i] };

		// A = M * P
		float A[4];
		multMatrixVector((float*)m, pv, A);

		// pos[i]   = T  · A
		pos[i] = T[0]*A[0] + T[1]*A[1] + T[2]*A[2] + T[3]*A[3];

		// deriv[i] = T' · A
		deriv[i] = Td[0]*A[0] + Td[1]*A[1] + Td[2]*A[2] + Td[3]*A[3];
	}
}


void getGlobalCatmullRomPoint(float gt, float *pos, float *deriv) {

	float t = gt * POINT_COUNT;
	int index = (int)floor(t);
	t = t - index;

	int indices[4];
	indices[0] = (index + POINT_COUNT - 1) % POINT_COUNT;
	indices[1] = (indices[0] + 1) % POINT_COUNT;
	indices[2] = (indices[1] + 1) % POINT_COUNT;
	indices[3] = (indices[2] + 1) % POINT_COUNT;

	getCatmullRomPoint(t, p[indices[0]], p[indices[1]],
	                      p[indices[2]], p[indices[3]], pos, deriv);
}


void renderCatmullRomCurve() {

	float pos[3], deriv[3];
	float step = 0.01f;   // 100 segments

	glBegin(GL_LINE_LOOP);
	for (float gt = 0.0f; gt < 1.0f; gt += step) {
		getGlobalCatmullRomPoint(gt, pos, deriv);
		glVertex3f(pos[0], pos[1], pos[2]);
	}
	glEnd();
}


void changeSize(int w, int h) {
	if (h == 0) h = 1;
	float ratio = w * 1.0f / h;
	glMatrixMode(GL_PROJECTION);
	glLoadIdentity();
	glViewport(0, 0, w, h);
	gluPerspective(45, ratio, 1, 1000);
	glMatrixMode(GL_MODELVIEW);
}


void renderScene(void) {

	static float t = 0;

	glClearColor(0.0f, 0.0f, 0.0f, 0.0f);
	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

	glLoadIdentity();
	gluLookAt(camX, camY, camZ,
	          0.0,  0.0,  0.0,
	          0.0f, 1.0f, 0.0f);

	renderCatmullRomCurve();

	// move & orient the teapot
	float pos[3], deriv[3];
	getGlobalCatmullRomPoint(t, pos, deriv);

	// X axis = normalised tangent
	float X[3] = { deriv[0], deriv[1], deriv[2] };
	normalize(X);
	glBegin(GL_LINES);
		glColor3f(1,0,0);
		glVertex3f(pos[0], pos[1], pos[2]);
		glVertex3f(pos[0]+X[0], pos[1]+X[1], pos[2]+X[2]);
	glEnd();

	// Initial "up" guess
	float Y0[3] = { 0.0f, 1.0f, 0.0f };

	// Z = X × Y0
	float Z[3];
	cross(prevY, X, Z);
	normalize(Z);
	glBegin(GL_LINES);
		glColor3f(0,1,0);
		glVertex3f(pos[0], pos[1], pos[2]);
		glVertex3f(pos[0]+Z[0], pos[1]+Z[1], pos[2]+Z[2]);
	glEnd();

	// Y = Z × X  (recomputed so the frame is orthonormal)
	float Y[3];
	cross(X, Z, Y);
	normalize(Y);
	glBegin(GL_LINES);
		glColor3f(0,0,1);
		glVertex3f(pos[0], pos[1], pos[2]);
		glVertex3f(pos[0]+Y[0], pos[1]+Y[1], pos[2]+Y[2]);
	glEnd();

	glColor3f(1,1,1);
	prevY[0] = Y[0]; prevY[1] = Y[1]; prevY[2] = Y[2];

	// Build the 4×4 rotation matrix (row-major here; OpenGL wants column-major,
	// so we pass the transpose — buildRotMatrix stores vectors as rows, which
	// is already the transpose of the column-major convention).
	float rotM[16];
	buildRotMatrix(X, Z, Y, rotM);

	glTranslatef(pos[0], pos[1], pos[2]);
	glMultMatrixf(rotM);

	glutWireTeapot(0.1);

	glutSwapBuffers();
	t += 0.001f;
	if (t > 1.0f) t -= 1.0f;   // keep t in [0,1)
}


void processMouseButtons(int button, int state, int xx, int yy) {
	if (state == GLUT_DOWN) {
		startX = xx; startY = yy;
		if      (button == GLUT_LEFT_BUTTON)  tracking = 1;
		else if (button == GLUT_RIGHT_BUTTON) tracking = 2;
		else                                  tracking = 0;
	} else if (state == GLUT_UP) {
		if (tracking == 1) { alpha += (xx - startX); beta += (yy - startY); }
		else if (tracking == 2) { r -= yy - startY; if (r < 3) r = 3; }
		tracking = 0;
	}
}

void processMouseMotion(int xx, int yy) {
	if (!tracking) return;
	int deltaX = xx - startX, deltaY = yy - startY;
	int alphaAux, betaAux, rAux;

	if (tracking == 1) {
		alphaAux = alpha + deltaX;
		betaAux  = beta  + deltaY;
		if (betaAux >  85) betaAux =  85;
		if (betaAux < -85) betaAux = -85;
		rAux = r;
	} else {
		alphaAux = alpha; betaAux = beta;
		rAux = r - deltaY;
		if (rAux < 3) rAux = 3;
	}
	camX = rAux * sin(alphaAux * M_PI/180.0) * cos(betaAux * M_PI/180.0);
	camZ = rAux * cos(alphaAux * M_PI/180.0) * cos(betaAux * M_PI/180.0);
	camY = rAux *                               sin(betaAux * M_PI/180.0);
}


int main(int argc, char **argv) {
	glutInit(&argc, argv);
	glutInitDisplayMode(GLUT_DEPTH | GLUT_DOUBLE | GLUT_RGBA);
	glutInitWindowPosition(100, 100);
	glutInitWindowSize(320, 320);
	glutCreateWindow("CG@DI-UM");

	glutDisplayFunc(renderScene);
	glutIdleFunc(renderScene);
	glutReshapeFunc(changeSize);
	glutMouseFunc(processMouseButtons);
	glutMotionFunc(processMouseMotion);

	glEnable(GL_DEPTH_TEST);
	glEnable(GL_CULL_FACE);

	glutMainLoop();
	return 1;
}