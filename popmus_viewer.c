/*
File: popmus_viewer.c
Author: Teodor Ande Elstad

General description: Viewing application for the POPMUS system. 
                     Reads a input file form the popmus_analysis 
		     sytem and visualizes it with help from OpenGL/GLUT.

System reqirements: OpenGL and GLUT has to be installed on the system.

Notes on content: The main function reads input from a file called results.dat 
                  and formmattes this info into global variables.
		  All methods with draw prefix is conserned with drawing on screen.
		  The rest of the functions are used to fit the coordinates inside the viewing window.
*/

#include <assert.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "GL/glut.h"

GLuint Window = 0;
int dataSize;
int nvmSelection = 2;
float (*dataArr)[10];
float maxLocalForce[3];

float dxyCoords(int y)
{
  int i;
  float min = 0.0;
  float max = 0.0;

  for(i = 0; i<dataSize; i++) {
    if (dataArr[i][0] > max) {
      max = dataArr[i][0+y];
    } else if (dataArr[i][2+y] > max) {
      max = dataArr[i][2+y];
    } else if (dataArr[i][0+y] < min) {
      min = dataArr[i][0+y];
    } else if (dataArr[i][2+y] < min) {
      min = dataArr[i][2+y];
    }
  }
  return (max+min)/2.0;
}

float maxCoords(void)
{
  int i;
  float max = 0.0;

  for(i = 0; i<dataSize; i++) {
    if (fabs(dataArr[i][0]) > max) {
      max = fabs(dataArr[i][0]);
    } else if (fabs(dataArr[i][1]) > max) {
      max = fabs(dataArr[i][1]);
    } else if (fabs(dataArr[i][2]) > max) {
      max = fabs(dataArr[i][2]);
    } else if (fabs(dataArr[i][3]) > max) {
      max = fabs(dataArr[i][3]);
    }
  }
  return max;
}

float scale(float num, float scale)
{
  if(num == 0.0){
    return num;
  }
  return num/(scale*1.2);
}

void scaleCoords(void)
{
  int i;
  float scaleNum = maxCoords();

  for(i = 0; i<dataSize; i++) {
    dataArr[i][0] = scale(dataArr[i][0], scaleNum);
    dataArr[i][1] = scale(dataArr[i][1], scaleNum);
    dataArr[i][2] = scale(dataArr[i][2], scaleNum);
    dataArr[i][3] = scale(dataArr[i][3], scaleNum);
  }
}

void transformCoords(void)
{
  int i;
  float dx = dxyCoords(0);
  float dy = dxyCoords(1);

  for(i = 0; i<dataSize; i++) {
    dataArr[i][0] = dataArr[i][0] - dx;
    dataArr[i][1] = dataArr[i][1] - dy;
    dataArr[i][2] = dataArr[i][2] - dx;
    dataArr[i][3] = dataArr[i][3] - dy;
  }
  scaleCoords();
}

void collectMaxForce(void)
{
  int i, j;
  
  for(j = 0; j<3; j++){
    for(i = 0; i<dataSize; i++) {
      if (dataArr[i][j+4] > maxLocalForce[j]) {
	maxLocalForce[j] = dataArr[i][j+4];
      } if (dataArr[i][j+7] > maxLocalForce[j]) {
	maxLocalForce[j] = dataArr[i][j+7];
      }
    }
  }
}

void drawInfo(void)
{
  glColor3f(1.0, 0.0, 0.0); 
  glBegin(GL_POLYGON); 
  glVertex3f(-0.95, 0.95, 0.0); 
  glVertex3f(-0.95, 0.9, 0.0); 
  glVertex3f(-0.9, 0.9, 0.0); 
  glVertex3f(-0.9, 0.95, 0.0); 
  glEnd();

  char str[15];
  if (nvmSelection == 2) {
    sprintf(str, "%i %s",(int)(maxLocalForce[nvmSelection]), "Nmm");
  } else {
    sprintf(str, "%i %s",(int)(maxLocalForce[nvmSelection]), "N");
  }
  
  glColor3f(0.0, 0.0, 0.0); 
  glRasterPos3f(-0.87, 0.905, 0.0);
  glutBitmapString(GLUT_BITMAP_HELVETICA_18, str);
  
  glRasterPos3f(-0.1, 0.905, 0.0);
  glutBitmapString(GLUT_BITMAP_HELVETICA_12, "View V, N and M diagrams by pressing v, n or m on the keyboard.");
}

void drawStructure(void)
{
  int i;

  glLineWidth(7);
  glBegin(GL_LINES);
  
  for (i = 0; i<dataSize; i++) {
    glColor3f(0.0 + scale(fabs(dataArr[i][nvmSelection+4]), maxLocalForce[nvmSelection]), 
	      0.0, 
	      1.0 - scale(fabs(dataArr[i][nvmSelection+4]), maxLocalForce[nvmSelection]));
    glVertex2f(dataArr[i][0], dataArr[i][1]);
    
    glColor3f(0.0 + scale(fabs(dataArr[i][nvmSelection+7]), maxLocalForce[nvmSelection]), 
	      0.0, 
	      1.0 - scale(fabs(dataArr[i][nvmSelection+7]), maxLocalForce[nvmSelection]));
    glVertex2f(dataArr[i][2], dataArr[i][3]);
  }
  glEnd();
}

void drawScene(void)
{
  glClear(GL_COLOR_BUFFER_BIT);
  glClearColor(1.0, 1.0, 1.0, 1.0);
  glPushMatrix();

  drawInfo();
  drawStructure();
  
  glPopMatrix();  
  glutSwapBuffers();
}

void key(unsigned char k, int x, int y)
{
  (void) x;
  (void) y;
  switch (k) {
  case 27:
    glutDestroyWindow(Window);
    exit(0);
  case 'n':
    nvmSelection = 0;
    break;
  case 'v':
    nvmSelection = 1;
    break;
  case 'm':
    nvmSelection = 2;
    break;
  }
  glutPostRedisplay();
}

int main(int argc, char *argv[])
{
  FILE *inputFile;
  int i;
  char buffer[500];
  
  inputFile = fopen("results.dat", "r");
  
  fgets(buffer, 500, inputFile);
  fscanf(inputFile, "%i", &dataSize);
  fgets(buffer, 500, inputFile);
  fgets(buffer, 500, inputFile);
  fgets(buffer, 500, inputFile);
  
  float inputArr[dataSize][10];
  for (i = 0; i<dataSize; i++) {
    fscanf(inputFile, "%f %f %f %f %f %f %f %f %f %f", 
	   &inputArr[i][0], &inputArr[i][1], &inputArr[i][2], &inputArr[i][3], &inputArr[i][4], 
	   &inputArr[i][5], &inputArr[i][6], &inputArr[i][7], &inputArr[i][8], &inputArr[i][9]); 
  }

  fclose(inputFile);
  dataArr = inputArr;

  transformCoords();
  collectMaxForce();

  glutInit(&argc, argv);
  glutInitWindowPosition(0, 0);
  glutInitWindowSize(700, 700);
  glutInitDisplayMode( GLUT_RGB | GLUT_DEPTH | GLUT_DOUBLE );
  
  Window = glutCreateWindow("POPMUS Viewer");
  if (!Window) {
    exit(1);
  }

  glutKeyboardFunc(key);
  glutDisplayFunc(drawScene);
  glutMainLoop();
  return 0;
}

