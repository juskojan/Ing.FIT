/**
*	Projekt PRL 2
*	Mesh multiplication
*	
*	Autor: Jan Jusko (xjusko00)
*	Brno University of Technology @2017
*/

#include <mpi.h>
#include <cstdio>
#include <cstdlib>
#include <iostream>
#include <fstream>
#include <sstream>
#include <string>

using namespace std;

/* struct of matrix */
typedef struct tsruct{
	int rows;
	int cols;
	int **matrix2D;
}TMatrix;


/* Get dimensions of a matrix */
void getDimensions(string matFile, int *cols, int *rows){
	char c;
	string rowNumber, line;
	int tmp = 1;
	int counterOfRows = -1;
	ifstream handle(matFile.c_str());
	
	while (getline (handle,line)){
		counterOfRows++;
	}
	*rows = counterOfRows;

	handle.clear();
	handle.seekg(0);

	// skip first number
	handle.get(c);
	while(c != '\n'){
		handle.get(c);
	}

	// get number of cols
	handle.get(c);
	while(c != '\n'){
		if(c == ' ')
			tmp++;
		handle.get(c);
	}
	*cols = tmp;

	// done, close file
	handle.close();
}

/* print & free matrix */
bool freeMat(TMatrix *matrix, bool printing){
	if(printing){
		printf("%d:%d\n", matrix->rows, matrix->cols);
		for (int r = 0; r < matrix->rows; r++){
			for (int c = 0; c < matrix->cols; c++){
				printf("%d ", matrix->matrix2D[r][c]);
			}
			printf("\n");
		}
	}

	for (int i = 0; i < matrix->rows; i++)
		free((int *) matrix->matrix2D[i]);
	
	free(matrix->matrix2D);

	return true;
}

/* is my procesor in the first column? */
bool isFirstCol(int id, int cols){
	return (id % cols == 0) ? true : false;
}

/* is my procesor in the first row? */
bool isFirstRow(int id, int cols){
	return (id < cols) ? true : false;
}

/* is my procesor in the first row or first column? */
bool isOnTheEdge(int id, int cols){
	return (isFirstCol(id, cols) || isFirstRow(id, cols));
}

/* init memory for matrix struct, if stream given, fill elements too */
bool initMat(TMatrix *matrix, ifstream *stream){

	string line;
	int j = 0, tmp;

	// ***** create 2D array *****
	if ((matrix->matrix2D = new int *[matrix->rows]) == NULL)
		return false;

	for (int i = 0; i < matrix->rows; i++)
		if ((matrix->matrix2D[i] = new int[matrix->cols]) == NULL )
			return false;
	
	// ***** fill matrix2Dents *****

	if(stream == NULL)
		return true;
	// get rid of initial line
	getline(*stream, line);

	for (int i = 0; i < matrix->rows; i++, j = 0){
		// get whole line and convert to stream
		getline(*stream, line);
		stringstream lineStream(line);

		while(1){
			// get number from row
			lineStream >> tmp;
			if(!lineStream)
				break;
			// assign appropriately
			matrix->matrix2D[i][j++] = tmp;
		}
	}

	return true;
}


/* MAIN */
int main(int argc,char* argv[])
{
	int mat1cols, mat1rows, mat2cols, mat2rows, proc_count, myid, numprocs;
	int current_row = 0, current_col = 0, a = 0, b = 0, c = 0;
	TMatrix mat1, mat2, resultMat;

	// MPI init
	MPI_Status status;
	MPI_Init(&argc,&argv);
	MPI_Comm_size(MPI_COMM_WORLD, &numprocs);
	MPI_Comm_rank(MPI_COMM_WORLD, &myid);

	// get dimensions, receive through variables
	getDimensions("mat1", &mat1cols, &mat1rows);
	getDimensions("mat2", &mat2cols, &mat2rows);

	mat1.rows = resultMat.rows = mat1rows;
	mat1.cols = mat1cols;
	mat2.rows = mat2rows;
	mat2.cols = resultMat.cols = mat2cols;
	proc_count = mat1rows * mat2cols;

	// first column procs will hold matrix 1
	if (isFirstCol(myid, resultMat.cols)){
		// open file mat1 - constant filename
		ifstream mat1File;
		mat1File.open("mat1");

		// init & fill matrix
		if (!initMat(&mat1, &mat1File)){
			fprintf(stderr,"Error: Could not initialize matrix!\n");
			return EXIT_FAILURE;
		}
		// close
		mat1File.close();
	}

	// first row procs will hold matrix 2
	if (myid < resultMat.cols){
		// open file mat2 - constant filename
		ifstream mat2File;
		mat2File.open("mat2");
		
		// init & fill matrix
		if (!initMat(&mat2, &mat2File)){
			fprintf(stderr,"Error: Could not initialize matrix!\n");
			return EXIT_FAILURE;
		}
		// close
		mat2File.close();
	}
	

	// a b numbers propagation across matrix
	for(int i = 0; i < mat1.cols; i++){

		if(isOnTheEdge(myid, resultMat.cols)){
			if(isFirstCol(myid, resultMat.cols)){
				if(current_col < mat1.cols)
					a = mat1.matrix2D[ (myid/resultMat.cols) ][current_col++];
			}
			if(isFirstRow(myid, resultMat.cols)){
				if(current_row < mat2.rows)
					b = mat2.matrix2D[current_row++][myid];
			}
		}
		else{
			MPI_Recv(&a, 1, MPI_INT, myid - 1, 42, MPI_COMM_WORLD, &status);
			MPI_Recv(&b, 1, MPI_INT, myid - resultMat.cols, 42, MPI_COMM_WORLD, &status);
			c = c + a * b;
		}

		if(myid == 0){
			// upper left corner
			c = c + a * b;

			// just init result matrix
			if (!initMat(&resultMat, NULL)){
				fprintf(stderr,"Error: Could not allocate enough space for matrix!\n");
				return EXIT_FAILURE;
			}
			
			// fill first value
			resultMat.matrix2D[0][0] = c;
		}
		else{
			if(isFirstCol(myid, resultMat.cols)){
				// recieve from upper neighbour
				MPI_Recv(&b, 1, MPI_INT, myid - resultMat.cols, 42, MPI_COMM_WORLD, &status);
				c = c + a * b;
			}
			else if(isFirstRow(myid, resultMat.cols)){
				// receive from left neighbour
				MPI_Recv(&a, 1, MPI_INT, myid - 1, 42, MPI_COMM_WORLD, &status);
				c = c + a * b;
			}
		}

		// send to right if not last col
		if(!isFirstCol(myid + 1, resultMat.cols))
			MPI_Send(&a, 1, MPI_INT, myid + 1, 42, MPI_COMM_WORLD);

		// send down if not last row
		if(myid < proc_count - resultMat.cols)
			MPI_Send(&b, 1, MPI_INT, myid + resultMat.cols, 42, MPI_COMM_WORLD);
	}


	// all send computed tmpue to master cpu 0
	if (myid != 0){
		MPI_Send(&c, 1, MPI_INT, 0, 42, MPI_COMM_WORLD);
	}

	// fill result matrix
	if (myid == 0){
		// receive from all cpus
		for (int i = 1; i < proc_count; i++){
			MPI_Recv(&c, 1, MPI_INT, i, 42, MPI_COMM_WORLD, &status);
			resultMat.matrix2D[i / resultMat.cols][i % resultMat.cols] = c;
		}

		// print & free
		freeMat(&resultMat, true);
	}

	// free partial matrixes, dont print
	if(isFirstCol(myid, resultMat.cols)){
		freeMat(&mat1, false);
	}
	if(isFirstRow(myid, resultMat.cols)){
		freeMat(&mat2, false);
	}

	// finalize FIN
	MPI_Finalize();

	return EXIT_SUCCESS;
}
