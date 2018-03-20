#include <mpi.h>
#include <fstream>
#include <stdio.h>

using namespace std;

#define REG_X_FLAG 0
#define REG_Y_FLAG 1
#define REG_Z_FLAG 2
#define ROTATION_FLAG 3

#define C_REGISTER_INIT     1
#define EMPTY_REGISTER      360

int main (int argc, char *argv[]) {
    const char * FILE_NAME = "numbers";         // File name
    int procCount;                              // Number of processors
    int procId;                                 // Current processor ID
    MPI_Status status;
    
    MPI_Init(&argc,&argv);                      // MPI initialisation
    MPI_Comm_size(MPI_COMM_WORLD, &procCount);  // Get number of proc's
    MPI_Comm_rank(MPI_COMM_WORLD, &procId);     // Get current proc ID

    int numberCount = procCount - 1;            // Get length of sorted array
    
    float C;                             // Registers
    float X;
    float Y;
    float Z;

    if (procId == 0) {
        // Master Processor no. 0
        float numbers[numberCount];     // Array for input numbers
        
        // Read numbers from file 
        fstream fin;
        fin.open(FILE_NAME, ios::in);

        map<float, int> occurences;


        // Print the unsorted sequence on stdout and store into an array 
        for (int i = 0; i < numberCount; i++) {
            float tmp = fin.get();

            if(occurences[tmp] == 0){
                occurences[tmp] = 1;
                numbers[i] = tmp;
            }
            else{
                tmp = tmp + (i/100.0);
                numbers[i] = tmp;
            }

            printf("%.0f ", numbers[i]);
        }
        // Line Feed
        printf("\n");
        
        // Algorithm step 2, linear propagation of values
        for (int i = 0; i < numberCount; i++) {
            // Send x_i to corresopondent processor X_i (through a bus) 
            MPI_Send(&numbers[i], 1, MPI_INT, i + 1, REG_X_FLAG, MPI_COMM_WORLD);
            // Send x_i to processor Y_1 (as a neighbour)
            MPI_Send(&numbers[i], 1, MPI_INT, 1, REG_Y_FLAG, MPI_COMM_WORLD);
        }

        // Algorithm step 3, sequentionally receive values from processor N (rotation principle)
        for (int i = 1; i <= numberCount; i++) {
            MPI_Recv(&numbers[numberCount - i], 1, MPI_INT, numberCount, ROTATION_FLAG, MPI_COMM_WORLD, &status);
        }

        // Print sorted sequence
        for (int i = 0; i < numberCount; i += 1) {
            printf("%.0f\n", numbers[i]);
        }
    }
    else {
        // Slave processors numbers 1 - numberCount
        // Algorithm step 1, init values in registers
        C = C_REGISTER_INIT;
        X = EMPTY_REGISTER;
        Y = EMPTY_REGISTER;
        Z = EMPTY_REGISTER;
        int neighbour = 0;

        // Algorithm step 2, receive x_i value as a processor X_i (through a bus)
        MPI_Recv(&X, 1, MPI_INT, 0, REG_X_FLAG, MPI_COMM_WORLD, &status);

        // Algorithm step 2 (shifting right), each proc receive x_i value from left neighbour proc
        for (int i = 0; i < numberCount; i++) {
            MPI_Recv(&Y, 1, MPI_INT, procId - 1, REG_Y_FLAG, MPI_COMM_WORLD, &status);
            
            // Algorithm step 2, comparison
            if( X != EMPTY_REGISTER && Y != EMPTY_REGISTER && X > Y ){
                C++;
            }

            // Algorithm step 2, linear propagation of values to the right neighbour
            if (procId < numberCount) {
                MPI_Send(&Y, 1, MPI_INT, procId + 1, REG_Y_FLAG, MPI_COMM_WORLD);
            }
        }

        // Algorithm step 2, send your X value to processor number C
        MPI_Send(&X, 1, MPI_INT, C, REG_Z_FLAG, MPI_COMM_WORLD);
        // Algorithm step 2, receive X value from unknown processor (ANY_SOURCE) and store into register Z
        MPI_Recv(&Z, 1, MPI_INT, MPI_ANY_SOURCE, REG_Z_FLAG, MPI_COMM_WORLD, &status);

        // Algorithm step 3, every slave processor will send Z value to the right (rotation right)
        // Each proc will send n values but only receive n-1 values
        for (int i = 1; i < procId; i++) {
            // Calculate right neighbour (roatation)
            if (procId < numberCount)
                neighbour = procId + 1;
            else
                neighbour = 0;

            MPI_Send(&Z, 1, MPI_INT, neighbour, ROTATION_FLAG, MPI_COMM_WORLD);
            MPI_Recv(&Z, 1, MPI_INT, procId - 1, ROTATION_FLAG, MPI_COMM_WORLD, &status);
        }

        // Calculate right neighbour (roatation)
        if (procId < numberCount)
            neighbour = procId + 1;
        else
            neighbour = 0;

        // One additional send (every proc will send its own value + (n-1) previous values from left)
        MPI_Send(&Z, 1, MPI_INT, neighbour, ROTATION_FLAG, MPI_COMM_WORLD);
    }

    MPI_Finalize();
    return 0;
}