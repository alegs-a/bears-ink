#include "font.h"

unsigned char buffer_A[5] = {
    0b11111100,
    0b00010011,
    0b00010001,
    0b00010011,
    0b11111100
};

unsigned char buffer_B[5] = {
    0b11111111,
    0b10001001,
    0b10001001,
    0b10001001,
    0b01110110
};

unsigned char buffer_C[5] = {
    0b11111111,
    0b10000001,
    0b10000001,
    0b10000001,
    0b01100010
};

unsigned char buffer_D[5] = {
    0b11111111,
    0b10000001,
    0b10000001,
    0b10000001,
    0b01111110
};

unsigned char buffer_E[5] = {
    0b11111111,
    0b10001001,
    0b10001001,
    0b10001001,
    0b10001001
};

unsigned char buffer_F[5] = {
    0b11111111,
    0b00001001,
    0b00001001,
    0b00001001,
    0b00001001
};

unsigned char buffer_G[5] = {
    0b11111111,
    0b10000001,
    0b10010001,
    0b10010001,
    0b11110001
};

unsigned char buffer_H[5] = {
    0b11111111,
    0b00001000,
    0b00001000,
    0b00001000,
    0b11111111,
};

unsigned char buffer_I[5] = {
    0b10000001,
    0b10000001,
    0b11111111,
    0b10000001,
    0b10000001
};

unsigned char buffer_J[5] = {
    0b01000001,
    0b10000001,
    0b01111111,
    0b00000001,
    0b00000001
};

unsigned char buffer_K[5] = {
    0b11111111,
    0b00011000,
    0b00100100,
    0b01000010,
    0b10000001
};

unsigned char buffer_L[5] = {
    0b11111111,
    0b10000000,
    0b10000000,
    0b10000000,
    0b11000000
};

unsigned char buffer_M[5] = {
    0b11111111,
    0b00000001,
    0b11111111,
    0b00000001,
    0b11111111
};

unsigned char buffer_N[5] = {
    0b11111111,
    0b00000011,
    0b00011100,
    0b01100000,
    0b11111111
};

unsigned char buffer_O[5] = {
    0b11111111,
    0b10000001,
    0b10000001,
    0b10000001,
    0b11111111
};

unsigned char buffer_P[5] = {
    0b11111111,
    0b00010001,
    0b00010001,
    0b00010001,
    0b00001110
};

unsigned char buffer_Q[5] = {
    0b00111110,
    0b01000001,
    0b01000001,
    0b01000001,
    0b10111110
};

unsigned char buffer_R[5] = {
    0b11111111,
    0b00001001,
    0b00011001,
    0b00101001,
    0b11000110
};

unsigned char buffer_S[5] = {
    0b01000110,
    0b10001001,
    0b10001001,
    0b10001001,
    0b01110010
};

unsigned char buffer_T[5] = {
    0b00000001,
    0b00000001,
    0b11111111,
    0b00000001,
    0b00000001
};

unsigned char buffer_U[5] = {
    0b01111111,
    0b10000000,
    0b10000000,
    0b10000000,
    0b01111111,
};

unsigned char buffer_V[5] = {
    0b00000111,
    0b01111000,
    0b10000000,
    0b01111000,
    0b00000111
};

unsigned char buffer_W[5] = {
    0b11111111,
    0b10000000,
    0b11111111,
    0b10000000,
    0b11111111
};

unsigned char buffer_X[5] = {
    0b11000011,
    0b00110100,
    0b00001000,
    0b00110100,
    0b11000011
};

unsigned char buffer_Y[5] = {
    0b00000011,
    0b00001100,
    0b11110000,
    0b00001100,
    0b00000011
};

unsigned char buffer_Z[5] = {
    0b10000011,
    0b10000101,
    0b10001001,
    0b10010001,
    0b11100001
};

unsigned char buffer_0[5] = {
    0b11111111,
    0b10000001,
    0b10000001,
    0b10000001,
    0b11111111
};

unsigned char buffer_1[5] = {
    0b10000100,
    0b10000010,
    0b11111111,
    0b10000000,
    0b10000000
};

unsigned char buffer_2[5] = {
    0b11100010,
    0b10010001,
    0b10010001,
    0b10010001,
    0b10001110
};

unsigned char buffer_3[5] = {
    0b01000010,
    0b10001001,
    0b10001001,
    0b10001001,
    0b01110110
};

unsigned char buffer_4[5] = {
    0b00001111,
    0b00001000,
    0b00001000,
    0b00001000,
    0b11111111
};

unsigned char buffer_5[5] = {
    0b10001111,
    0b10001001,
    0b10001001,
    0b10001001,
    0b01110001
};

unsigned char buffer_6[5] = {
    0b01111110,
    0b10001001,
    0b10001001,
    0b10001001,
    0b01110000
};

unsigned char buffer_7[5] = {
    0b00000001,
    0b11000001,
    0b00110001,
    0b00001101,
    0b00000011
};

unsigned char buffer_8[5] = {
    0b01110110,
    0b10001001,
    0b10001001,
    0b10001001,
    0b01110110
};

unsigned char buffer_9[5] = {
    0b00000110,
    0b10001001,
    0b10001001,
    0b10001001,
    0b01110110
};

unsigned char buffer_question_mark[5] = {
    0b00000110,
    0b00000001,
    0b10110001,
    0b00001001,
    0b00000110
};

unsigned char *display_get_buffer(char c)
{
    // Convert lowercase letters to uppercase.
    if (c > 96 && c < 123)
        c -= 32;

    switch (c)
    {
        case 'A': return buffer_A; break;
        case 'B': return buffer_B; break;
        case 'C': return buffer_C; break;
        case 'D': return buffer_D; break;
        case 'E': return buffer_E; break;
        case 'F': return buffer_F; break;
        case 'G': return buffer_G; break;
        case 'H': return buffer_H; break;
        case 'I': return buffer_I; break;
        case 'J': return buffer_J; break;
        case 'K': return buffer_K; break;
        case 'L': return buffer_L; break;
        case 'M': return buffer_M; break;
        case 'N': return buffer_N; break;
        case 'O': return buffer_O; break;
        case 'P': return buffer_P; break;
        case 'Q': return buffer_Q; break;
        case 'R': return buffer_R; break;
        case 'S': return buffer_S; break;
        case 'T': return buffer_T; break;
        case 'U': return buffer_U; break;
        case 'V': return buffer_V; break;
        case 'W': return buffer_W; break;
        case 'X': return buffer_X; break;
        case 'Y': return buffer_Y; break;
        case 'Z': return buffer_Z; break;
        case '0': return buffer_0; break;
        case '1': return buffer_1; break;
        case '2': return buffer_2; break;
        case '3': return buffer_3; break;
        case '4': return buffer_4; break;
        case '5': return buffer_5; break;
        case '6': return buffer_6; break;
        case '7': return buffer_7; break;
        case '8': return buffer_8; break;
        case '9': return buffer_9; break;
    }

    return buffer_question_mark;
}

int display_string(const char *string, unsigned int x, unsigned int y)
{
    const char *c = string;
    while (*c) {

        // Wrap strings to the next line.
        if (x + 5 > 127) {
            x = 0;
            y += 1;
        }

        // Wrap bottom row to the top row.
        if (y > 7)
            y = 0;

        unsigned char *buffer = display_get_buffer(*c);
        int error = display_write(x, x + 5, y, y, buffer, 5);
        if (error)
            return error;

        // Add padding between characters.
        x += 6;
        c++;
    }

    return 0;
}

