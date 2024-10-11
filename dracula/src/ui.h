#ifndef UI_H
#define UI_H

#include "display.h"
#include "font.h"

// The size of a half image buffer
#define HALF_BUFFER 512

int ui_init();

void ui_splash();

void mes_dracula();
void mes_no_dracula();
void mes_player_bitten(int player);
void mes_valid_action();

void err_not_adjacent();
void err_too_many_actions();
void err_no_water();
void err_no_light();
void err_no_garlic();

void err_already_moved();
void err_not_your_turn(int player);
void err_invalid_resource();

void display_health(int player_lives, int dracula_lives);


#endif // UI_H
