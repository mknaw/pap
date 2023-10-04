; ========================================================================
;
; (C) Copyright 2023 by Molly Rocket, Inc., All Rights Reserved.
;
; This software is provided 'as-is', without any express or implied
; warranty. In no event will the authors be held liable for any damages
; arising from the use of this software.
;
; Please see https://computerenhance.com for further information
;
; ========================================================================

; ========================================================================
; LISTING 47
; ========================================================================

bits 16

add bx, 30000
add bx, 10000
sub bx, 5000 ; bx:0x9c40->0x88b8 flags:SO->PAS
sub bx, 5000 ; bx:0x88b8->0x7530 flags:PAS->PO
mov bx, 1 ; bx:0x7530->0x1 
mov cx, 100 ; cx:0x0->0x64 
add bx, cx ; bx:0x1->0x65 flags:PO->P
mov dx, 10 ; dx:0x0->0xa 
sub cx, dx ; cx:0x64->0x5a flags:P->PA
add bx, 40000 ; bx:0x65->0x9ca5 flags:PA->PS
add cx, -90 ; cx:0x5a->0x0 flags:PS->CPAZ
mov sp, 99 ; sp:0x0->0x63 
mov bp, 98 ; bp:0x0->0x62 
cmp bp, sp ; flags:CPAZ->CPAS

; 1001 1100 0100 0000
; 0001 0011 1000 1000
