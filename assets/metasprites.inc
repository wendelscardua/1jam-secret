metasprite_0_data:

	.byte   0,  0,$00,0
	.byte   8,  0,$00,0|OAM_FLIP_H
	.byte   0,  8,$02,0
	.byte   8,  8,$02,0|OAM_FLIP_H
	.byte   5,- 8,$05,0
	.byte 128

metasprite_1_data:

	.byte   0,  0,$00,1
	.byte   8,  0,$00,1|OAM_FLIP_H
	.byte   0,  8,$02,1
	.byte   8,  8,$02,1|OAM_FLIP_H
	.byte   4,- 8,$06,1
	.byte 128

metasprite_2_data:

	.byte   0,  0,$00,2
	.byte   8,  0,$00,2|OAM_FLIP_H
	.byte   0,  8,$02,2
	.byte   8,  8,$02,2|OAM_FLIP_H
	.byte   4,- 8,$07,2
	.byte 128

metasprite_3_data:

	.byte   0,  0,$00,3
	.byte   8,  0,$00,3|OAM_FLIP_H
	.byte   0,  8,$02,3
	.byte   8,  8,$02,3|OAM_FLIP_H
	.byte   4,- 8,$08,3
	.byte 128

metasprite_4_data:

	.byte   0,  0,$03,0
	.byte   8,  0,$03,0|OAM_FLIP_H
	.byte   0,  8,$02,0
	.byte   8,  8,$02,0|OAM_FLIP_H
	.byte   4,- 8,$09,0
	.byte 128

metasprite_5_data:

	.byte   0,  0,$03,1
	.byte   8,  0,$03,1|OAM_FLIP_H
	.byte   0,  8,$02,1
	.byte   8,  8,$02,1|OAM_FLIP_H
	.byte   4,- 8,$0a,1
	.byte 128

metasprite_6_data:

	.byte   0,  0,$03,2
	.byte   8,  0,$03,2|OAM_FLIP_H
	.byte   0,  8,$02,2
	.byte   8,  8,$02,2|OAM_FLIP_H
	.byte   4,- 8,$0b,2
	.byte 128

metasprite_7_data:

	.byte   0,  0,$03,3
	.byte   8,  0,$03,3|OAM_FLIP_H
	.byte   0,  8,$02,3
	.byte   8,  8,$02,3|OAM_FLIP_H
	.byte   4,- 8,$0c,3
	.byte 128

metasprite_8_data:

	.byte   0,  0,$04,1
	.byte   8,  0,$04,1|OAM_FLIP_H
	.byte   0,  8,$02,1
	.byte   8,  8,$02,1|OAM_FLIP_H
	.byte   4,- 8,$0d,1
	.byte 128

metasprite_9_data:

	.byte   0,  0,$12,1
	.byte   0,  8,$12,1|OAM_FLIP_V
	.byte   8,  0,$13,1
	.byte   8,  8,$13,1|OAM_FLIP_V
	.byte  16, 12,$0d,1|OAM_FLIP_V
	.byte 128

metasprite_10_data:

	.byte   0,  0,$0e,0
	.byte   8,  0,$0e,0|OAM_FLIP_H
	.byte   0,  8,$02,0
	.byte   8,  8,$02,0|OAM_FLIP_H
	.byte   4,- 8,$0f,0
	.byte 128

metasprite_pointers:

	.word metasprite_0_data
	.word metasprite_1_data
	.word metasprite_2_data
	.word metasprite_3_data
	.word metasprite_4_data
	.word metasprite_5_data
	.word metasprite_6_data
	.word metasprite_7_data
	.word metasprite_8_data
	.word metasprite_9_data
	.word metasprite_10_data

