#!/bin/bash

CMD=$1

echo MYCS
date
echo 180_3_20
time $CMD /home/jkessler/play/play/ThreeColo/inputs/MYC_180_3_20.3color > WVS_MYC_180_3_20.3color_out;
echo 60_4_20
time $CMD /home/jkessler/play/play/ThreeColo/inputs/MYC_60_4_20.3color > WVS_MYC_60_4_20.3color_out;
echo 300_3_80
time $CMD /home/jkessler/play/play/ThreeColo/inputs/MYC_300_3_80.3color > WVS_MYC_300_3_80.3color_out;
echo 100_4_80
time $CMD /home/jkessler/play/play/ThreeColo/inputs/MYC_100_4_80.3color > WVS_MYC_100_4_80.3color_out;
echo RANDS
date
echo 100_4_80
time $CMD /home/jkessler/play/play/ThreeColo/inputs/RAND_100_4_80.3color > WVS_RAND_100_4_80.3color_out;
echo 300_3_80
time $CMD /home/jkessler/play/play/ThreeColo/inputs/RAND_300_3_80.3color > WVS_RAND_300_3_80.3color_out;
echo 180_3_20
time $CMD /home/jkessler/play/play/ThreeColo/inputs/RAND_180_3_20.3color > WVS_RAND_180_3_20.3color_out;
echo 90_4_20
time $CMD /home/jkessler/play/play/ThreeColo/inputs/RAND_90_4_20.3color > WVS_RAND_90_4_20.3color_out;
echo CLIQUES
date
echo 300_4
time $CMD /home/jkessler/play/play/ThreeColo/inputs/CLIQUE_300_4.3color > WVS_CLIQUE_300_4.3color_out;
echo 300_3
time $CMD /home/jkessler/play/play/ThreeColo/inputs/CLIQUE_300_3.3color > WVS_CLIQUE_300_3.3color_out;
echo Grotzsch
time $CMD /home/jkessler/play/play/ThreeColo/inputs/grotzsch.3color > WVS_grotzsch.3color_out;
echo Chvatal
time $CMD /home/jkessler/play/play/ThreeColo/inputs/chvatal.3color > WVS_chvatal.3color_out;
