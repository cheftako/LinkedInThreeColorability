# Run the time test
bash -c "time ./threecolor tests/SampleThreeColorable2.3color" 2> /tmp/time.holder
echo SampleThreeColorable2.3color `egrep -v "user|sys|^$" /tmp/time.holder` > timer.log
bash -c "time ./threecolor tests/SampleNonThreeColorable3.3color" 2> /tmp/time.holder
echo SampleNonThreeColorable3.3color `egrep -v "user|sys|^$" /tmp/time.holder` >> timer.log
bash -c "time ./threecolor tests/sample5.txt" 2> /tmp/time.holder
echo sample5.txt `egrep -v "user|sys|^$" /tmp/time.holder` >> timer.log
bash -c "time ./threecolor tests/sampleTriPod.txt" 2> /tmp/time.holder
echo sampleTriPod.txt `egrep -v "user|sys|^$" /tmp/time.holder` >> timer.log
bash -c "time ./threecolor tests/Sample3Colorable.3color" 2> /tmp/time.holder
echo Sample3Colorable.3color `egrep -v "user|sys|^$" /tmp/time.holder` >> timer.log
bash -c "time ./threecolor tests/SampleThreeColorable2.3color" 2> /tmp/time.holder
echo SampleThreeColorable2.3color `egrep -v "user|sys|^$" /tmp/time.holder` >> timer.log
bash -c "time ./threecolor tests/sampleBand.txt" 2> /tmp/time.holder
echo sampleBand.txt `egrep -v "user|sys|^$" /tmp/time.holder` >> timer.log
bash -c "time ./threecolor tests/SampleNon3Colorable.3color" 2> /tmp/time.holder
echo SampleNon3Colorable.3color `egrep -v "user|sys|^$" /tmp/time.holder` >> timer.log
bash -c "time ./threecolor tests/SampleThreeColorable3.3color" 2> /tmp/time.holder
echo SampleThreeColorable3.3color `egrep -v "user|sys|^$" /tmp/time.holder` >> timer.log
bash -c "time ./threecolor tests/sampleClique.txt" 2> /tmp/time.holder
echo tests/sampleClique.txt `egrep -v "user|sys|^$" /tmp/time.holder` >> timer.log
bash -c "time ./threecolor tests/SampleNonThreeColorable2.3color" 2> /tmp/time.holder
echo tests/SampleNonThreeColorable2.3color `egrep -v "user|sys|^$" /tmp/time.holder` >> timer.log
bash -c "time ./threecolor tests/sample.txt" 2> /tmp/time.holder
echo tests/sample.txt `egrep -v "user|sys|^$" /tmp/time.holder` >> timer.log
bash -c "time ./threecolor tests/samplePentagram.txt " 2> /tmp/time.holder
echo tests/samplePentagram.txt `egrep -v "user|sys|^$" /tmp/time.holder` >> timer.log
bash -c "time ./threecolor data/li/balanced_tree_2_6.txt" 2> /tmp/time.holder
echo balanced_tree_2_6.txt `egrep -v "user|sys|^$" /tmp/time.holder` >> timer.log
bash -c "time ./threecolor data/li/complete3.txt" 2> /tmp/time.holder
echo complete3.txt `egrep -v "user|sys|^$" /tmp/time.holder` >> timer.log
bash -c "time ./threecolor data/li/gnp_random_15_0.5.txt" 2> /tmp/time.holder
echo gnp_random_15_0.5.txt `egrep -v "user|sys|^$" /tmp/time.holder` >> timer.log
bash -c "time ./threecolor data/li/balanced_tree_4_3.txt" 2> /tmp/time.holder
echo balanced_tree_4_3.txt `egrep -v "user|sys|^$" /tmp/time.holder` >> timer.log
bash -c "time ./threecolor data/li/complete32.txt" 2> /tmp/time.holder
echo complete32.txt `egrep -v "user|sys|^$" /tmp/time.holder` >> timer.log
bash -c "time ./threecolor data/li/gnp_random_23_0.75.txt" 2> /tmp/time.holder
echo gnp_random_23_0.75.txt `egrep -v "user|sys|^$" /tmp/time.holder` >> timer.log
bash -c "time ./threecolor data/li/balanced_tree_4_7.txt" 2> /tmp/time.holder
echo balanced_tree_4_7.txt `egrep -v "user|sys|^$" /tmp/time.holder` >> timer.log
bash -c "time ./threecolor data/li/complete4.txt" 2> /tmp/time.holder
echo complete4.txt `egrep -v "user|sys|^$" /tmp/time.holder` >> timer.log
bash -c "time ./threecolor data/li/gnp_random_29_0.2.txt" 2> /tmp/time.holder
echo gnp_random_29_0.2.txt `egrep -v "user|sys|^$" /tmp/time.holder` >> timer.log
bash -c "time ./threecolor data/li/barabasi_albert_31_4.txt" 2> /tmp/time.holder
echo barabasi_albert_31_4.txt `egrep -v "user|sys|^$" /tmp/time.holder` >> timer.log
bash -c "time ./threecolor data/li/complete5.txt" 2> /tmp/time.holder
echo complete5.txt `egrep -v "user|sys|^$" /tmp/time.holder` >> timer.log
bash -c "time ./threecolor data/li/gnp_random_43_0.8.txt" 2> /tmp/time.holder
echo gnp_random_43_0.8.txt `egrep -v "user|sys|^$" /tmp/time.holder` >> timer.log
bash -c "time ./threecolor data/li/barabasi_albert_47_5.txt" 2> /tmp/time.holder
echo barabasi_albert_47_5.txt `egrep -v "user|sys|^$" /tmp/time.holder` >> timer.log
bash -c "time ./threecolor data/li/complete6.txt" 2> /tmp/time.holder
echo complete6.txt `egrep -v "user|sys|^$" /tmp/time.holder` >> timer.log
bash -c "time ./threecolor data/li/ladder_12.txt" 2> /tmp/time.holder
echo ladder_12.txt `egrep -v "user|sys|^$" /tmp/time.holder` >> timer.log
bash -c "time ./threecolor data/li/barabasi_albert_8_2.txt" 2> /tmp/time.holder
echo barabasi_albert_8_2.txt `egrep -v "user|sys|^$" /tmp/time.holder` >> timer.log
bash -c "time ./threecolor data/li/complete64.txt" 2> /tmp/time.holder
echo complete64.txt `egrep -v "user|sys|^$" /tmp/time.holder` >> timer.log
bash -c "time ./threecolor data/li/ladder_6.txt" 2> /tmp/time.holder
echo ladder_6.txt `egrep -v "user|sys|^$" /tmp/time.holder` >> timer.log
bash -c "time ./threecolor data/li/barbell_4_2.txt" 2> /tmp/time.holder
echo barbell_4_2.txt `egrep -v "user|sys|^$" /tmp/time.holder` >> timer.log
bash -c "time ./threecolor data/li/complete7.txt" 2> /tmp/time.holder
echo complete7.txt `egrep -v "user|sys|^$" /tmp/time.holder` >> timer.log
bash -c "time ./threecolor data/li/lollipop_15_7.txt" 2> /tmp/time.holder
echo lollipop_15_7.txt `egrep -v "user|sys|^$" /tmp/time.holder` >> timer.log
bash -c "time ./threecolor data/li/barbell_8_5.txt" 2> /tmp/time.holder
echo barbell_8_5.txt `egrep -v "user|sys|^$" /tmp/time.holder` >> timer.log
bash -c "time ./threecolor data/li/complete8.txt" 2> /tmp/time.holder
echo complete8.txt `egrep -v "user|sys|^$" /tmp/time.holder` >> timer.log
bash -c "time ./threecolor data/li/lollipop_8_4.txt" 2> /tmp/time.holder
echo lollipop_8_4.txt `egrep -v "user|sys|^$" /tmp/time.holder` >> timer.log
bash -c "time ./threecolor data/li/binomial_127_0.37.txt" 2> /tmp/time.holder
echo binomial_127_0.37.txt `egrep -v "user|sys|^$" /tmp/time.holder` >> timer.log
bash -c "time ./threecolor data/li/complete9.txt" 2> /tmp/time.holder
echo complete9.txt `egrep -v "user|sys|^$" /tmp/time.holder` >> timer.log
bash -c "time ./threecolor data/li/powerlaw_cluster_15_4_0.6.txt" 2> /tmp/time.holder
echo powerlaw_cluster_15_4_0.6.txt `egrep -v "user|sys|^$" /tmp/time.holder` >> timer.log
bash -c "time ./threecolor data/li/binomial_31_0.77.txt" 2> /tmp/time.holder
echo binomial_31_0.77.txt `egrep -v "user|sys|^$" /tmp/time.holder` >> timer.log
bash -c "time ./threecolor data/li/cycle_graph_14.txt" 2> /tmp/time.holder
echo cycle_graph_14.txt `egrep -v "user|sys|^$" /tmp/time.holder` >> timer.log
bash -c "time ./threecolor data/li/powerlaw_cluster_21_5_0.7.txt" 2> /tmp/time.holder
echo powerlaw_cluster_21_5_0.7.txt `egrep -v "user|sys|^$" /tmp/time.holder` >> timer.log
bash -c "time ./threecolor data/li/circular_ladder_12.txt" 2> /tmp/time.holder
echo circular_ladder_12.txt `egrep -v "user|sys|^$" /tmp/time.holder` >> timer.log
bash -c "time ./threecolor data/li/cycle_graph_8.txt" 2> /tmp/time.holder
echo cycle_graph_8.txt `egrep -v "user|sys|^$" /tmp/time.holder` >> timer.log
bash -c "time ./threecolor data/li/powerlaw_cluster_26_3_0.3.txt" 2> /tmp/time.holder
echo powerlaw_cluster_26_3_0.3.txt `egrep -v "user|sys|^$" /tmp/time.holder` >> timer.log
bash -c "time ./threecolor data/li/circular_ladder_7.txt" 2> /tmp/time.holder
echo circular_ladder_7.txt `egrep -v "user|sys|^$" /tmp/time.holder` >> timer.log
bash -c "time ./threecolor data/li/dorogovtsev_goltsev_mendes_8.txt" 2> /tmp/time.holder
echo dorogovtsev_goltsev_mendes_8.txt `egrep -v "user|sys|^$" /tmp/time.holder` >> timer.log
bash -c "time ./threecolor data/li/powerlaw_cluster_99_8_0.39.txt" 2> /tmp/time.holder
echo powerlaw_cluster_99_8_0.39.txt `egrep -v "user|sys|^$" /tmp/time.holder` >> timer.log
bash -c "time ./threecolor data/li/complete1.txt" 2> /tmp/time.holder
echo complete1.txt `egrep -v "user|sys|^$" /tmp/time.holder` >> timer.log
bash -c "time ./threecolor data/li/gnm_random_14_16.txt" 2> /tmp/time.holder
echo gnm_random_14_16.txt `egrep -v "user|sys|^$" /tmp/time.holder` >> timer.log
bash -c "time ./threecolor data/li/star_15.txt" 2> /tmp/time.holder
echo star_15.txt `egrep -v "user|sys|^$" /tmp/time.holder` >> timer.log
bash -c "time ./threecolor data/li/complete128.txt" 2> /tmp/time.holder
echo complete128.txt `egrep -v "user|sys|^$" /tmp/time.holder` >> timer.log
bash -c "time ./threecolor data/li/gnm_random_26_103.txt" 2> /tmp/time.holder
echo gnm_random_26_103.txt `egrep -v "user|sys|^$" /tmp/time.holder` >> timer.log
bash -c "time ./threecolor data/li/star_33.txt" 2> /tmp/time.holder
echo star_33.txt `egrep -v "user|sys|^$" /tmp/time.holder` >> timer.log
bash -c "time ./threecolor data/li/complete16.txt" 2> /tmp/time.holder
echo complete16.txt `egrep -v "user|sys|^$" /tmp/time.holder` >> timer.log
bash -c "time ./threecolor data/li/gnm_random_32_256.txt" 2> /tmp/time.holder
echo gnm_random_32_256.txt `egrep -v "user|sys|^$" /tmp/time.holder` >> timer.log
bash -c "time ./threecolor data/li/star_65.txt" 2> /tmp/time.holder
echo star_65.txt `egrep -v "user|sys|^$" /tmp/time.holder` >> timer.log
bash -c "time ./threecolor data/li/complete2.txt" 2> /tmp/time.holder
echo complete2.txt `egrep -v "user|sys|^$" /tmp/time.holder` >> timer.log
bash -c "time ./threecolor data/li/gnm_random_64_1024.txt" 2> /tmp/time.holder
echo gnm_random_64_1024.txt `egrep -v "user|sys|^$" /tmp/time.holder` >> timer.log
bash -c "time ./threecolor data/li/wheel_63.txt" 2> /tmp/time.holder
echo wheel_63.txt `egrep -v "user|sys|^$" /tmp/time.holder` >> timer.log
bash -c "time ./threecolor tests/3CNFSAT_small.txt" 2> /tmp/time.holder
echo 3CNFSAT_small.txt `egrep -v "user|sys|^$" /tmp/time.holder` >> timer.log
