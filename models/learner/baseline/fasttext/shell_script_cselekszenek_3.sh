./fasttext supervised -input train_cselekszenek_3.txt -output model_cselekszenek_3 -autotune-validation valid_cselekszenek_3.txt
./fasttext predict-prob model_cselekszenek_3.bin test_cselekszenek_3.txt > pred_cselekszenek_3.txt
./fasttext dump model_cselekszenek_3.bin args > args_cselekszenek_3.txt
rm model_cselekszenek_3.bin
