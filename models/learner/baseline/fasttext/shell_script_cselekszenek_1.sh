./fasttext supervised -input train_cselekszenek_1.txt -output model_cselekszenek_1 -autotune-validation valid_cselekszenek_1.txt
./fasttext predict-prob model_cselekszenek_1.bin test_cselekszenek_1.txt > pred_cselekszenek_1.txt
./fasttext dump model_cselekszenek_1.bin args > args_cselekszenek_1.txt
rm model_cselekszenek_1.bin
