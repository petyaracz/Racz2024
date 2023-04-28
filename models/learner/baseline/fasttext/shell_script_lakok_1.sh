./fasttext supervised -input train_lakok_1.txt -output model_lakok_1 -autotune-validation valid_lakok_1.txt
./fasttext predict-prob model_lakok_1.bin test_lakok_1.txt > pred_lakok_1.txt
./fasttext dump model_lakok_1.bin args > args_lakok_1.txt
rm model_lakok_1.bin
