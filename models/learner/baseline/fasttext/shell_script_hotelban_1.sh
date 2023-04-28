./fasttext supervised -input train_hotelban_1.txt -output model_hotelban_1 -autotune-validation valid_hotelban_1.txt
./fasttext predict-prob model_hotelban_1.bin test_hotelban_1.txt > pred_hotelban_1.txt
./fasttext dump model_hotelban_1.bin args > args_hotelban_1.txt
rm model_hotelban_1.bin
