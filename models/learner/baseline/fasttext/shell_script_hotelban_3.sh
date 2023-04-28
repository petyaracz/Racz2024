./fasttext supervised -input train_hotelban_3.txt -output model_hotelban_3 -autotune-validation valid_hotelban_3.txt
./fasttext predict-prob model_hotelban_3.bin test_hotelban_3.txt > pred_hotelban_3.txt
./fasttext dump model_hotelban_3.bin args > args_hotelban_3.txt
rm model_hotelban_3.bin
