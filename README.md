# Tweet classifier in Haskell 

## Idea

Tweets on Twitter may contain aggressive content. Haggressive is a supervised k-Nearest Neighbor machine learning classifier that distinguishes aggressive and non-aggressive tweets. It uses a bag-of-unigram approach for extracting features. The empirical evaluation of the approach has yielded an average accuracy of 68.57 % of k = 10. See report_haggressive.pdf for the accompanying report.

## Usage

Run it with cabal and specify the directory for the dataset. The
actual main file is in `src-lib/Hag.hs`.

