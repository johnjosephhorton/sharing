# Notes on code and data for "Owning, Using and Renting: Some Simple Economics of the Sharing Economy"

## Replication

To replicate the empirical portion of the paper:

First, download the repository from github:

```
 git clone git@github.com:johnjosephhorton/sharing.git 
```

Email `john.joseph.horton@gmail.com` and I will email you the two files you need.
Put those files in the `sharing/` folder.
The two files are: 
```
data_passphrase.txt
data_url.txt
```
One contains the download URL for the data, while the other contains the passphrase to decrypt the file locally.
Once the files are in place, from the `/sharing` directory run: 

```
cd writeup
make sharing.pdf
```
which will download the necessary data files, decrypt, run the statistical analysis in R (downloading all needed packages) and then produce plots and tables.
Finally, it will build the pdf file using `pdflatex`. 