# Notes

This is the associated code for my paper  ``Owning, Using and Renting: Some Simple Economics of the "Sharing" Economy``.
The paper is available at:

1. My website: [http://www.john-joseph-horton.com/papers/sharing.pdf](http://www.john-joseph-horton.com/papers/sharing.pdf)
1. SSRN: [http://papers.ssrn.com/sol3/papers.cfm?abstract_id=2730850](http://papers.ssrn.com/sol3/papers.cfm?abstract_id=2730850)
1. NBER: [http://www.nber.org/papers/w22029](http://www.nber.org/papers/w22029)

## Citation Info

```
@techreport{NBERw22029,
 title = "Owning, Using and Renting:  Some Simple Economics of the "Sharing Economy"",
 author = "John J. Horton and Richard J. Zeckhauser",
 institution = "National Bureau of Economic Research",
 type = "Working Paper",
 series = "Working Paper Series",
 number = "22029",
 year = "2016",
 month = "February",
 doi = {10.3386/w22029},
 URL = "http://www.nber.org/papers/w22029",
 abstract = {New Internet-based markets enable consumer/owners to rent out their durable goods when not using them. Such markets are modeled to determine ownership, rental rates, quantities, and surplus generated. Both the short run, before consumers can revise their ownership decisions, and the long run, in which they can, are examined to assess how these markets change ownership and consumption. The analysis examines bringing-to-market costs, such as labor costs and transaction costs, and considers the operating platform’s pricing problem. A survey of consumers broadly supports the modeling assumptions employed. For example, ownership is determined by individuals’ forward-looking assessments of planned usage.},
}	  

```

## Replication

The repository is set up to make it transparent how the final PDF is constructed from the raw data. 
To replicate, you will need a Linux or Mac OX machine that has the following installed:

1. `R`
1. `pdflatex`
1. `make`
1. `gpg`
1. `curl`

To replicate the data analysis, you will need several R packages.
However, when you run the code below, it *should* obtain all these R-specific dependencies you need. 

Note that this repository does not contain the actual experimental data.
To obtain the data, email me at `john.joseph.horton@gmail.com` and I will email you two small text files.
These files have the information you need to download and unencrypt the experimental data. 

One you have the two files, the steps are:

####Download the repository from github
```
 git clone git@github.com:johnjosephhorton/sharing.git 
```
#### Add the text files to the repository
Move the two files I sent you into the `/sharing` directory.
For example, if you download them to you downloads folder, you might run
```
cp ~/Downloads/*txt ~/sharing
```
The two files are: 
```
data_passphrase.txt
data_url.txt
```
#### Build the PDF
From `/sharing`, run: 
```
cd writeup
make sharing.pdf
```
This should download the necessary data files and decrypt them.
It will also run the statistical analysis in R (downloading all needed packages) and then produce plots and tables (stored in `writeup/tables` and `writeup/plots`). 
Finally, it will build the pdf file using `pdflatex`, leaving the resultant `sharing.pdf` in the `/writing` folder.
To see the actual steps that are being followed, you can inspect `writeup\Makefile`.

If you run into any trouble replicating, please contact me at ``john.joseph.horton@gmail.com``. 
