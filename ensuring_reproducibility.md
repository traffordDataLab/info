### Ensuring reproducibility

#### 2011 Census

Querying [Nomis](https://www.nomisweb.co.uk/) for Census data and creating an API link like this:

```r
http://www.nomisweb.co.uk/api/v01/dataset/NM_162_1.data.csv?geography=1673527867...1673527876,1673527878,1673527877,1673527879...1673527899,1673527901,1673527900,1673527902...1673527950,1673527953,1673527951,1673527952,1673527954...1673527997,1673527999,1673527998,1673528000,1673528002,1673528003,1673528001,1673528004...1673528050,1673528052,1673528051,1673528053...1673528081&date=latestMINUS33-latest&gender=0&age=0&measure=1,2&measures=20100&select=date_name,geography_name,geography_code,gender_name,age_name,measure_name,measures_name,obs_value,obs_status_name
```

is easy but not *reproducible*. Others cannot easily determine which fields you selected in the data query page from the API call. **It's better to bulk download and filter your data in R.**

Here are the steps:

1. Use the 2011 Census [Table Finder](https://www.nomisweb.co.uk/census/2011/data_finder) to identify a dataset e.g. Religion
2. Follow the link to the dataset's page e.g. [Religion [KS209EW]](https://www.nomisweb.co.uk/census/2011/ks209ew)
3. Now you need to use the web inspector to pull out:
	
	i. the name of the dataset (e.g. nm_616_1) and;
	
	ii. the type of geography (e.g. TYPE265). 

4. With this information we can concatenate an API call to use in R:

```r
df <- read_csv("https://www.nomisweb.co.uk/api/v01/dataset/nm_616_1.bulk.csv?time=latest&measures=20100&rural_urban=total&geography=TYPE295")
```
