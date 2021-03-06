# cluster-people

K-means clustering algorithm to group people who live close to each other.

The data are all people that will be clustered. The weights are the distance from each person's home to the destination. Moreover, the distance used is Haversine distance because the lat-lon coordinate system is used.

## Table of Contents
- [Install](#install)
- [Usage](#usage)
- [Technologies](#technologies)
- [Contributing](#contributing)
- [License](#license)

## Install

Prerequisites:

Download and install [R](https://cran.r-project.org/doc/manuals/r-release/R-admin.html).

- Clone the repository:
```bash
git clone https://github.com/erickmp07/cluster-people.git
```

## Usage

To run the scripts:

```bash
cd cluster-people/codes
```

Then, start the R interactive terminal:
```bash
R
source("haversine_dist.R")
source("SSE.R")
source("weighted_kmeans.R")
source("print_result.R")
```

The [``print_result.R``](codes/print_result.R) script will read the CSV [file](data/people_data.csv) and print the result generated by the K-means clustering algorithm.

To change the number of clusters, change the value of the K in the [``print_result.R``](codes/print_result.R).

To change the input data, change the CSV [file](data/people_data.csv).

NOTE: The CSV file should have the columns: name, longitude, latitude and distance.

## Technologies

This project was developed with the following technologies:

- [R](https://www.r-project.org/)

## Contributing

PRs and stars are always welcome.

To ask a question, please [contact me](mailto:erimacedo_92@hotmail.com).

## License

Licensed under [MIT](LICENSE) license.