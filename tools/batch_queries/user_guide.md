This tool can be used to execute multiple queries sequentially. The tool will take the queries from the local `queries.sql` file, and execute them one by one. The results of successful queries are stored in the csv form into the `result/YYYYMMDDhhmmss` folder. Query errors are stored in `errors.txt` in the same folder.

To use the tool, you first need to generate an API token. Visit the Air site, click on the gear icon, and then select API token. Then, enter a token description, and in select API in the Access dropdown. Finally, click the create button. The new token is displayed on the next page. Make sure to copy the token and store it somewhere, since you won't be able to see it anymore in the Air interface.

Next, you need to configure the tool. Create the `config.json` file in the same folder where this tool resides. The file should have the following shape:

```
{
  "url": "http://AIR_SITE",
  "data_source": "DATA_SOURCE_NAME",
  "api_token": "YOUR_API_TOKEN"
}
```

Finally, you need to create the file named `queries.sql` in the same folder as the tool. In this files you can specify the queries which must be executed. The queries should be separated by at least one empty line.

Here's an example:

```
select age
from demographics

select count(*)
from demographics

select age, count(*)
from demographics
group by age
```

At this point, you can start the tool.
