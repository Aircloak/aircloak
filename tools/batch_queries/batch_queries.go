package main

import (
	"bufio"
	"bytes"
	"encoding/csv"
	"encoding/json"
	"fmt"
	"io/ioutil"
	"log"
	"net/http"
	"os"
	"regexp"
	"time"
)

type config struct {
	URL        string `json:"url"`
	DataSource string `json:"data_source"`
	APIToken   string `json:"api_token"`
}

type startQueryResult struct {
	Success bool   `json:"success"`
	QueryID string `json:"query_id"`
}

type queryResult struct {
	Query query `json:"query"`
}

type query struct {
	Completed bool     `json:"completed"`
	Columns   []string `json:"columns"`
	Rows      []row    `json:"rows"`
	Error     *string  `json:"error"`
}

type row struct {
	Row         []interface{} `json:"row"`
	Occurrences int           `json:"occurrences"`
}

var configuration = config{}
var resultsPath = fmt.Sprintf("results/%s", time.Now().Format("20060102150405"))

func main() {
	defer func() {
		r := recover()
		if r != nil && r != "stop" {
			fmt.Printf("Error: %s", r)
		}

		fmt.Print("\nPress Enter to finish")
		bufio.NewReader(os.Stdin).ReadString('\n')
	}()

	log.SetFlags(log.LstdFlags | log.Lshortfile)

	configuration = readConfig()

	err := os.MkdirAll(resultsPath, os.ModePerm)
	if err != nil {
		reportError(err)
	}

	queries := readQueries()
	for index, query := range queries {
		fmt.Printf("%d/%d\n\n%s\n", index+1, len(queries), query)
		queryResult := runQuerySync(query)
		storeResult(index, query, queryResult)
	}
}

func readConfig() config {
	raw, err := ioutil.ReadFile("config.json")
	if err != nil {
		reportError(err)
	}

	var decoded config
	err = json.Unmarshal(raw, &decoded)
	if err != nil {
		reportError(err)
	}

	return decoded
}

func readQueries() []string {
	buffer, err := ioutil.ReadFile("queries.sql")
	if err != nil {
		reportError(err)
	}

	return regexp.MustCompile(`(?m)^\s*$`).Split(string(buffer), -1)
}

func runQuerySync(query string) queryResult {
	startQueryResult := startQuery(configuration.DataSource, query)
	for {
		fmt.Print(".")
		time.Sleep(5 * time.Second)
		queryResult := getQueryResult(startQueryResult.QueryID)
		if queryResult.Query.Completed == true {
			fmt.Print("\n")
			return queryResult
		}
	}
}

func startQuery(dataSource string, statement string) startQueryResult {
	var result startQueryResult

	jsonRequest(
		"POST",
		"/api/queries",
		map[string](map[string]string){
			"query": {
				"statement":        statement,
				"data_source_name": dataSource,
			},
		},
		&result,
	)

	return result
}

func getQueryResult(queryID string) queryResult {
	var result queryResult

	jsonRequest(
		"GET",
		fmt.Sprintf("/api/queries/%s", queryID),
		nil,
		&result,
	)

	return result
}

func jsonRequest(method string, path string, payload interface{}, out interface{}) {
	buffer := new(bytes.Buffer)
	if payload != nil {
		err := json.NewEncoder(buffer).Encode(payload)
		if err != nil {
			reportError(err)
		}
	}

	fullPath := fmt.Sprintf("%s%s", configuration.URL, path)
	client := &http.Client{}
	req, err := http.NewRequest(method, fullPath, buffer)
	if err != nil {
		reportError(err)
	}

	req.Header.Add("auth-token", configuration.APIToken)
	req.Header.Add("Content-Type", "application/json")

	resp, err := client.Do(req)
	if err != nil {
		reportError(err)
	}

	defer resp.Body.Close()

	if resp.StatusCode == 401 {
		reportError(fmt.Sprintf("Unauthorized request %s %s. Check if API token is valid.", method, fullPath))
	}

	bodyBytes, err := ioutil.ReadAll(resp.Body)
	if err != nil {
		reportError(err)
	}

	err = json.Unmarshal(bodyBytes, out)
	if err != nil {
		reportError(string(bodyBytes))
	}
}

func storeResult(index int, query string, queryResult queryResult) {
	if queryResult.Query.Error == nil {
		storeSuccess(index, queryResult)
	} else {
		storeError(query, queryResult)
	}
}

func storeSuccess(index int, queryResult queryResult) {
	file, err := os.Create(fmt.Sprintf("%s/query_%d.csv", resultsPath, index+1))
	if err != nil {
		reportError(err)
	}
	defer file.Close()

	writer := csv.NewWriter(file)
	defer writer.Flush()

	err = writer.Write(queryResult.Query.Columns)
	if err != nil {
		reportError(err)
	}

	for _, row := range queryResult.Query.Rows {
		strings := make([]string, len(row.Row))
		for i, v := range row.Row {
			strings[i] = fmt.Sprintf("%v", v)
		}

		for i := 0; i < row.Occurrences; i++ {
			err = writer.Write(strings)
			if err != nil {
				reportError(err)
			}
		}
	}
}

func storeError(query string, queryResult queryResult) {
	file, err := os.OpenFile(
		fmt.Sprintf("%s/errors.txt", resultsPath),
		os.O_APPEND|os.O_WRONLY|os.O_CREATE,
		0644,
	)
	if err != nil {
		reportError(err)
	}
	defer file.Close()

	file.Write([]byte(fmt.Sprintf("----\nError runninq query\n%s\n%s\n----\n\n", query, *queryResult.Query.Error)))
}

func reportError(v ...interface{}) {
	log.Output(2, fmt.Sprint(v...))
	panic("stop")
}
