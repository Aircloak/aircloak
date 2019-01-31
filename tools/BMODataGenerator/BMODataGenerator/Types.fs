module BMODataGenerator.Types

type Msg =
    | Lines of string list
    | StopCallback of AsyncReplyChannel<unit>

type Row = {
    userId: int

    merchName: string
    
    (*
    MERCH_SIC text
        MERCH_SIC contains numbers stored as strings such as:
        ·         5964
        ·         0
       There There are no null values in that column.
    *)
    merchSic: string
    merchCity: string
    merchState: string
    
    // This should really be a double, but Drill fails at importing it.
    // Create a string and then cast it later in Drill...
    tranAmt: string
    // This really should be a datetime, but we'll generate random
    // datetime looking strings here, and then make Drill interpret
    // them as datetimes
    tranDt: string
    
    (*
    SPOUSE contains mostly spouse names, but there’s other values that seem nonsensical to me (like numbers?) in strings:
    ·         HELEN
    ·         MARTHA
    ·         CHRISTINE
    ·         ROBIN STRADINGE
    ·         601-12-1596
    ·         391-30-1913
    ·         HERS:SEE MM01
    ·         APP-N/COAPP-Y
    ·         JEAN J*SHAW
    No null values, but there are string values that are ‘NULL’, which I think make more sense as null values,
    but that’s how it was in the parquet file.
    *)
    spouse: string
    workPlace: string
    // ANNUAL_INCOME looks like a decimal encoded as a string, that can also be null.
    annualInc: string 
    (*
    CARD_NBR are string encoded credit card numbers.
    I will not be posting examples of this column here, but they are 16 digit numbers.
    CARD_NBR can also be null.       
    *)
    cardNbr: string 
    (*
    I think genesis_id is some sort of identifier? Looks like a bunch of numbers stored as strings.
    0: jdbc:drill:zk=local> select distinct genesis_id from genesis2_parquet limit 10;
    +-------------+
    | genesis_id  |
    +-------------+
    | 67108933    |
    | 67108960    |
    | 67109307    |
    | 67110026    |
    | 67110105    |
    | 67110223    |
    | 67110255    |
    | 67110286    |
    | 67110801    |
    | 67110853    |
    +-------------+ 
    *)
    genesisId: int
}