%% -------------------------------------------------------------------
%%
%% The main shell runner file for riak_shell
%%
%% Copyright (c) 2007-2016 Basho Technologies, Inc.  All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------
-module(help).

-export([
         help/3,
         help/1
        ]).

help(Mod, Example, List) ->
    Msg1 = io_lib:format("The following functions are available~n", []),
    Msg2 = print_exts(List),
    Msg3 = io_lib:format("~nYou can get more help by calling help with the~n" ++
                  "extension name and function name like 'help ~s ~s;'~n",
              [Mod, Example]),
    Msg4 = io_lib:format("~nFor SQL help type 'help SQL'", []),
    Msg1 ++ Msg2 ++ Msg3 ++ Msg4.

help(sql) ->
    "The following SQL help commands are supported:~n"
        "CREATE   - using CREATE TABLE statements~n"
        "DELETE   - deleting data with DELETE FROM~n"
        "DESCRIBE - examining table structures~n"
        "EXPLAIN  - understanding SELECT query execution paths~n"
        "INSERT   - inserting data with INSERT INTO statements~n"
        "SELECT   - querying data~n"
        "SHOW     - listing tables or showing table creation SQL~n"
        "~n"
        "SELECT can be used with ORDER BY, GROUP BY and LIMIT clauses. It supports arithmetic on column values and has a "
        "variety of aggregation functions: COUNT, SUM, MEAN, AVG, MAX, MIN, STDDEV, STDDEV_SAMP and STDDEV_POP~n"
        "~n"
        "To get more help type 'help SQL SELECT' (replacing SELECT "
        "with another statement as appropriate)";
help(create) ->
    "You can use the CREATE TABLE statement to create Time Series tables.~n"
        "An example of the format is shown below:~n"
        "~n"
        "(1)>CREATE TABLE mytable (~n"
        "      keyfield    VARCHAR   NOT NULL,~n"
        "      timefield   TIMESTAMP NOT NULL,~n"
        "      otherfield1 SINT64    NOT NULL,~n"
        "      otherfield2 DOUBLE    NOT NULL,~n"
        "      otherfield3 BOOLEAN,~n"
        "      PRIMARY KEY (~n"
        "        (keyfield, QUANTUM(timefield, 15, 'm')),~n"
        "         keyfield, timefield~n"
        "      )~n"
        ");~n"
        "~n"
        "The valid field types are:~n"
        "* VARCHAR~n"
        "* TIMESTAMP - times~n"
        "* SINT64    - integers~n"
        "* DOUBLE    - floats~n"
        "* BLOB      - binary objects~n"
        "* BOOLEAN   - true or false~n"
        "~n"
        "Fields can optionally be declared NOT NULL - (fields that are in the keys MUST "
        "be set to NOT NULL).~n"
        "~n"
        "The QUANTUM function (which is not required, but recommended) can be used to co-locate data "
        "in time slices on the ring. It takes 2 parameters:~n"
        "* a number~n"
        "* one of 's', 'm', 'h' or 'd' for second/minute/hour/day~n"
        "~n"
        "So in this example the data is quantized in 15 minute blocks~n"
        "~n"
        "IT IS POSSIBLE TO CREATE HOT-SPOTS AND MAKE YOUR CLUSTER PERFORM POORLY WITH BADLY FORMATTED TABLES. "
        "PLEASE READ THE ONLINE DOCUMENTATION THOROUGHLY.~n"
        "For more details please go to http://docs.basho.com/riak/ts~n";
help(delete) ->
    "You can use the DELETE FROM statement to delete a single Time Series record.~n"
        "An example of the format is shown below:~n"
        "~n"
        "(1)>DELETE FROM mytable WHERE keyfield = 'keyvalue' AND timefield = '2016-11-30 19:30:00';~n"
        "~n"
        "The WHERE clause must uniquely identify a key.~n"
        "~n"
        "For more details please go to http://docs.basho.com/riak/ts~n";
help(describe) ->
    "You can use the DESCRIBE statement to inspect a Time Series table."
        "An example of the format is shown below:~n"
        "~n"
        "(1)>DESCRIBE mytable;~n"
        "~n"
        "For more details please go to http://docs.basho.com/riak/ts~n";
help(explain) ->
    "You can use the EXPLAIN statement to see how a Time Series SELECT query will be executed."
        "This feature is EXPERIMENTAL and its output should not be relied upon."
        "Its output is subject to unsignaled change between releases."
        "~n"
        "(this example uses the table definition from 'help SQL CREATE' "
        "and the data from 'help SQL INSERT')~n"
        "~n"
        "An example of the format is shown below:~n"
        "~n"
        "(1)>EXPLAIN SELECT * FROM mytable WHERE keyfield = 'keyvalue' ~n"
        "      AND timefield > '2016-11-30 19:30:00' ~n"
        "      AND timefield > '2016-12-31 19:30:00';~n"
        "~n"
        "An invalid SELECT query will result in EXPLAIN returning an error~n"
        "~n"
        "For more details please go to http://docs.basho.com/riak/ts~n";
%% TODO ask @jgorlick about NULLs in strings and this help function
help(insert) ->
    "You can use the INSERT INTO statement to insert data into a Time Series table.~n"
        "There are two formats that the INSERT INTO statement can use.~n"
        "~n"
        "(this example uses the table definition from 'help SQL CREATE')~n"
        "~n"
        "An example of the first format is shown below:~n"
        "~n"
        "(1)>INSERT INTO mytable VALUES ('keyvalue', '2016-11-30 19:30:00', 123, 12.3, false);"
        "~n"
        "Using this format you have to provide values for all columns - including those that "
        "can contain nulls.~n"
        "~n"
        "An example of the second format is shown below:~n"
        "~n"
        "(2)>INSERT INTO mytable (keyfield, timefield, otherfield1, otherfield2) VALUES ('keyvalue', '2016-11-30 19:30:00', 123, 12.3);"
        "~n"
        "In both of these formats multiple rows of data can be specified~n"
        "~n"
        "(3)>INSERT INTO mytable VALUES ('keyvalue', '2016-11-30 19:30:00', 123, 12.3, false), ('newvalue', '2016-11-30 19:31:04' 456, 45.6, true);"
        "~n"
        "For more details please go to http://docs.basho.com/riak/ts~n";
help(select) ->
    "You can use the SELECT statement to query your Time Series data."
        "~n"
        "(this example uses the table definition from 'help SQL CREATE' "
        "and the data from 'help SQL INSERT')~n"
        "~n"
        "An example of the format is shown below:~n"
        "~n"
        "(1)>SELECT * FROM mytable where keyfield = 'keyvalue' and timefield > '2016-11-30 19:15:00' and timefield < '2016-11-30 19:45:00';~n"
        "~n"
        "You can specify individual field names, and apply functions or arithmetic to them:~n"
        "~n"
        "(2)>SELECT otherfield1 FROM mytable where keyfield = 'keyvalue' and timefield > '2016-11-30 19:15:00' and timefield < '2016-11-30 19:45:00';~n"
        "~n"
        "(3)>SELECT otherfield1/2 FROM mytable where keyfield = 'keyvalue' and timefield > '2016-11-30 19:15:00' and timefield < '2016-11-30 19:45:00';~n"
        "~n"
        "(4)>SELECT MEAN(otherfield1) FROM mytable where keyfield = 'keyvalue' and timefield > '2016-11-30 19:15:00' and timefield < '2016-11-30 19:45:00';~n"
        "~n"
        "The functions supported are:~n"
        "* COUNT~n"
        "* SUM~n"
        "* MEAN and AVG~n"
        "* MIN~n"
        "* MAX~n"
        "* STDEV and STDDEV_SAMP~n"
        "* STDDEVPOP~n"
        "~n"
        "You can also decorate SELECT statements with ORDER BY, GROUP BY, LIMIT and OFFSET~n"
        "~n"
        "For more details please go to http://docs.basho.com/riak/ts~n";
help(show) ->
    "You can use SHOW TABLES to list all Time Series table you have created.~n"
        "~n"
        "This will give the description of the tables in a tabular format "
        "that is suitable for machine manipulation.~n"
        "~n"
        "An example of the format is shown below:~n"
        "~n"
        "(1)>SHOW TABLES;~n"
        "~n"
        "You can use SHOW CREATE TABLE to get the SQL statement that "
        "created the table.~n"
        "~n"
        "An example of the format is shown below:~n"
        "~n"
        "(2)>SHOW CREATE TABLE mytable;~n"
        "~n"
        "For more details please go to http://docs.basho.com/riak/ts~n";
help(Other) ->
    Msg = io_lib:format("No help available for '~p'~n~n", [Other]),
    Msg ++ help(sql).
        
%%
%% Internal Fns
%%

print_exts(E) ->
    Grouped = group(E, []),
    lists:flatten([begin
                       io_lib:format("~nExtension '~s':~n", [Mod]) ++
                           riak_shell_util:print_help(Fns)
                   end || {Mod, Fns} <- Grouped]).

group([], Acc) ->
    [{Mod, lists:sort(L)} || {Mod, L} <- lists:sort(Acc)];
group([{Fn, Mod} | T], Acc) ->
    Mod2 = remove_EXT_from_module_name(Mod),
    NewAcc = case lists:keyfind(Mod2, 1, Acc) of
                 false ->
                     [{Mod2, [Fn]} | Acc];
                 {Mod2, A2} ->
                     lists:keyreplace(Mod2, 1, Acc, {Mod2, [Fn | A2]})
             end,
    group(T, NewAcc).

remove_EXT_from_module_name(Atom) ->
    re:replace(atom_to_list(Atom), "_EXT", "", [{return, list}]).
